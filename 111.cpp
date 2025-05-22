#include <iostream>
#include <fstream>
#include <cctype>
#include <unordered_map>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>

#define TEMP_REG1 "$t8"
#define TEMP_REG2 "$t9"
#define SPILL_SLOT_SIZE 4

using namespace std;

unordered_map<string, string> keys = {{"const", "CONSTTK"}, {"int", "INTTK"}, {"break", "BREAKTK"}, {"continue", "CONTINUETK"}, {"if", "IFTK"}, {"else", "ELSETK"}, {"while", "WHILETK"}, {"return", "RETURNTK"}, {"void", "VOIDTK"}, {"main", "MAINTK"}, {"getint", "GETINTTK"}, {"printf", "PRINTFTK"}};
unordered_map<string, string> symbols = {{"!=", "NEQ"}, {"!", "NOT"}, {"&&", "AND"}, {"||", "OR"}, {"+", "PLUS"}, {"-", "MINU"}, {"*", "MULT"}, {"/", "DIV"}, {"%", "MOD"}, {"<", "LSS"}, {"<=", "LEQ"}, {">", "GRE"}, {">=", "GEQ"}, {"==", "EQL"}, {"=", "ASSIGN"}, {";", "SEMICN"}, {",", "COMMA"}, {"(", "LPARENT"}, {")", "RPARENT"}, {"[", "LBRACK"}, {"]", "RBRACK"}, {"{", "LBRACE"}, {"}", "RBRACE"}};

struct Token
{
    string type;
    string value;
};

struct midcode
{
    string op;
    string arg1;
    string arg2;
    string result;
};

struct Symbol
{
    string name;
    string type;
    int addr;
    bool isGlobal = true;
    bool isConst = false;
    bool isFunction = false;
    int arraySize = 1;
    string value = "";
    vector<string> constValuesList;
    Symbol(const string &n, const string &t, int a, bool c, bool b) : name(n), type(t), addr(a), isConst(c), isFunction(b) {}
    Symbol()
    {
        name = "";
        type = "";
        addr = 0;
        arraySize = 1;
        isGlobal = true;
        isConst = false;
        isFunction = false;
    }
};

int labelCount = 0;
size_t pos = 0;
string label_current = "";
vector<Token> tokens;                               // 词法分析结果
vector<string> label_start;                         // 开始标签
vector<string> label_end;                           // 结束标签
vector<midcode> midcodes;                           // 中间代码
vector<unordered_map<string, Symbol>> symbol_table; // 符号表
vector<string> stringLiterals;                      // 存储字符串常量
int tempAddr = 0;                                   // 记录当前临时变量地址
int strCount = 0;                                   // 记录字符串常量个数
int slots_offset = 0;                               // 记录溢出槽位偏移量
int slots_max = 0;                                  // 记录最大溢出槽位数

// 声明函数 (不声明会有未定义冲突)
void match(string expectedType);
void CompUnit();
void VarDecl();
void VarDef();
void MainFuncDef();
void Block();
void Stmt();
string Exp();
string AddExp();
string MulExp();
string UnaryExp();
string PrimaryExp();
string AddExpForConst();
string RelExp();
string EqExp();
string LAndExp();
string LOrExp();
string Cond();
string LVal(bool *outIsArrayAccessWithoutIndex = nullptr);
string FuncCall();
string FuncType();

// 符号表添加符号
void addSymbol(const string &name, const string &type, bool isConst, bool isFunction, int arraySize = 1)
{
    Symbol sym;
    sym.name = name;
    sym.type = type;
    sym.addr = tempAddr;
    sym.isConst = isConst;
    sym.isFunction = isFunction;
    sym.arraySize = arraySize;
    if (symbol_table.size() > 1)
    {
        sym.isGlobal = false;
    }
    else
    {
        sym.isGlobal = true;
    }
    symbol_table.back()[name] = sym;
    if (!sym.isGlobal && !isFunction)
    {
        tempAddr += arraySize * 4;
    }
}

// 符号表查找符号
bool get_const(const string &identName, int &outValue)
{
    for (auto it_s = symbol_table.rbegin(); it_s != symbol_table.rend(); ++it_s)
    {
        if (it_s->count(identName))
        {
            const Symbol &sym = it_s->at(identName);
            if (sym.isConst)
            {
                string valToParse;
                if (!sym.value.empty())
                {
                    valToParse = sym.value;
                }
                else if (!sym.constValuesList.empty())
                {
                    valToParse = sym.constValuesList[0];
                }

                try
                {
                    outValue = stoi(valToParse);
                    return true;
                }
                catch (const std::exception &e)
                {
                    return false;
                }
            }
            else
            {
                return false;
            }
        }
    }
    return false;
}

// 查找变量地址
string get_address(const string &varName)
{
    for (auto it = symbol_table.rbegin(); it != symbol_table.rend(); ++it)
    {
        const auto &scope = *it;
        if (scope.count(varName))
        {
            const Symbol &sym = scope.at(varName);
            if (sym.isGlobal)
            {
                return sym.name;
            }
            else
            {
                return to_string(sym.addr) + "($sp)";
            }
        }
    }
    return "";
}

// 中间代码生成
void code_emit(const string &op, const string &arg1_param, const string &arg2_param, const string &result_param)
{
    auto mapOperand = [](const string &name) -> string
    {
        if (name.empty())
            return "";
        if (name[0] == '$')
            return name;
        if (name.rfind("L_", 0) == 0)
            return name;
        if (name.rfind("FUNC_", 0) == 0)
            return name;
        if (name.find("($sp)") != string::npos)
        {
            return name;
        }
        if (isdigit(name[0]) || (name.size() > 1 && name[0] == '-' && isdigit(name[1])))
            return name;
        if (name.rfind("str", 0) == 0)
        {
            bool isStrLabel = true;
            for (size_t i = 3; i < name.size(); ++i)
            {
                if (!isdigit(name[i]))
                {
                    isStrLabel = false;
                    break;
                }
            }
            if (isStrLabel)
                return name;
        }
        if (!symbol_table.empty() && symbol_table[0].count(name))
        {
            const Symbol &sym = symbol_table[0].at(name);
            if (sym.isGlobal && !sym.isFunction)
                return name;
        }
        if (name.length() >= 1 && name[0] == 't' && (name.length() == 1 || isdigit(name[1])))
        {
            bool all_digits_after_t = true;
            for (size_t k = 1; k < name.length(); ++k)
            {
                if (!isdigit(name[k]))
                {
                    all_digits_after_t = false;
                    break;
                }
            }
            if (all_digits_after_t)
                return "$" + name;
        }
        for (auto it_s = symbol_table.rbegin(); it_s != symbol_table.rend(); ++it_s)
        {
            if (it_s->count(name))
            {
                const Symbol &sym_s = it_s->at(name);
                if (!sym_s.isGlobal && !sym_s.isFunction)
                {
                    return to_string(sym_s.addr) + "($sp)";
                }
            }
        }
        return name;
    };

    string arg1_mapped = mapOperand(arg1_param);
    string arg2_mapped = mapOperand(arg2_param);
    string result_mapped = mapOperand(result_param);

    if (op == "GETINT")
    {
        midcodes.push_back({"GETINT", "", "", result_mapped});
    }
    else if (op == "LABEL")
    {
        midcodes.push_back({op, "", "", result_mapped});
    }
    else if (op == "NOP_PROLOGUE_PLACEHOLDER")
    {
        midcodes.push_back({op, "", "", ""});
    }
    else if (op == "PRINTF_STR")
    {
        midcodes.push_back({op, arg1_mapped, "", ""});
    }
    else if (op == "PRINTF_ARG")
    {
        midcodes.push_back({op, arg1_mapped, "", ""});
    }
    else
    {
        midcodes.push_back({op, arg1_mapped, arg2_mapped, result_mapped});
    }
}

// 查找临时寄存器
string getNextTempReg()
{
    static int tempCount = 0;
    string reg = "t" + to_string(tempCount % 8);
    tempCount++;
    return reg;
}

// 查找下一个Token
Token lookahead(int offset = 0)
{
    if (pos + offset < tokens.size())
        return tokens[pos + offset];
    else
        return Token{"EOF", ""};
}

// 作用域
void enterScope(bool isGlobal = false)
{
    symbol_table.emplace_back();
    if (!isGlobal)
        tempAddr = 0;
}

void leaveScope()
{
    if (!symbol_table.empty())
    {
        symbol_table.pop_back();
    }
}

// 计算当前作用域中需要的帧大小
int calculateFrameSize(const unordered_map<string, Symbol> &symbols_in_scope, int spill_bytes_needed)
{
    int maxLocalVarParamAddr = 0;
    for (const auto &pair_map_item : symbols_in_scope)
    {
        const Symbol &sym = pair_map_item.second;
        if (!sym.isGlobal && !sym.isFunction)
        {
            int endAddr = sym.addr + (sym.arraySize * 4);
            if (endAddr > maxLocalVarParamAddr)
            {
                maxLocalVarParamAddr = endAddr;
            }
        }
    }
    int totalVariableSpace = maxLocalVarParamAddr + spill_bytes_needed;
    int frameSize = (totalVariableSpace + 4 + 7) & ~7;
    return frameSize;
}

// ======================= 后续均为在语法分析的基础上进行中间代码生成 =======================

void match(string expected)
{
    if (pos < tokens.size() && tokens[pos].type == expected)
    {
        pos++;
    }
}

string RelExp()
{
    string left = AddExp();
    while (lookahead().type == "LSS" || lookahead().type == "GRE" ||
           lookahead().type == "LEQ" || lookahead().type == "GEQ")
    {
        string op_type = lookahead().type;
        match(op_type);
        string right = AddExp();
        string temp_res = getNextTempReg();
        code_emit(op_type, left, right, temp_res);
        left = temp_res;
    }
    return left;
}

string EqExp()
{
    string left = RelExp();
    while (lookahead().type == "EQL" || lookahead().type == "NEQ")
    {
        string op_type = lookahead().type;
        match(op_type);
        string right = RelExp();
        string temp_res = getNextTempReg();
        code_emit(op_type, left, right, temp_res);
        left = temp_res;
    }
    return left;
}

string LAndExp()
{
    string left_operand_result = EqExp(); // Evaluate the first operand.
    // If there are no AND operations, this is the final result.
    // If there are AND operations, we need a new register to build the chain.
    string overall_result_reg = left_operand_result; // Default to LHS if no ANDs

    if (lookahead().type == "AND") { // Only allocate new reg if there's an AND
        overall_result_reg = getNextTempReg();
        code_emit("MOVE", left_operand_result, "", overall_result_reg);
    }

    while (lookahead().type == "AND")
    {
        match("AND");
        string skip_rhs_eval_label = "L_skip_eval_rhs_and_" + to_string(labelCount++);
        string end_this_and_op_label = "L_end_this_and_op_" + to_string(labelCount++);

        // If current overall_result_reg is 0 (false), the AND expression yields false.
        // Skip evaluation of the RHS (EqExp) and keep overall_result_reg as 0.
        code_emit("BEQZ", overall_result_reg, "", skip_rhs_eval_label);

        // If we are here, overall_result_reg was 1 (true). Evaluate RHS.
        string right_operand_result = EqExp();
        code_emit("MOVE", right_operand_result, "", overall_result_reg); // Result is now determined by RHS.
        code_emit("J", "", "", end_this_and_op_label);

        code_emit("LABEL", "", "", skip_rhs_eval_label);
        // If we jumped here, overall_result_reg was already 0 (from a previous evaluation or the first BEQZ)
        // and it should remain 0. If it wasn't explicitly set to 0 by a previous step (e.g. if overall_result_reg
        // was just a value from EqExp that happened to be 0 but not yet a boolean result register),
        // ensure it's 0. However, given the BEQZ, it must have been 0.
        // To be absolutely safe, if overall_result_reg wasn't guaranteed to be 0/1 before this,
        // an explicit LI "0" might be needed. But current structure should maintain 0.
        // If overall_result_reg came from EqExp and was 0, it's still 0.
        // If it was already 0 from a prior AND op, it's still 0.
        // So, no specific code_emit to set it to 0 should be needed here if BEQZ ensures it's 0.
        // For clarity and robustness, let's ensure it is set to 0 if skipped.
        code_emit("LI", "0", "", overall_result_reg);


        code_emit("LABEL", "", "", end_this_and_op_label);
    }
    return overall_result_reg;
}

string LOrExp()
{
    string left_operand_result = LAndExp(); // Evaluate the first operand.
    string overall_result_reg = left_operand_result; // Default to LHS if no ORs

    if (lookahead().type == "OR") { // Only allocate new reg if there's an OR
        overall_result_reg = getNextTempReg();
        code_emit("MOVE", left_operand_result, "", overall_result_reg);
    }

    while (lookahead().type == "OR")
    {
        match("OR");
        string skip_rhs_eval_label = "L_skip_eval_rhs_or_" + to_string(labelCount++);
        string end_this_or_op_label = "L_end_this_or_op_" + to_string(labelCount++);

        // If current overall_result_reg is 1 (true), the OR expression yields true.
        // Skip evaluation of the RHS (LAndExp) and keep overall_result_reg as 1.
        code_emit("BNEZ", overall_result_reg, "", skip_rhs_eval_label);

        // If we are here, overall_result_reg was 0 (false). Evaluate RHS.
        string right_operand_result = LAndExp();
        code_emit("MOVE", right_operand_result, "", overall_result_reg); // Result is now determined by RHS.
        code_emit("J", "", "", end_this_or_op_label);

        code_emit("LABEL", "", "", skip_rhs_eval_label);
        // If we jumped here, overall_result_reg was already 1 (true).
        // Ensure it's 1.
        code_emit("LI", "1", "", overall_result_reg);

        code_emit("LABEL", "", "", end_this_or_op_label);
    }
    return overall_result_reg;
}

string Cond()
{
    string condReg = LOrExp();
    return condReg;
}

vector<pair<string, bool>> parseFormatString(const string &fmt)
{
    vector<pair<string, bool>> parts;
    string processedPart;
    for (size_t i = 0; i < fmt.size(); ++i)
    {
        if (fmt[i] == '\\' && i + 1 < fmt.size())
        {
            switch (fmt[i + 1])
            {
            case 'n':
                processedPart += '\n';
                break;
            case 't':
                processedPart += '\t';
                break;
            case '\\':
                processedPart += '\\';
                break;
            default:
                processedPart += fmt[i + 1];
            }
            i++;
        }
        else if (fmt[i] == '%' && i + 1 < fmt.size() && fmt[i + 1] == 'd')
        {
            if (!processedPart.empty())
            {
                parts.emplace_back(processedPart, false);
                processedPart.clear();
            }
            parts.emplace_back("%d", true);
            i++;
        }
        else
        {
            processedPart += fmt[i];
        }
    }
    if (!processedPart.empty())
    {
        parts.emplace_back(processedPart, false);
    }
    return parts;
}

void Stmt()
{
    if (lookahead().type == "LBRACE")
    {
        Block();
    }
    else if (lookahead().type == "IFTK")
    {
        match("IFTK");
        match("LPARENT");
        string condReg = Cond();
        match("RPARENT");
        string falsePathLabel = "L_if_false_path_" + to_string(labelCount++);
        code_emit("BEQZ", condReg, "", falsePathLabel);
        Stmt();
        if (lookahead().type == "ELSETK")
        {
            string endOfIfElseLabel = "L_endifelse_" + to_string(labelCount++);
            code_emit("J", "", "", endOfIfElseLabel);
            code_emit("LABEL", "", "", falsePathLabel);
            match("ELSETK");
            Stmt();
            code_emit("LABEL", "", "", endOfIfElseLabel);
        }
        else
        {
            code_emit("LABEL", "", "", falsePathLabel);
        }
    }
    else if (lookahead().type == "WHILETK")
    {
        match("WHILETK");
        string startLabel = "L_while_" + to_string(labelCount++);
        string endLabel = "L_endwhile_" + to_string(labelCount++);

        label_start.push_back(startLabel);
        label_end.push_back(endLabel);

        code_emit("LABEL", "", "", startLabel);
        match("LPARENT");
        string condReg = Cond();
        match("RPARENT");
        code_emit("BEQZ", condReg, "", endLabel);

        Stmt();

        code_emit("J", "", "", startLabel);
        code_emit("LABEL", "", "", endLabel);

        label_start.pop_back();
        label_end.pop_back();
    }
    else if (lookahead().type == "BREAKTK")
    {
        match("BREAKTK");
        code_emit("J", "", "", label_end.back());
        match("SEMICN");
    }
    else if (lookahead().type == "CONTINUETK")
    {
        match("CONTINUETK");
        code_emit("J", "", "", label_start.back());
        match("SEMICN");
    }
    else if (lookahead().type == "RETURNTK")
    {
        match("RETURNTK");
        if (lookahead().type != "SEMICN")
        {
            string retReg = Exp();
            code_emit("MOVE", retReg, "", "$v0");
        }
        if (!label_current.empty())
        {
            code_emit("J", "", "", label_current);
        }
        match("SEMICN");
    }
    else if (lookahead().type == "PRINTFTK")
    {
        match("PRINTFTK");
        match("LPARENT");
        string rawFormatStr = lookahead().value;
        if (rawFormatStr.length() >= 2 && rawFormatStr.front() == '"' && rawFormatStr.back() == '"')
        {
            rawFormatStr = rawFormatStr.substr(1, rawFormatStr.length() - 2);
        }
        match("STRCON");

        vector<string> arg_spill_addrs;
        int saved_spill_offset_for_printf = slots_offset;

        auto calculate_printf_arg_spill_address = [&]() -> string
        {
            int current_func_max_declared_vars_size = 0;
            if (!symbol_table.empty() && !symbol_table.back().empty())
            {
                for (const auto &pair_sym : symbol_table.back())
                {
                    const Symbol &sym = pair_sym.second;
                    if (!sym.isGlobal && !sym.isFunction)
                    {
                        current_func_max_declared_vars_size = max(current_func_max_declared_vars_size, sym.addr + sym.arraySize * 4);
                    }
                }
            }
            int total_offset = current_func_max_declared_vars_size + slots_offset;
            string spill_addr = to_string(total_offset) + "($sp)";
            return spill_addr;
        };

        while (lookahead().type == "COMMA")
        {
            match("COMMA");
            string tempReg = Exp();
            string spill_loc = calculate_printf_arg_spill_address();
            code_emit("SW", tempReg, "", spill_loc);
            arg_spill_addrs.push_back(spill_loc);
            slots_offset += SPILL_SLOT_SIZE;
            slots_max = max(slots_max, slots_offset);
        }
        match("RPARENT");
        match("SEMICN");

        vector<pair<string, bool>> parts = parseFormatString(rawFormatStr);
        int argIdx = 0;
        for (const auto &part_pair : parts)
        {
            const string &part_content = part_pair.first;
            bool isArgPlaceholder = part_pair.second;

            if (isArgPlaceholder)
            {
                    string temp_for_print = getNextTempReg();
                    code_emit("LW", temp_for_print, "", arg_spill_addrs[argIdx]);
                    code_emit("PRINTF_ARG", temp_for_print, "", "");
                    argIdx++;
                }
            else
            {
                string label = "str" + to_string(strCount++);
                string mipsEscapedPart;
                for (char c : part_content)
                {
                    if (c == '\n')
                        mipsEscapedPart += "\\n";
                    else if (c == '\t')
                        mipsEscapedPart += "\\t";
                    else if (c == '"')
                        mipsEscapedPart += "\\\"";
                    else if (c == '\\')
                        mipsEscapedPart += "\\\\";
                    else
                        mipsEscapedPart += c;
                }
                stringLiterals.push_back("\"" + mipsEscapedPart + "\"");
                code_emit("PRINTF_STR", label, "", "");
            }
        }
        slots_offset = saved_spill_offset_for_printf;
    }
    else if (lookahead().type == "IDENFR" && (lookahead(1).type == "ASSIGN" || lookahead(1).type == "LBRACK"))
    {
        string lvalResult_addr_str_or_reg = LVal();
            string finalLvalTargetForStore = lvalResult_addr_str_or_reg;
            string lvalSpillLocForRegisterAddress;
            bool lvalAddressWasInRegisterAndSpilled = false;
            int stmt_level_spill_offset_backup = slots_offset;
        bool lvalIsRegister = (!lvalResult_addr_str_or_reg.empty() &&
                               lvalResult_addr_str_or_reg[0] == 't' &&
                               lvalResult_addr_str_or_reg.length() > 1 &&
                               isdigit(lvalResult_addr_str_or_reg[1]));

        if (lvalIsRegister)
        {
            int current_func_max_declared_vars_size = 0;
            if (!symbol_table.empty() && !symbol_table.back().empty())
            {
                for (const auto &pair_sym : symbol_table.back())
                {
                    const Symbol &sym = pair_sym.second;
                    if (!sym.isGlobal && !sym.isFunction)
                    {
                        current_func_max_declared_vars_size = max(current_func_max_declared_vars_size, sym.addr + sym.arraySize * 4);
                    }
                }
            }
            lvalSpillLocForRegisterAddress = to_string(current_func_max_declared_vars_size + slots_offset) + "($sp)";

                code_emit("SW", lvalResult_addr_str_or_reg, "", lvalSpillLocForRegisterAddress);
                lvalAddressWasInRegisterAndSpilled = true;
                slots_offset += SPILL_SLOT_SIZE;
                slots_max = max(slots_max, slots_offset);
            }

        match("ASSIGN");
            string rhsReg_val;
            if (lookahead().type == "GETINTTK")
            {
                match("GETINTTK");
                match("LPARENT");
                match("RPARENT");
                rhsReg_val = getNextTempReg();
                code_emit("GETINT", "", "", rhsReg_val);
            }
            else
            {
            rhsReg_val = Exp();
            }
        if (lvalAddressWasInRegisterAndSpilled)
        {
                string reloadedLvalAddressReg = getNextTempReg();
                code_emit("LW", reloadedLvalAddressReg, "", lvalSpillLocForRegisterAddress);
            finalLvalTargetForStore = reloadedLvalAddressReg;
            }
            code_emit("STORE", rhsReg_val, "", finalLvalTargetForStore);
        if (lvalAddressWasInRegisterAndSpilled)
        {
            slots_offset = stmt_level_spill_offset_backup;
        }
            match("SEMICN");
        }
    else
    {
        if (lookahead().type != "SEMICN")
        {
            Exp();
        }
        match("SEMICN");
    }
}

vector<string> ArgList()
{
    vector<string> arg_spill_locations;

    auto calculate_current_spill_address = [&]() -> string
    {
        int current_func_max_declared_vars_size = 0;
        if (!symbol_table.empty() && !symbol_table.back().empty())
        {
            for (const auto &pair_sym : symbol_table.back())
            {
                const Symbol &sym = pair_sym.second;
                if (!sym.isGlobal && !sym.isFunction)
                {
                    current_func_max_declared_vars_size = max(current_func_max_declared_vars_size, sym.addr + sym.arraySize * 4);
                }
            }
        }
        return to_string(current_func_max_declared_vars_size + slots_offset) + "($sp)";
    };
    string temp_reg_arg = Exp();
    string spill_loc = calculate_current_spill_address();
    code_emit("SW", temp_reg_arg, "", spill_loc);
    arg_spill_locations.push_back(spill_loc);
    slots_offset += SPILL_SLOT_SIZE;
    slots_max = max(slots_max, slots_offset);
    while (lookahead().type == "COMMA")
    {
        match("COMMA");
        temp_reg_arg = Exp();
        spill_loc = calculate_current_spill_address();
        code_emit("SW", temp_reg_arg, "", spill_loc);
        arg_spill_locations.push_back(spill_loc);
        slots_offset += SPILL_SLOT_SIZE;
        slots_max = max(slots_max, slots_offset);
    }
    return arg_spill_locations;
}

string FuncCall()
{
    string funcName = lookahead().value;
    match("IDENFR");
    match("LPARENT");

    int saved_caller_spill_offset = slots_offset;

    vector<string> arg_spill_addrs_on_caller_stack;
    if (lookahead().type != "RPARENT")
    {
        arg_spill_addrs_on_caller_stack = ArgList();
    }
    match("RPARENT");
    int num_total_args = arg_spill_addrs_on_caller_stack.size();

    for (int i = 0; i < min(4, num_total_args); ++i)
    {
        code_emit("LW", "$a" + to_string(i), "", arg_spill_addrs_on_caller_stack[i]);
    }

    for (int i = num_total_args - 1; i >= 4; --i)
    {
        string temp_for_push = getNextTempReg();
        code_emit("LW", temp_for_push, "", arg_spill_addrs_on_caller_stack[i]);
        code_emit("PUSH", temp_for_push, "", "");
    }

    string returnReg = "";
    bool is_void_func = false;
    if (!symbol_table.empty() && symbol_table[0].count(funcName))
    {
        if (symbol_table[0][funcName].type == "void")
        {
            is_void_func = true;
        }
    }
    if (!is_void_func)
    {
        returnReg = getNextTempReg();
    }

    code_emit("CALL", funcName, "", returnReg);

    int num_stack_args_passed = max(0, num_total_args - 4);
    if (num_stack_args_passed > 0)
    {
        code_emit("ADDIU", "$sp", to_string(num_stack_args_passed * 4), "$sp");
    }

    slots_offset = saved_caller_spill_offset;

    return returnReg;
}

void InitVal()
{
    if (lookahead().type == "LBRACE")
    {
        match("LBRACE");
        if (lookahead().type != "RBRACE")
        {
            InitVal();
            while (lookahead().type == "COMMA")
            {
                match("COMMA");
                InitVal();
            }
        }
        match("RBRACE");
    }
    else
    {
        Exp();
    }
}

string UnaryExpForConst()
{
    if (lookahead().type == "PLUS" || lookahead().type == "MINU" || lookahead().type == "NOT")
    {
        string op = lookahead().type;
        match(op);
        string valStr = UnaryExpForConst();
        try
        {
            int num = stoi(valStr);
            if (op == "MINU")
                return to_string(-num);
            else if (op == "NOT")
                return to_string(!num);
            else
                return valStr;
        }
        catch (const std::exception &e)
        {
            exit(1);
        }
    }
    else if (lookahead().type == "INTCON")
    {
        string num = lookahead().value;
        match("INTCON");
        return num;
    }
    else if (lookahead().type == "IDENFR")
    {
        string identName = lookahead().value;
        match("IDENFR");
        int constVal;
        if (get_const(identName, constVal))
        {
            return to_string(constVal);
        }
    }
    else if (lookahead().type == "LPARENT")
    {
        match("LPARENT");
        string val = AddExpForConst();
        match("RPARENT");
        return val;
    }
    return "";
}

string MulExpForConst()
{
    string left = UnaryExpForConst();
    while (lookahead().type == "MULT" || lookahead().type == "DIV" || lookahead().type == "MOD")
    {
        string op = lookahead().type;
        match(op);
        string right = UnaryExpForConst();
        try
        {
            int leftVal = stoi(left);
            int rightVal = stoi(right);
            if (op == "MULT")
                left = to_string(leftVal * rightVal);
            else if (op == "DIV")
            {
                left = to_string(leftVal / rightVal);
            }
            else
            {
                left = to_string(leftVal % rightVal);
            }
        }
        catch (const std::exception &e)
        {
            exit(1);
        }
    }
    return left;
}

string AddExpForConst()
{
    string left = MulExpForConst();
    while (lookahead().type == "PLUS" || lookahead().type == "MINU")
    {
        string op = lookahead().type;
        match(op);
        string right = MulExpForConst();
        try
        {
            int leftVal = stoi(left);
            int rightVal = stoi(right);
            left = (op == "PLUS") ? to_string(leftVal + rightVal) : to_string(leftVal - rightVal);
        }
        catch (const std::exception &e)
        {
            exit(1);
        }
    }
    return left;
}

string ConstExp()
{
    string numStr = AddExpForConst();
    return numStr;
}

vector<string> ConstInitVal()
{
    vector<string> values;
    if (lookahead().type == "LBRACE")
    {
        match("LBRACE");
        if (lookahead().type != "RBRACE")
        {
            values.push_back(ConstExp());

            while (lookahead().type == "COMMA")
            {
                match("COMMA");
                values.push_back(ConstExp());
            }
        }
        match("RBRACE");
    }
    else
    {
        values.push_back(ConstExp());
    }
    return values;
}

void VarDef()
{
    string varName = lookahead().value;
    match("IDENFR");
    int arraySize = 1;
    if (lookahead().type == "LBRACK")
    {
        match("LBRACK");
        string sizeStr = ConstExp();
        arraySize = stoi(sizeStr);
        match("RBRACK");
    }
    addSymbol(varName, "int", false, false, arraySize);
    Symbol &sym_ref = symbol_table.back()[varName];

    if (lookahead().type == "ASSIGN")
    {
        match("ASSIGN");
        if (sym_ref.isGlobal)
        {
            if (arraySize == 1)
            {
                sym_ref.value = ConstExp();
            }
            else
            {
                match("LBRACE");
                sym_ref.value = "{";
                int init_count = 0;
                if (lookahead().type != "RBRACE")
                {
                    string first_val = ConstExp();
                    sym_ref.value += first_val;
                    init_count++;
                    while (lookahead().type == "COMMA")
                    {
                        match("COMMA");
                        string next_val = ConstExp();
                        sym_ref.value += "," + next_val;
                        init_count++;
                    }
                }
                sym_ref.value += "}";
                match("RBRACE");
            }
        }
        else
        {
            if (arraySize > 1 && lookahead().type == "LBRACE")
            {
                match("LBRACE");
                int elements_initialized = 0;
                if (lookahead().type != "RBRACE")
                {
                    do
                    {
                        if (elements_initialized > 0 && lookahead().type == "COMMA")
                            match("COMMA");
                        else if (elements_initialized > 0)
                            break;

                        // 计算目标地址
                        string baseAddrArr_str = get_address(varName);
                        string idx_val_reg = getNextTempReg();
                        code_emit("LI", to_string(elements_initialized), "", idx_val_reg);

                        string offsetBytesReg = getNextTempReg();
                        string four_val_reg = getNextTempReg();
                        code_emit("LI", "4", "", four_val_reg);
                        code_emit("MUL", idx_val_reg, four_val_reg, offsetBytesReg);

                        string baseAddrReg_for_calc = getNextTempReg();
                        size_t sp_pos = baseAddrArr_str.find("($sp)");
                        string offset_from_sp_str = baseAddrArr_str.substr(0, sp_pos);
                        code_emit("ADDIU", "$sp", offset_from_sp_str, baseAddrReg_for_calc);

                        string finalElementAddrReg = getNextTempReg();
                        code_emit("ADD", baseAddrReg_for_calc, offsetBytesReg, finalElementAddrReg);

                        // 在地址计算完成后，再计算要存储的值
                        string element_val_reg = Exp();
                        code_emit("STORE", element_val_reg, "", finalElementAddrReg);
                        elements_initialized++;
                    } while (lookahead().type == "COMMA");
                }
                match("RBRACE");
            }
            else
            {
                string valReg;
                if (lookahead().type == "GETINTTK")
                {
                    match("GETINTTK");
                    match("LPARENT");
                    match("RPARENT");
                    valReg = getNextTempReg();
                    code_emit("GETINT", "", "", valReg);
                }
                else
                {
                    valReg = Exp();
                }
                string varAddr = get_address(varName);
                code_emit("STORE", valReg, "", varAddr);
            }
        }
    }
    else
    {
    }
}

void VarDecl()
{
    match("INTTK");
    VarDef();
    while (lookahead().type == "COMMA")
    {
        match("COMMA");
        VarDef();
    }
    match("SEMICN");
}

void ConstDef()
{
    string constName = lookahead().value;
    match("IDENFR");
    int declaredArraySize = -1;
    if (lookahead().type == "LBRACK")
    {
        match("LBRACK");
        string sizeStr = ConstExp();
        declaredArraySize = stoi(sizeStr);
        match("RBRACK");
    }

    match("ASSIGN");
    vector<string> initValues = ConstInitVal();

    int finalArraySize;
    if (declaredArraySize != -1)
    {
        finalArraySize = declaredArraySize;
        if (finalArraySize > 0)
        {
            while (initValues.size() < (size_t)finalArraySize)
            {
                initValues.push_back("0");
            }
        }
    }
    else
    {
        bool isEmptyBraceInitializer = false;
        if (initValues.empty() && pos > 0 && tokens[pos - 1].type == "RBRACE" &&
            pos > 1 && tokens[pos - 2].type == "LBRACE" &&
            pos > 2 && tokens[pos - 3].type == "ASSIGN")
        {
            isEmptyBraceInitializer = true;
        }
        if (initValues.empty() && isEmptyBraceInitializer)
        {
            finalArraySize = 0;
        }
        else
        {
            finalArraySize = initValues.size();
        }
    }
    addSymbol(constName, "int", true, false, finalArraySize);
    Symbol &sym = symbol_table.back()[constName];
    sym.constValuesList = initValues;
    if (finalArraySize == 1 && !initValues.empty())
    {
        sym.value = initValues[0];
    }
    if (!sym.isGlobal && sym.arraySize > 0)
    {
        string baseAddrArr_str = get_address(constName);
        for (size_t k = 0; k < sym.constValuesList.size(); ++k)
        {
            if (k >= (size_t)sym.arraySize)
                break;

            // string val_to_store_reg = getNextTempReg(); // Original line, now handled differently
            code_emit("LI", sym.constValuesList[k], "", TEMP_REG1); // Load const value directly into TEMP_REG1
            // code_emit("MOVE", val_to_store_reg, "", TEMP_REG1); // No longer needed as loaded directly

            string idx_val_reg = getNextTempReg();
            code_emit("LI", to_string(k), "", idx_val_reg);

            string offsetBytesReg = getNextTempReg();
            string four_val_reg = getNextTempReg();
            code_emit("LI", "4", "", four_val_reg);
            code_emit("MUL", idx_val_reg, four_val_reg, offsetBytesReg);

            string baseAddrReg_for_calc = getNextTempReg();
            size_t sp_pos = baseAddrArr_str.find("($sp)");
            string offset_from_sp_str = baseAddrArr_str.substr(0, sp_pos);
            code_emit("ADDIU", "$sp", offset_from_sp_str, baseAddrReg_for_calc);

            string finalElementAddrReg = getNextTempReg();
            code_emit("ADD", baseAddrReg_for_calc, offsetBytesReg, finalElementAddrReg);

            code_emit("STORE", TEMP_REG1, "", finalElementAddrReg); // Store from the safe temp reg
        }
    }
}

void ConstDecl()
{
    match("CONSTTK");
    match("INTTK");
    ConstDef();
    while (lookahead().type == "COMMA")
    {
        match("COMMA");
        ConstDef();
    }
    match("SEMICN");
}

void Decl()
{
    if (lookahead().type == "CONSTTK")
    {
        ConstDecl();
    }
    else if (lookahead().type == "INTTK")
    {
        VarDecl();
    }
}

void BlockItem()
{
    if (lookahead().type == "INTTK" || lookahead().type == "CONSTTK")
    {
        Decl();
    }
    else
    {
        Stmt();
    }
}

void Block()
{
    match("LBRACE");
    while (lookahead().type != "RBRACE" && lookahead().type != "EOF")
    {
        BlockItem();
    }
    match("RBRACE");
}

void MainFuncDef()
{
    match("INTTK");
    match("MAINTK");
    match("LPARENT");
    match("RPARENT");

    string func_name_for_label = "main";
    symbol_table[0][func_name_for_label] = Symbol(func_name_for_label, "int", 0, false, true);

    enterScope();
    slots_offset = 0;
    slots_max = 0;

    string oldEpilogueLabel = label_current;
    label_current = "L_epilogue_FUNC_main";

    code_emit("LABEL", "", "", "FUNC_main");
    size_t prologueInsertionPoint = midcodes.size();
    midcodes.push_back({"NOP_PROLOGUE_PLACEHOLDER", "", "", ""});

    Block();
    int frameSize = calculateFrameSize(symbol_table.back(), slots_max);
    vector<midcode> prologueQuads;
    prologueQuads.push_back({"ADDIU", "$sp", "-" + to_string(frameSize), "$sp"});
    prologueQuads.push_back({"SW", "$ra", "", to_string(frameSize - 4) + "($sp)"});
    if (prologueInsertionPoint < midcodes.size() && midcodes[prologueInsertionPoint].op == "NOP_PROLOGUE_PLACEHOLDER")
    {
        midcodes.erase(midcodes.begin() + prologueInsertionPoint);
        midcodes.insert(midcodes.begin() + prologueInsertionPoint,
                        prologueQuads.begin(), prologueQuads.end());
    }
    bool explicitReturnExists = false;
    if (!midcodes.empty())
    {
        for (long k_loop = midcodes.size() - 1; k_loop >= 0; --k_loop)
        {
            const auto &quad = midcodes[k_loop];
            if (quad.op == "J" && quad.result == label_current)
            {
                explicitReturnExists = true;
                break;
            }
            if (quad.op == "LABEL" && quad.result.rfind("FUNC_", 0) == 0 && quad.result != label_current)
            {
                break;
            }
        }
    }
    if (!explicitReturnExists)
    {
        code_emit("LI", "0", "", "$v0");
        code_emit("J", "", "", label_current);
    }
    code_emit("LABEL", "", "", label_current);
    code_emit("LW", "$ra", "", to_string(frameSize - 4) + "($sp)");
    code_emit("ADDIU", "$sp", to_string(frameSize), "$sp");
    code_emit("JR", "$ra", "", "");
    leaveScope();
    label_current = oldEpilogueLabel;
}

void FuncDef()
{
    string return_type_str = FuncType();
    string funcName = lookahead().value;
    match("IDENFR");
    if (!symbol_table.empty())
    {
        symbol_table[0][funcName] = Symbol(funcName, return_type_str, 0, false, true);
        symbol_table[0][funcName].type = return_type_str;
    }
    string oldEpilogueLabel = label_current;
    label_current = "L_epilogue_FUNC_" + funcName;
    match("LPARENT");
    slots_offset = 0;
    slots_max = 0;
    enterScope();
    vector<string> paramNamesList;
    if (lookahead().type != "RPARENT")
    {
        match("INTTK");
        string firstParamName = lookahead().value;
        match("IDENFR");
        paramNamesList.push_back(firstParamName);
        addSymbol(firstParamName, "int", false, false, 1);

        while (lookahead().type == "COMMA")
        {
            match("COMMA");
            match("INTTK");
            string paramName = lookahead().value;
            match("IDENFR");
            paramNamesList.push_back(paramName);
            addSymbol(paramName, "int", false, false, 1);
        }
    }
    match("RPARENT");
    string funcLabelForMIPS = "FUNC_" + funcName;
    code_emit("LABEL", "", "", funcLabelForMIPS);
    size_t prologueInsertionPoint = midcodes.size();
    midcodes.push_back({"NOP_PROLOGUE_PLACEHOLDER", "", "", ""});
    Block();
    int frameSize = calculateFrameSize(symbol_table.back(), slots_max);
    vector<midcode> prologueQuads;
    prologueQuads.push_back({"ADDIU", "$sp", "-" + to_string(frameSize), "$sp"});
    prologueQuads.push_back({"SW", "$ra", "", to_string(frameSize - 4) + "($sp)"});
    for (int i = 0; i < (int)paramNamesList.size(); ++i)
    {
        string paramLocalAddr = get_address(paramNamesList[i]);
        if (i < 4)
        {
            prologueQuads.push_back({"SW", "$a" + to_string(i), "", paramLocalAddr});
        }
        else
        {
            string callerStackArgAddr = to_string(frameSize + (i - 4) * 4) + "($sp)";
            string tempRegForParamLoad = TEMP_REG1;
            prologueQuads.push_back({"LW", tempRegForParamLoad, "", callerStackArgAddr});
            prologueQuads.push_back({"SW", tempRegForParamLoad, "", paramLocalAddr});
        }
    }
    if (prologueInsertionPoint < midcodes.size() && midcodes[prologueInsertionPoint].op == "NOP_PROLOGUE_PLACEHOLDER")
    {
        midcodes.erase(midcodes.begin() + prologueInsertionPoint);
        midcodes.insert(midcodes.begin() + prologueInsertionPoint,
                        prologueQuads.begin(), prologueQuads.end());
    }
    if (return_type_str == "void")
    {
        bool explicitReturnExists = false;
        if (!midcodes.empty())
        {
            for (long k_loop = midcodes.size() - 1; k_loop >= 0; --k_loop)
            {
                const auto &quad = midcodes[k_loop];
                if (quad.op == "J" && quad.result == label_current)
                {
                    explicitReturnExists = true;
                    break;
                }
                if (quad.op == "LABEL" && quad.result == funcLabelForMIPS && quad.result != label_current)
                {
                    break;
                }
            }
        }
        if (!explicitReturnExists)
        {
            code_emit("J", "", "", label_current);
        }
    }
    else
    {
        bool lastInstructionJumpsToEpilogue = false;
        if (!midcodes.empty())
        {
            const auto &lastQuad = midcodes.back();
            if (lastQuad.op == "J" && lastQuad.result == label_current)
            {
                lastInstructionJumpsToEpilogue = true;
            }
        }
    }
    code_emit("LABEL", "", "", label_current);
    code_emit("LW", "$ra", "", to_string(frameSize - 4) + "($sp)");
    code_emit("ADDIU", "$sp", to_string(frameSize), "$sp");
    code_emit("JR", "$ra", "", "");
    leaveScope();
    label_current = oldEpilogueLabel;
}

void CompUnit()
{
    while (lookahead().type == "CONSTTK" ||
           (lookahead().type == "INTTK" && lookahead(1).type == "IDENFR" && lookahead(2).type != "LPARENT"))
    {
        Decl();
    }
    while ((lookahead().type == "INTTK" || lookahead().type == "VOIDTK") &&
           lookahead(1).type == "IDENFR" &&
           lookahead(1).value != "main" &&
           lookahead(2).type == "LPARENT")
    {
        FuncDef();
    }
    MainFuncDef();
}

string FuncType()
{
    if (lookahead().type == "INTTK")
    {
        match("INTTK");
        return "int";
    }
    else if (lookahead().type == "VOIDTK")
    {
        match("VOIDTK");
        return "void";
    }
    return "";
}

string LVal(bool *outIsArrayAccessWithoutIndex)
{
    if (outIsArrayAccessWithoutIndex)
        *outIsArrayAccessWithoutIndex = false;
    string varName = lookahead().value;
    match("IDENFR");
    Symbol sym;
    bool foundSym = false;
    for (auto it = symbol_table.rbegin(); it != symbol_table.rend(); ++it)
    {
        const auto &scope = *it;
        if (scope.count(varName))
        {
            sym = scope.at(varName);
            foundSym = true;
            break;
        }
    }
    string baseAddrStr = get_address(varName);
    if (lookahead().type == "LBRACK")
    {
        if (outIsArrayAccessWithoutIndex)
            *outIsArrayAccessWithoutIndex = false;
        match("LBRACK");
        string idxReg = Exp();
        match("RBRACK");
        string offsetReg = getNextTempReg();
        string four_val_reg = getNextTempReg();
        code_emit("LI", "4", "", four_val_reg);
        code_emit("MUL", idxReg, four_val_reg, offsetReg);
        string elementAddrReg = getNextTempReg();
        if (sym.isGlobal)
        {
            string baseAddrInReg = getNextTempReg();
            code_emit("LA", sym.name, "", baseAddrInReg);
            code_emit("ADD", baseAddrInReg, offsetReg, elementAddrReg);
        }
        else
        {
            string baseAddrInReg = getNextTempReg();
            size_t sp_pos = baseAddrStr.find("($sp)");
            string offset_from_sp_str = (sp_pos != string::npos) ? baseAddrStr.substr(0, sp_pos) : "0";
            code_emit("ADDIU", "$sp", offset_from_sp_str, baseAddrInReg);
            code_emit("ADD", baseAddrInReg, offsetReg, elementAddrReg);
        }
        return elementAddrReg;
    }
    else
    {
        if (outIsArrayAccessWithoutIndex && sym.arraySize > 1)
        {
            *outIsArrayAccessWithoutIndex = true;
        }
        return baseAddrStr;
    }
}

string UnaryExp()
{
    if (lookahead().type == "IDENFR" && lookahead(1).type == "LPARENT")
    {
        return FuncCall();
    }
    else if (lookahead().type == "PLUS" || lookahead().type == "MINU" || lookahead().type == "NOT")
    {
        string op = lookahead().type;
        match(op);
        string operand_reg = UnaryExp();
        string temp_res_reg = getNextTempReg();
        if (op == "MINU")
        {
            code_emit("NEG", operand_reg, "", temp_res_reg);
        }
        else if (op == "NOT")
        {
            code_emit("NOT", operand_reg, "", temp_res_reg);
        }
        else
        {
            code_emit("MOVE", operand_reg, "", temp_res_reg);
        }
        return temp_res_reg;
    }
    else
    {
        return PrimaryExp();
    }
}

string MulExp()
{
    string current_accumulated_value_reg = UnaryExp();

    while (lookahead().type == "MULT" || lookahead().type == "DIV" || lookahead().type == "MOD")
    {
        string op_token_type = lookahead().type;
        string op_code = (op_token_type == "MULT") ? "MUL"
                                                   : (op_token_type == "DIV" ? "DIV" : "MOD");
        match(op_token_type);

        string lhs_operand_for_this_op = current_accumulated_value_reg;
        bool did_spill_lhs = false;
        string spill_addr_str_for_lhs;
        int saved_spill_offset_before_lhs_spill = slots_offset;

        bool lhs_is_temp_reg = (!lhs_operand_for_this_op.empty() && // Ensure not empty string
                                lhs_operand_for_this_op[0] == 't' &&
                                lhs_operand_for_this_op.length() > 1 &&
                                isdigit(lhs_operand_for_this_op[1]));

        if (lhs_is_temp_reg)
        {
            // Always spill LHS if it's in a temp reg before evaluating RHS UnaryExp
            int current_func_max_declared_vars_size = 0;
            if (!symbol_table.empty() && !symbol_table.back().empty())
            {
                for (const auto &pair_sym : symbol_table.back())
                {
                    const Symbol &sym_s = pair_sym.second;
                    if (!sym_s.isGlobal && !sym_s.isFunction)
                    {
                        current_func_max_declared_vars_size = max(current_func_max_declared_vars_size, sym_s.addr + sym_s.arraySize * 4);
                    }
                }
            }
            int spill_offset_on_stack = current_func_max_declared_vars_size + slots_offset;
            spill_addr_str_for_lhs = to_string(spill_offset_on_stack) + "($sp)";

            code_emit("SW", lhs_operand_for_this_op, "", spill_addr_str_for_lhs);
            did_spill_lhs = true;

            slots_offset += SPILL_SLOT_SIZE;
            slots_max = max(slots_max, slots_offset);
        }

        string rhs_value_reg = UnaryExp();

        string final_lhs_reg_for_gen;
        if (did_spill_lhs)
        {
            final_lhs_reg_for_gen = getNextTempReg();
            code_emit("LW", final_lhs_reg_for_gen, "", spill_addr_str_for_lhs);
            slots_offset = saved_spill_offset_before_lhs_spill;
        }
        else
        {
            final_lhs_reg_for_gen = lhs_operand_for_this_op;
        }
        string result_of_this_op_reg = getNextTempReg();
        code_emit(op_code, final_lhs_reg_for_gen, rhs_value_reg, result_of_this_op_reg);
        current_accumulated_value_reg = result_of_this_op_reg;
    }
    return current_accumulated_value_reg;
}

string AddExp()
{
    string current_accumulated_value_reg = MulExp();

    while (lookahead().type == "PLUS" || lookahead().type == "MINU")
    {
        string op_code = (lookahead().type == "PLUS") ? "ADD" : "SUB";
        match(lookahead().type);
        string lhs_operand_for_this_op = current_accumulated_value_reg;
        bool did_spill_lhs = false;
        string spill_addr_str_for_lhs;
        int saved_spill_offset_before_lhs_spill = slots_offset;

        bool lhs_is_temp_reg = (!lhs_operand_for_this_op.empty() &&
                                lhs_operand_for_this_op[0] == 't' &&
                                lhs_operand_for_this_op.length() > 1 &&
                                isdigit(lhs_operand_for_this_op[1]));

        if (lhs_is_temp_reg)
        {
            int current_func_max_declared_vars_size = 0;
            if (!symbol_table.empty() && !symbol_table.back().empty())
            {
                for (const auto &pair_sym : symbol_table.back())
                {
                    const Symbol &sym_s = pair_sym.second;
                    if (!sym_s.isGlobal && !sym_s.isFunction)
                    {
                        current_func_max_declared_vars_size = max(current_func_max_declared_vars_size, sym_s.addr + sym_s.arraySize * 4);
                    }
                }
            }

            int spill_offset_on_stack = current_func_max_declared_vars_size + slots_offset;
            spill_addr_str_for_lhs = to_string(spill_offset_on_stack) + "($sp)";

            code_emit("SW", lhs_operand_for_this_op, "", spill_addr_str_for_lhs);
            did_spill_lhs = true;

            slots_offset += SPILL_SLOT_SIZE;
            slots_max = max(slots_max, slots_offset);
        }

        string rhs_value_reg = MulExp(); // Evaluate RHS

        string final_lhs_reg_for_gen;
        if (did_spill_lhs)
        {
            final_lhs_reg_for_gen = getNextTempReg();
            code_emit("LW", final_lhs_reg_for_gen, "", spill_addr_str_for_lhs);
            slots_offset = saved_spill_offset_before_lhs_spill;
        }
        else
        {
            final_lhs_reg_for_gen = lhs_operand_for_this_op;
        }

        string result_of_this_op_reg = getNextTempReg();
        code_emit(op_code, final_lhs_reg_for_gen, rhs_value_reg, result_of_this_op_reg);
        current_accumulated_value_reg = result_of_this_op_reg;
    }
    return current_accumulated_value_reg;
}

string Exp()
{
    string result_reg = AddExp();
    return result_reg;
}

string PrimaryExp()
{
    if (lookahead().type == "LPARENT")
    {
        match("LPARENT");
        string temp_reg = Exp();
        match("RPARENT");
        return temp_reg;
    }
    else if (lookahead().type == "IDENFR")
    {
        string varName = lookahead().value;

        bool isArrayAccessWithoutIndex = false;
        string lvalResult = LVal(&isArrayAccessWithoutIndex);

        Symbol sym;
        bool foundSym = false;
        for (auto it = symbol_table.rbegin(); it != symbol_table.rend(); ++it)
        {
            if (it->count(varName))
            {
                sym = it->at(varName);
                foundSym = true;
                break;
            }
        }
        bool lvalReturnedRegisterHoldingAddress = (!lvalResult.empty() && lvalResult[0] == 't' && lvalResult.length() > 1 && isdigit(lvalResult[1]));

        if (lvalReturnedRegisterHoldingAddress)
        {
            string tempValueReg = getNextTempReg();
            code_emit("LOAD", lvalResult, "", tempValueReg);
            return tempValueReg;
        }
        else
        {
            string tempDestReg = getNextTempReg();
            if (sym.isConst && sym.arraySize == 1)
            {
                string const_val_str;
                if (!sym.value.empty())
                {
                    const_val_str = sym.value;
                }
                else if (!sym.constValuesList.empty())
                {
                    const_val_str = sym.constValuesList[0];
                }
                code_emit("LI", const_val_str, "", tempDestReg);
            }
            else if (sym.arraySize > 1 && isArrayAccessWithoutIndex)
            {
                if (sym.isGlobal)
                {
                    code_emit("LA", lvalResult, "", tempDestReg);
                }
                else
                {
                    size_t sp_pos = lvalResult.find("($sp)");
                    string offset_str = lvalResult.substr(0, sp_pos);
                    code_emit("ADDIU", "$sp", offset_str, tempDestReg);
                }
            }
            else
            {
                code_emit("LOAD", lvalResult, "", tempDestReg);
            }
            return tempDestReg;
        }
    }
    else if (lookahead().type == "INTCON")
    {
        string num = lookahead().value;
        match("INTCON");
        string temp_reg = getNextTempReg();
        code_emit("LI", num, "", temp_reg);
        return temp_reg;
    }
    else if (lookahead().type == "GETINTTK")
    {
        match("GETINTTK");
        match("LPARENT");
        match("RPARENT");
        string temp_reg = getNextTempReg();
        code_emit("GETINT", "", "", temp_reg);
        return temp_reg;
    }
    return "";
}

void skip_comments(const string &code, size_t &i)
{
    if (i + 1 < code.length() && code[i] == '/' && code[i + 1] == '/')
    {
        i += 2;
        while (i < code.length() && code[i] != '\n')
            i++;
    }
    else if (i + 1 < code.length() && code[i] == '/' && code[i + 1] == '*')
    {
        size_t comment_start_pos = i;
        i += 2;
        while (i < code.length())
        {
            if (i + 1 < code.length() && code[i] == '*' && code[i + 1] == '/')
            {
                i += 2;
                return;
            }
            i++;
        }
    }
}

void tokenize(const string &code)
{
    size_t i = 0, len = code.length();
    while (i < len)
    {
        if (isspace(code[i]))
        {
            i++;
            continue;
        }
        if (i + 1 < len && (code.substr(i, 2) == "//" || code.substr(i, 2) == "/*"))
        {
            skip_comments(code, i);
            continue;
        }
        if (code[i] == '"')
        {
            string str_val;
            size_t str_start_pos = i;
            i++;
            while (i < len && code[i] != '"')
            {
                if (code[i] == '\\')
                {
                    i++;
                    if (i < len)
                    {
                        switch (code[i])
                        {
                        case 'n':
                            str_val += '\n';
                            break;
                        case 't':
                            str_val += '\t';
                            break;
                        case '\\':
                            str_val += '\\';
                            break;
                        case '"':
                            str_val += '"';
                            break;
                        default:
                            str_val += '\\';
                            str_val += code[i];
                            break;
                        }
                    }
                }
                else
                {
                    str_val += code[i];
                }
                i++;
            }
            if (i < len && code[i] == '"')
            {
                tokens.push_back({"STRCON", "\"" + str_val + "\""});
                i++;
            }
            continue;
        }
        if (i + 1 < len)
        {
            string two_char_op = code.substr(i, 2);
            if (symbols.count(two_char_op))
            {
                tokens.push_back({symbols[two_char_op], two_char_op});
                i += 2;
                continue;
            }
        }
        string one_char_op = code.substr(i, 1);
        if (symbols.count(one_char_op))
        {
            tokens.push_back({symbols[one_char_op], one_char_op});
            i++;
            continue;
        }
        if (isalpha(code[i]) || code[i] == '_')
        {
            size_t start = i;
            while (i < len && (isalnum(code[i]) || code[i] == '_'))
            {
                i++;
            }
            string word = code.substr(start, i - start);
            tokens.push_back({keys.count(word) ? keys[word] : "IDENFR", word});
            continue;
        }
        if (isdigit(code[i]))
        {
            size_t start = i;
            while (i < len && isdigit(code[i]))
            {
                i++;
            }
            tokens.push_back({"INTCON", code.substr(start, i - start)});
            continue;
        }
    }
}

// ======================= MIPS生成 =======================
void DataSection(ofstream &mipsFile)
{
    mipsFile << ".data\n";
    if (!symbol_table.empty())
    {
        for (const auto &symPair : symbol_table[0])
        {
            const Symbol &sym = symPair.second;
            if (sym.isFunction)
                continue;
            if (sym.isGlobal)
            {
                if (sym.isConst)
                {
                    mipsFile << sym.name << ": .word ";
                    if (!sym.constValuesList.empty())
                    {
                        for (size_t k = 0; k < sym.constValuesList.size(); ++k)
                        {
                            mipsFile << sym.constValuesList[k] << (k == sym.constValuesList.size() - 1 ? "" : ", ");
                        }
                    }
                    else if (sym.arraySize > 0 && sym.arraySize != 1)
                    {
                        for (int k = 0; k < sym.arraySize; ++k)
                            mipsFile << "0" << (k == sym.arraySize - 1 ? "" : ", ");
                    }
                    else
                    {
                        mipsFile << (sym.value.empty() ? "0" : sym.value);
                    }
                    mipsFile << "\n";
                }
                else
                {
                    if (sym.arraySize > 1)
                    {
                        if (!sym.value.empty() && sym.value.front() == '{' && sym.value.back() == '}')
                        {
                            string innerValues = sym.value.substr(1, sym.value.length() - 2);
                            if (innerValues.empty())
                            {
                                mipsFile << sym.name << ": .space " << sym.arraySize * 4 << "\n";
                            }
                            else
                            {
                                stringstream ss(innerValues);
                                string item;
                                vector<string> initializers;
                                while (getline(ss, item, ','))
                                {
                                    item.erase(0, item.find_first_not_of(" \t\n\r\f\v"));
                                    item.erase(item.find_last_not_of(" \t\n\r\f\v") + 1);
                                    initializers.push_back(item);
                                }
                                mipsFile << sym.name << ": .word ";
                                for (size_t k = 0; k < initializers.size(); ++k)
                                {
                                    mipsFile << initializers[k] << (k == initializers.size() - 1 ? "" : ", ");
                                }
                                for (int k_init = initializers.size(); k_init < sym.arraySize; ++k_init)
                                {
                                    mipsFile << ", 0";
                                }
                                mipsFile << "\n";
                            }
                        }
                        else
                        {
                            mipsFile << sym.name << ": .space " << sym.arraySize * 4 << "\n";
                        }
                    }
                    else
                    {
                        string initVal = sym.value.empty() ? "0" : sym.value;
                        mipsFile << sym.name << ": .word " << initVal << "\n";
                    }
                }
            }
        }
    }
    for (size_t i = 0; i < stringLiterals.size(); ++i)
    {
        mipsFile << "str" << i << ": .asciiz " << stringLiterals[i] << "\n";
    }
}
void TextSection(ofstream &mipsFile)
{
    mipsFile << ".text\n";
    mipsFile << ".globl main\n";
    mipsFile << "main:\n";
    mipsFile << "    li   $sp, 0x7ffffffc\n";
    mipsFile << "    jal  FUNC_main\n";
    mipsFile << "    li   $v0, 10\n";
    mipsFile << "    syscall\n";
    for (const auto &quad : midcodes)
    {
        if (quad.op == "NOP_PROLOGUE_PLACEHOLDER")
        {
            continue;
        }
        else if (quad.op == "PRINTF_STR")
        {
            mipsFile << "li $v0, 4\n";
            mipsFile << "la $a0, " << quad.arg1 << "\n";
            mipsFile << "syscall\n";
        }
        else if (quad.op == "PRINTF_ARG")
        {
            mipsFile << "li $v0, 1\n";
            mipsFile << "move $a0, " << quad.arg1 << "\n";
            mipsFile << "syscall\n";
        }
        else if (quad.op == "GETINT")
        {
            mipsFile << "li $v0, 5\n";
            mipsFile << "syscall\n";
            mipsFile << "move " << quad.result << ", $v0\n";
        }
        else if (quad.op == "SYSCALL")
        {
            mipsFile << "syscall\n";
        }
        else if (quad.op == "LOAD")
        {
            if (!quad.arg1.empty() && quad.arg1[0] == '$')
            {
                mipsFile << "lw " << quad.result << ", 0(" << quad.arg1 << ")\n";
            }
            else if (quad.arg1.find("($sp)") == string::npos && !isdigit(quad.arg1[0]) && quad.arg1.find("str") != 0 && quad.arg1.find("L_") != 0)
            {
                bool isDataLabel = false;
                if (!symbol_table.empty() && symbol_table[0].count(quad.arg1))
                {
                    isDataLabel = !symbol_table[0][quad.arg1].isFunction;
                }
                if (isDataLabel)
                {
                    string addrReg = "$at";
                    mipsFile << "la " << addrReg << ", " << quad.arg1 << "\n";
                    mipsFile << "lw " << quad.result << ", 0(" << addrReg << ")\n";
                }
                else
                {
                    mipsFile << "lw " << quad.result << ", " << quad.arg1 << "\n";
                }
            }
            else
            {
                mipsFile << "lw " << quad.result << ", " << quad.arg1 << "\n";
            }
        }
        else if (quad.op == "LW")
        {
            mipsFile << "lw " << quad.arg1 << ", " << quad.result << "\n";
        }
        else if (quad.op == "STORE")
        {
            if (!quad.result.empty() && quad.result[0] == '$')
            {
                mipsFile << "sw " << quad.arg1 << ", 0(" << quad.result << ")\n";
            }
            else if (quad.result.find("($sp)") == string::npos && !isdigit(quad.result[0]) && quad.result.find("str") != 0 && quad.result.find("L_") != 0)
            {
                string addrReg = "$at";
                mipsFile << "la " << addrReg << ", " << quad.result << "\n";
                mipsFile << "sw " << quad.arg1 << ", 0(" << addrReg << ")\n";
            }
            else
            {
                mipsFile << "sw " << quad.arg1 << ", " << quad.result << "\n";
            }
        }
        else if (quad.op == "SW")
        {
            mipsFile << "sw " << quad.arg1 << ", " << quad.result << "\n";
        }
        else if (quad.op == "ADD")
        {
            mipsFile << "add " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "SUB")
        {
            mipsFile << "sub " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "MUL")
        {
            mipsFile << "mult " << quad.arg1 << ", " << quad.arg2 << "\n";
            mipsFile << "mflo " << quad.result << "\n";
        }
        else if (quad.op == "DIV")
        {
            mipsFile << "div " << quad.arg1 << ", " << quad.arg2 << "\n";
            mipsFile << "mflo " << quad.result << "\n";
        }
        else if (quad.op == "MOD")
        {
            mipsFile << "div " << quad.arg1 << ", " << quad.arg2 << "\n";
            mipsFile << "mfhi " << quad.result << "\n";
        }
        else if (quad.op == "NEG")
        {
            mipsFile << "sub " << quad.result << ", $zero, " << quad.arg1 << "\n";
        }
        else if (quad.op == "POS")
        {
            mipsFile << "add " << quad.result << ", " << quad.arg1 << ", $zero\n";
        }
        else if (quad.op == "ASSIGN")
        {
            string temp_assign_reg = TEMP_REG1;
            mipsFile << "li " << temp_assign_reg << ", " << quad.arg2 << "\n";
            if (!quad.result.empty() && quad.result[0] == '$')
            {
                mipsFile << "sw " << temp_assign_reg << ", 0(" << quad.result << ")\n";
            }
            else if (quad.result.find("($sp)") == string::npos && !isdigit(quad.result[0]))
            {
                string addrReg = "$at";
                mipsFile << "la " << addrReg << ", " << quad.result << "\n";
                mipsFile << "sw " << temp_assign_reg << ", 0(" << addrReg << ")\n";
            }
            else
            {
                mipsFile << "sw " << temp_assign_reg << ", " << quad.result << "\n";
            }
        }
        else if (quad.op == "IMM")
        {
            mipsFile << "li " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "MOVE")
        {
            mipsFile << "move " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "PUSH")
        {
            mipsFile << "addiu $sp, $sp, -4\n";
            mipsFile << "sw " << quad.arg1 << ", 0($sp)\n";
        }
        else if (quad.op == "CALL")
        {
            mipsFile << "jal FUNC_" << quad.arg1 << "\n";
            if (!quad.result.empty())
            {
                mipsFile << "move " << quad.result << ", $v0\n";
            }
        }
        else if (quad.op == "AND")
        {
            string false_label = "L_and_false_" + to_string(labelCount);
            string end_label = "L_and_end_" + to_string(labelCount);
            labelCount++;
            mipsFile << "beqz " << quad.arg1 << ", " << false_label << endl;
            mipsFile << "nop" << endl;
            mipsFile << "beqz " << quad.arg2 << ", " << false_label << endl;
            mipsFile << "nop" << endl;
            mipsFile << "li " << quad.result << ", 1" << endl;
            mipsFile << "j " << end_label << endl;
            mipsFile << "nop" << endl;
            mipsFile << false_label << ":" << endl;
            mipsFile << "li " << quad.result << ", 0" << endl;
            mipsFile << end_label << ":" << endl;
        }
        else if (quad.op == "OR")
        {
            string true_label = "L_or_true_" + to_string(labelCount);
            string end_label = "L_or_end_" + to_string(labelCount);
            labelCount++;
            mipsFile << "bnez " << quad.arg1 << ", " << true_label << endl;
            mipsFile << "nop" << endl;
            mipsFile << "bnez " << quad.arg2 << ", " << true_label << endl;
            mipsFile << "nop" << endl;
            mipsFile << "li " << quad.result << ", 0" << endl;
            mipsFile << "j " << end_label << endl;
            mipsFile << "nop" << endl;
            mipsFile << true_label << ":" << endl;
            mipsFile << "li " << quad.result << ", 1" << endl;
            mipsFile << end_label << ":" << endl;
        }
        else if (quad.op == "NOT")
        {
            mipsFile << "sltiu " << quad.result << ", " << quad.arg1 << ", 1" << endl;
        }
        else if (quad.op == "LA")
        {
            mipsFile << "la " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "ADDIU")
        {
            mipsFile << "addiu " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "LSS")
        {
            mipsFile << "slt " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << endl;
        }
        else if (quad.op == "LEQ")
        {
            mipsFile << "slt " << TEMP_REG1 << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
            mipsFile << "seq " << TEMP_REG2 << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
            mipsFile << "or " << quad.result << ", " << TEMP_REG1 << ", " << TEMP_REG2 << "\n";
        }
        else if (quad.op == "GRE")
        {
            mipsFile << "slt " << quad.result << ", " << quad.arg2 << ", " << quad.arg1 << endl;
        }
        else if (quad.op == "GEQ")
        {
            mipsFile << "slt " << TEMP_REG1 << ", " << quad.arg2 << ", " << quad.arg1 << "\n";
            mipsFile << "seq " << TEMP_REG2 << ", " << quad.arg2 << ", " << quad.arg1 << "\n";
            mipsFile << "or " << quad.result << ", " << TEMP_REG1 << ", " << TEMP_REG2 << "\n";
        }
        else if (quad.op == "EQL")
        {
            mipsFile << "xor " << TEMP_REG1 << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
            mipsFile << "sltiu " << quad.result << ", " << TEMP_REG1 << ", 1\n";
        }
        else if (quad.op == "NEQ")
        {
            mipsFile << "xor " << TEMP_REG1 << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
            mipsFile << "sltu " << quad.result << ", $zero, " << TEMP_REG1 << "\n";
        }
        else if (quad.op == "BEQ")
        {
            mipsFile << "beq " << quad.arg1 << ", " << quad.arg2 << ", " << quad.result << "\n";
        }
        else if (quad.op == "BEQZ" || quad.op == "BNEZ")
        {
            mipsFile << (quad.op == "BEQZ" ? "beqz " : "bnez ") << quad.arg1 << ", " << quad.result << "\n";
        }
        else if (quad.op == "BNE")
        {
            mipsFile << "bne " << quad.arg1 << ", " << quad.arg2 << ", " << quad.result << "\n";
        }
        else if (quad.op == "J")
        {
            mipsFile << "j " << quad.result << "\n";
        }
        else if (quad.op == "LABEL")
        {
            mipsFile << quad.result << ":\n";
        }
        else if (quad.op == "FUNC_DEF")
        {
            mipsFile << quad.arg1 << ":\n";
        }
        else if (quad.op == "LI")
        {
            mipsFile << "li " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "JR")
        {
            mipsFile << "jr " << quad.arg1 << "\n";
        }
    }
}
void MIPS()
{
    ofstream mipsFile("mips.txt");
    DataSection(mipsFile);
    TextSection(mipsFile);
}

int main()
{
    ifstream infile("testfile.txt");
    stringstream buffer;
    buffer << infile.rdbuf();
    infile.close();
    enterScope(true);
    tokenize(buffer.str());
    CompUnit();
    MIPS();
    ifstream mips_check("mips.txt");
    mips_check.close();
    return 0;
}