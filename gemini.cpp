#include <iostream>
#include <fstream>
#include <cctype>
#include <unordered_map>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>

#define TEMP_REG1 "$t8" // Kept from original 111.cpp, reference uses MIPS_TEMP_REG_1
#define TEMP_REG2 "$t9" // Kept from original 111.cpp, reference uses MIPS_TEMP_REG_2
#define SPILL_SLOT_SIZE 4

using namespace std;

// Renamed keys to keywords
unordered_map<string, string> keywords = {{"const", "CONSTTK"}, {"int", "INTTK"}, {"break", "BREAKTK"}, {"continue", "CONTINUETK"}, {"if", "IFTK"}, {"else", "ELSETK"}, {"while", "WHILETK"}, {"return", "RETURNTK"}, {"void", "VOIDTK"}, {"main", "MAINTK"}, {"getint", "GETINTTK"}, {"printf", "PRINTFTK"}};
unordered_map<string, string> symbols = {{"!=", "NEQ"}, {"!", "NOT"}, {"&&", "AND"}, {"||", "OR"}, {"+", "PLUS"}, {"-", "MINU"}, {"*", "MULT"}, {"/", "DIV"}, {"%", "MOD"}, {"<", "LSS"}, {"<=", "LEQ"}, {">", "GRE"}, {">=", "GEQ"}, {"==", "EQL"}, {"=", "ASSIGN"}, {";", "SEMICN"}, {",", "COMMA"}, {"(", "LPARENT"}, {")", "RPARENT"}, {"[", "LBRACK"}, {"]", "RBRACK"}, {"{", "LBRACE"}, {"}", "RBRACE"}};

struct Token
{
    string type;
    string value;
};

// Renamed midcode to Quadruple
struct Quadruple
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
    string value = ""; // For single const value or global init string
    vector<string> constValuesList; // For const array values
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
string currentFunctionEpilogueLabel = "";      // Renamed from label_current
vector<Token> tokens;                          //词法分析结果
vector<string> loopStartLabels;                // Renamed from label_start  //开始标签
vector<string> loopEndLabels;                  // Renamed from label_end    //结束标签
vector<Quadruple> intermediateCode;            // Renamed from midcodes     //中间代码
vector<unordered_map<string, Symbol>> symbolTableStack; // Renamed from symbol_table //符号表
vector<string> stringLiterals;                 //存储字符串常量
int tempAddr = 0;                              //记录当前临时变量地址 (Max offset for declared local vars in func)
int strCount = 0;                              //记录字符串常量个数

// Spill management variables, renamed and new one added
int current_spill_slot_offset = 0;             // Renamed from slots_offset //记录溢出槽位偏移量 (within spill area)
int max_spill_slots_needed_for_function = 0;   // Renamed from slots_max    //记录最大溢出槽位数

// Declaration for reset_spill_management
void reset_spill_management() {
    current_spill_slot_offset = 0;
    max_spill_slots_needed_for_function = 0;
}

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

// Updated Const Exp related functions
string ConstExp();
string LOrExpForConst();
string LAndExpForConst();
string EqExpForConst();
string RelExpForConst();
string AddExpForConst_Old(); // Renamed from AddExpForConst
string MulExpForConst_Old(); // Renamed from MulExpForConst
string UnaryExpForConst_Old(); // Renamed from UnaryExpForConst
string PrimaryExpForConst();


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
    sym.addr = tempAddr; // Assign current offset for local vars
    sym.isConst = isConst;
    sym.isFunction = isFunction;
    sym.arraySize = arraySize;
    if (symbolTableStack.size() > 1) // Scope is not global
    {
        sym.isGlobal = false;
    }
    else // Global scope
    {
        sym.isGlobal = true;
    }
    symbolTableStack.back()[name] = sym;
    if (!sym.isGlobal && !isFunction) // Only for local variables/params
    {
        tempAddr += arraySize * 4; // Increment offset for next local variable/param
    }
}

// 符号表查找符号 - Enhanced from lexer_phase2(4).cpp
bool getConstValueFromSymbolTable(const string &identName, int &outValue) { // Renamed from get_const
    for (auto it_s = symbolTableStack.rbegin(); it_s != symbolTableStack.rend(); ++it_s) {
        if (it_s->count(identName)) {
            const Symbol& sym = it_s->at(identName);
            if (sym.isConst) {
                if (sym.arraySize > 1 && sym.constValuesList.empty() && sym.value.empty()) {
                    cerr << "Error: Array constant '" << identName << "' cannot be used directly as a scalar in a constant expression without an initializer to infer a value." << endl;
                    return false; // Or exit(1)
                }
                 if (sym.arraySize > 1 && !sym.constValuesList.empty() && sym.constValuesList[0].find_first_not_of("-0123456789") != string::npos) {
                    cerr << "Error: Array constant '" << identName << "' first element cannot be directly used as a scalar if not a simple number." << endl;
                    return false; // Or exit(1)
                }
                if (sym.arraySize > 1 && sym.constValuesList.empty() && !sym.value.empty() && sym.value.front() == '{') {
                     cerr << "Error: Array constant '" << identName << "' with aggregate initializer cannot be used directly as a scalar." << endl;
                    return false; // Or exit(1)
                }

                string valToParse;
                if (!sym.value.empty() && sym.arraySize == 1) { // Only use sym.value if it's a scalar
                    valToParse = sym.value;
                } else if (!sym.constValuesList.empty()) {
                    valToParse = sym.constValuesList[0]; // Use the first element if it's a list (even for scalar, it's stored there)
                } else {
                    cerr << "Error: Constant '" << identName << "' has no value." << endl;
                    return false; // Or exit(1)
                }

                try {
                    outValue = stoi(valToParse);
                    return true;
                } catch (const std::exception& e) {
                    cerr << "Error: Invalid numeric value for constant '" << identName << "': " << valToParse << endl;
                    return false; // Or exit(1)
                }
            } else { // Non-const symbol found first
                // cerr << "Error: Identifier '" << identName << "' is not a constant in constant expression." << endl;
                return false; // Or exit(1)
            }
        }
    }
    // cerr << "Error: Constant '" << identName << "' not found in constant expression." << endl;
    return false; // Or exit(1)
}


// 查找变量地址
string getVariableAddress(const string &varName) // Renamed from get_address
{
    for (auto it = symbolTableStack.rbegin(); it != symbolTableStack.rend(); ++it)
    {
        const auto &scope = *it;
        if (scope.count(varName))
        {
            const Symbol &sym = scope.at(varName);
            if (sym.isGlobal)
            {
                return sym.name; // Global variable, use label
            }
            else
            {
                // Local variable, offset from $sp
                return to_string(sym.addr) + "($sp)";
            }
        }
    }
    cerr << "错误：变量 '" << varName << "' 未声明 (getVariableAddress)" << endl;
    exit(1); // Should be caught by semantic checks earlier ideally
    return ""; // Should not reach here
}

// 中间代码生成 - Renamed from code_emit to gen
void gen(const string &op, const string &arg1_param, const string &arg2_param, const string &result_param)
{
    auto mapOperand = [&](const string &name) -> string // Capture symbolTableStack
    {
        if (name.empty())
            return "";
        if (name[0] == '$') // Already a MIPS register
            return name;
        if (name.rfind("L_", 0) == 0) // Label
            return name;
        if (name.rfind("FUNC_", 0) == 0) // Function Label
            return name;
        if (name.find("($sp)") != string::npos) { // Stack address
            return name;
        }
        if (isdigit(name[0]) || (name.size() > 1 && name[0] == '-' && isdigit(name[1]))) // Immediate number
            return name;
        if (name.rfind("str", 0) == 0) // String literal label
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
        // Check if it's a global variable/constant data label
        if (!symbolTableStack.empty() && symbolTableStack[0].count(name)) {
             const Symbol& sym_global = symbolTableStack[0].at(name); // Corrected variable name
             if (sym_global.isGlobal && !sym_global.isFunction) return name; // Global data label
        }
        // Check if it's a temporary variable t0-t7 (map to $t0-$t7)
        if (name.length() >= 1 && name[0] == 't' && (name.length() > 1 && isdigit(name[1]))) { // Ensure t is followed by a digit
            bool all_digits_after_t = true;
            for(size_t k=1; k < name.length(); ++k) {
                if (!isdigit(name[k])) {
                    all_digits_after_t = false;
                    break;
                }
            }
            if (all_digits_after_t) return "$" + name; // Map tX to $tX
        }
        // If it's a declared local variable name, map to its stack address
        for (auto it_s = symbolTableStack.rbegin(); it_s != symbolTableStack.rend(); ++it_s) {
            if (it_s->count(name)) {
                const Symbol& sym_s = it_s->at(name);
                if (!sym_s.isGlobal && !sym_s.isFunction) { // It's a local variable
                    return to_string(sym_s.addr) + "($sp)";
                }
            }
        }
        // Default: assume it's a label or needs no mapping, or error
        // For robustness, if it's not recognized, it might be an error.
        // However, keeping original behavior for now if it was intended to pass through.
        return name;
    };

    string arg1_mapped = mapOperand(arg1_param);
    string arg2_mapped = mapOperand(arg2_param);
    string result_mapped = mapOperand(result_param);

    if (op == "GETINT")
    {
        intermediateCode.push_back({"GETINT", "", "", result_mapped});
    }
    else if (op == "LABEL")
    {
        intermediateCode.push_back({op, "", "", result_mapped});
    }
    else if (op == "NOP_PROLOGUE_PLACEHOLDER")
    {
        intermediateCode.push_back({op, "", "", ""});
    }
    else if (op == "PRINTF_STR")
    {
        intermediateCode.push_back({op, arg1_mapped, "", ""});
    }
    else if (op == "PRINTF_ARG")
    {
        intermediateCode.push_back({op, arg1_mapped, "", ""});
    }
    else
    {
        intermediateCode.push_back({op, arg1_mapped, arg2_mapped, result_mapped});
    }
}


// 查找临时寄存器
string getNextTempReg()
{
    static int tempCount = 0;
    string reg = "t" + to_string(tempCount % 8); // t0-t7 cycle
    tempCount++;
    return reg;
}
bool is_temp_register(const string& reg_name) { // Added from reference
    if (reg_name.length() < 2 || reg_name[0] != 't') {
        return false;
    }
    // Check if it's t0-t7, not $t8, $t9 used as fixed temp by MIPS generator
    if (reg_name == "t8" || reg_name == "t9") return false; // TEMP_REG1/2 are $t8/$t9
    for (size_t i = 1; i < reg_name.length(); ++i) {
        if (!isdigit(reg_name[i])) {
            return false;
        }
    }
    return true;
}

// 查找下一个Token
Token lookahead(int offset = 0)
{
    if (pos + offset < tokens.size() && pos + offset >= 0) // Added check for offset >=0
        return tokens[pos + offset];
    else
        return Token{"EOF", ""};
}

// 作用域
void enterScope(bool isNewFunctionFrame = false) // Modified to match reference
{
    symbolTableStack.emplace_back();
    if (isNewFunctionFrame) { // Was: !isGlobal
        tempAddr = 0; // Reset base offset for declared variables for the new function
    }
}

void leaveScope()
{
    if (!symbolTableStack.empty())
    {
        symbolTableStack.pop_back();
    }
}

// 计算当前作用域中需要的帧大小 - Modified to match reference
int calculateFrameSize(int maxTempAddrReachedInFunction, int spill_bytes_needed)
{
    int totalVariableSpace = maxTempAddrReachedInFunction + spill_bytes_needed;
    // Add space for $ra (4 bytes)
    // Align to 8 bytes
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
    else
    {
        cerr << "Syntax error on line (approx token " << pos << "): expected " << expected << " but got "
             << (pos < tokens.size() ? tokens[pos].type : "EOF")
             << (pos < tokens.size() ? " ('" + tokens[pos].value + "')" : "") << endl;
        exit(1);
    }
}

string RelExp() {
    string left_val_reg = AddExp();

    while (lookahead().type == "LSS" || lookahead().type == "GRE" ||
           lookahead().type == "LEQ" || lookahead().type == "GEQ") {
        string op_type = lookahead().type;
        match(op_type);

        string spill_addr_for_left;
        bool left_was_spilled = false;
        int saved_spill_offset_before_lhs_spill = current_spill_slot_offset;

        if (is_temp_register(left_val_reg)) { // Use helper
            spill_addr_for_left = to_string(tempAddr + current_spill_slot_offset) + "($sp)"; // Use tempAddr
            gen(op_type == "SW" ? "SW" : "SW", left_val_reg, "", spill_addr_for_left); // Gen takes care of $ prefix
            left_was_spilled = true;
            current_spill_slot_offset += SPILL_SLOT_SIZE;
            max_spill_slots_needed_for_function = max(max_spill_slots_needed_for_function, current_spill_slot_offset);
        }

        string right_val_reg = AddExp();

        string actual_left_operand_reg = left_val_reg;
        if (left_was_spilled) {
            actual_left_operand_reg = getNextTempReg();
            gen(op_type == "LW" ? "LW" : "LW", actual_left_operand_reg, "", spill_addr_for_left); // Gen takes care of $ prefix
            current_spill_slot_offset = saved_spill_offset_before_lhs_spill; 
        }

        string temp_res_reg = getNextTempReg();
        gen(op_type, actual_left_operand_reg, right_val_reg, temp_res_reg);
        left_val_reg = temp_res_reg;
    }
    return left_val_reg;
}

string EqExp() {
    string left_val_reg = RelExp();
    while (lookahead().type == "EQL" || lookahead().type == "NEQ") {
        string op_type = lookahead().type;
        match(op_type);
        string spill_addr_for_left;
        bool left_was_spilled = false;
        int saved_spill_offset_before_lhs_spill = current_spill_slot_offset;

        if (is_temp_register(left_val_reg)) { // Use helper
            spill_addr_for_left = to_string(tempAddr + current_spill_slot_offset) + "($sp)"; // Use tempAddr
            gen(op_type == "SW" ? "SW" : "SW", left_val_reg, "", spill_addr_for_left);
            left_was_spilled = true;
            current_spill_slot_offset += SPILL_SLOT_SIZE;
            max_spill_slots_needed_for_function = max(max_spill_slots_needed_for_function, current_spill_slot_offset);
        }

        string right_val_reg = RelExp();
        string actual_left_operand_reg = left_val_reg;
        if (left_was_spilled) {
            actual_left_operand_reg = getNextTempReg();
            gen(op_type == "LW" ? "LW" : "LW", actual_left_operand_reg, "", spill_addr_for_left);
            current_spill_slot_offset = saved_spill_offset_before_lhs_spill;
        }
        string temp_res_reg = getNextTempReg();
        gen(op_type, actual_left_operand_reg, right_val_reg, temp_res_reg);
        left_val_reg = temp_res_reg;
    }
    return left_val_reg;
}

// LAndExp and LOrExp from 111.cpp are kept as their logic is sound
// given that EqExp/RelExp already produce 0/1.
string LAndExp()
{
    string left_operand_result = EqExp(); 
    string overall_result_reg = left_operand_result; 

    if (lookahead().type == "AND") { 
        overall_result_reg = getNextTempReg();
        gen("MOVE", left_operand_result, "", overall_result_reg);
    }

    while (lookahead().type == "AND")
    {
        match("AND");
        string skip_rhs_eval_label = "L_skip_eval_rhs_and_" + to_string(labelCount++);
        string end_this_and_op_label = "L_end_this_and_op_" + to_string(labelCount++);

        gen("BEQZ", overall_result_reg, "", skip_rhs_eval_label);

        string right_operand_result = EqExp();
        gen("MOVE", right_operand_result, "", overall_result_reg); 
        gen("J", "", "", end_this_and_op_label);

        gen("LABEL", "", "", skip_rhs_eval_label);
        gen("LI", "0", "", overall_result_reg);


        gen("LABEL", "", "", end_this_and_op_label);
    }
    return overall_result_reg;
}

string LOrExp()
{
    string left_operand_result = LAndExp(); 
    string overall_result_reg = left_operand_result; 

    if (lookahead().type == "OR") { 
        overall_result_reg = getNextTempReg();
        gen("MOVE", left_operand_result, "", overall_result_reg);
    }

    while (lookahead().type == "OR")
    {
        match("OR");
        string skip_rhs_eval_label = "L_skip_eval_rhs_or_" + to_string(labelCount++);
        string end_this_or_op_label = "L_end_this_or_op_" + to_string(labelCount++);

        gen("BNEZ", overall_result_reg, "", skip_rhs_eval_label);

        string right_operand_result = LAndExp();
        gen("MOVE", right_operand_result, "", overall_result_reg); 
        gen("J", "", "", end_this_or_op_label);

        gen("LABEL", "", "", skip_rhs_eval_label);
        gen("LI", "1", "", overall_result_reg);

        gen("LABEL", "", "", end_this_or_op_label);
    }
    return overall_result_reg;
}


string Cond()
{
    string condReg = LOrExp(); // LOrExp already returns 0 or 1
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
            default: // Keep unrecognized escapes like \q as is
                processedPart += fmt[i + 1]; // Reference behavior
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
        enterScope(false); // Block is not necessarily a new function frame
        Block();
        leaveScope();
    }
    else if (lookahead().type == "IFTK")
    {
        match("IFTK");
        match("LPARENT");
        string condReg = Cond();
        match("RPARENT");
        string falsePathLabel = "L_if_false_path_" + to_string(labelCount++);
        gen("BEQZ", condReg, "", falsePathLabel);
        Stmt();
        if (lookahead().type == "ELSETK")
        {
            string endOfIfElseLabel = "L_endifelse_" + to_string(labelCount++);
            gen("J", "", "", endOfIfElseLabel);
            gen("LABEL", "", "", falsePathLabel);
            match("ELSETK");
            Stmt();
            gen("LABEL", "", "", endOfIfElseLabel);
        }
        else
        {
            gen("LABEL", "", "", falsePathLabel);
        }
    }
    else if (lookahead().type == "WHILETK")
    {
        match("WHILETK");
        string startLabel = "L_while_" + to_string(labelCount++);
        string endLabel = "L_endwhile_" + to_string(labelCount++);

        loopStartLabels.push_back(startLabel); // Use renamed global
        loopEndLabels.push_back(endLabel);     // Use renamed global

        gen("LABEL", "", "", startLabel);
        match("LPARENT");
        string condReg = Cond();
        match("RPARENT");
        gen("BEQZ", condReg, "", endLabel);

        Stmt();

        gen("J", "", "", startLabel);
        gen("LABEL", "", "", endLabel);

        loopStartLabels.pop_back();
        loopEndLabels.pop_back();
    }
    else if (lookahead().type == "BREAKTK")
    {
        match("BREAKTK");
        if (loopEndLabels.empty()) { // Check from reference
            cerr << "Syntax error: 'break' not in a loop." << endl;
            exit(1);
        }
        gen("J", "", "", loopEndLabels.back());
        match("SEMICN");
    }
    else if (lookahead().type == "CONTINUETK")
    {
        match("CONTINUETK");
         if (loopStartLabels.empty()) { // Check from reference
            cerr << "Syntax error: 'continue' not in a loop." << endl;
            exit(1);
        }
        gen("J", "", "", loopStartLabels.back());
        match("SEMICN");
    }
    else if (lookahead().type == "RETURNTK")
    {
        match("RETURNTK");
        if (lookahead().type != "SEMICN")
        {
            string retReg = Exp();
            gen("MOVE", retReg, "", "$v0");
        }
        if (!currentFunctionEpilogueLabel.empty()) // Use renamed global
        {
            gen("J", "", "", currentFunctionEpilogueLabel);
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
        else { // From reference
            cerr << "Error: Malformed string literal for printf: " << rawFormatStr << endl;
            exit(1);
        }
        match("STRCON");

        vector<string> arg_spill_addrs;
        int saved_spill_offset_for_printf = current_spill_slot_offset;

        auto calculate_printf_arg_spill_address = [&]() -> string // Use tempAddr
        {
            int total_offset = tempAddr + current_spill_slot_offset;
            string spill_addr = to_string(total_offset) + "($sp)";
            return spill_addr;
        };

        while (lookahead().type == "COMMA")
        {
            match("COMMA");
            string tempReg = Exp();
            string spill_loc = calculate_printf_arg_spill_address();
            gen("SW", tempReg, "", spill_loc);
            arg_spill_addrs.push_back(spill_loc);
            current_spill_slot_offset += SPILL_SLOT_SIZE;
            max_spill_slots_needed_for_function = max(max_spill_slots_needed_for_function, current_spill_slot_offset);
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
                if (argIdx >= arg_spill_addrs.size()) { // From reference
                    cerr << "Error: Not enough arguments provided to printf for format string." << endl;
                    exit(1);
                }
                string temp_for_print = getNextTempReg();
                gen("LW", temp_for_print, "", arg_spill_addrs[argIdx]);
                gen("PRINTF_ARG", temp_for_print, "", "");
                argIdx++;
            }
            else
            {
                string label = "str" + to_string(strCount++);
                string mipsEscapedPart;
                for (char c : part_content) // MIPS specific escapes
                {
                    if (c == '\n') mipsEscapedPart += "\\n";
                    else if (c == '\t') mipsEscapedPart += "\\t";
                    else if (c == '"') mipsEscapedPart += "\\\""; // escaped for MIPS .asciiz
                    else if (c == '\\') mipsEscapedPart += "\\\\"; // escaped for MIPS .asciiz
                    else mipsEscapedPart += c;
                }
                stringLiterals.push_back("\"" + mipsEscapedPart + "\"");
                gen("PRINTF_STR", label, "", "");
            }
        }
        current_spill_slot_offset = saved_spill_offset_for_printf; // Restore spill state
    }
    else if (lookahead().type == "IDENFR" && (lookahead(1).type == "ASSIGN" || lookahead(1).type == "LBRACK"))
    {
        string lvalResult_addr_str_or_reg = LVal();
        string finalLvalTargetForStore = lvalResult_addr_str_or_reg;
        string lvalSpillLocForRegisterAddress;
        bool lvalAddressWasInRegisterAndSpilled = false;
        int stmt_level_spill_offset_backup = current_spill_slot_offset; // Use renamed var

        bool lvalIsRegister = is_temp_register(lvalResult_addr_str_or_reg); // Use helper

        if (lvalIsRegister)
        {
            // Use tempAddr for spill calculation
            lvalSpillLocForRegisterAddress = to_string(tempAddr + current_spill_slot_offset) + "($sp)";
            gen("SW", lvalResult_addr_str_or_reg, "", lvalSpillLocForRegisterAddress);
            lvalAddressWasInRegisterAndSpilled = true;
            current_spill_slot_offset += SPILL_SLOT_SIZE; // Use renamed var
            max_spill_slots_needed_for_function = max(max_spill_slots_needed_for_function, current_spill_slot_offset); // Use renamed var
        }

        match("ASSIGN");
        string rhsReg_val;
        if (lookahead().type == "GETINTTK")
        {
            match("GETINTTK");
            match("LPARENT");
            match("RPARENT");
            rhsReg_val = getNextTempReg();
            gen("GETINT", "", "", rhsReg_val);
        }
        else
        {
            rhsReg_val = Exp();
        }
        if (lvalAddressWasInRegisterAndSpilled)
        {
            string reloadedLvalAddressReg = getNextTempReg();
            gen("LW", reloadedLvalAddressReg, "", lvalSpillLocForRegisterAddress);
            finalLvalTargetForStore = reloadedLvalAddressReg;
        }
        gen("STORE", rhsReg_val, "", finalLvalTargetForStore);
        if (lvalAddressWasInRegisterAndSpilled)
        {
            current_spill_slot_offset = stmt_level_spill_offset_backup; // Use renamed var
        }
        match("SEMICN");
    }
    else
    {
        if (lookahead().type != "SEMICN")
        {
            Exp(); // Exp for side effects
        }
        match("SEMICN");
    }
}

vector<string> ArgList()
{
    vector<string> arg_spill_locations;

    auto calculate_current_spill_address = [&]() -> string { // Use tempAddr
        return to_string(tempAddr + current_spill_slot_offset) + "($sp)";
    };
    string temp_reg_arg = Exp();
    string spill_loc = calculate_current_spill_address();
    gen("SW", temp_reg_arg, "", spill_loc);
    arg_spill_locations.push_back(spill_loc);
    current_spill_slot_offset += SPILL_SLOT_SIZE;
    max_spill_slots_needed_for_function = max(max_spill_slots_needed_for_function, current_spill_slot_offset);

    while (lookahead().type == "COMMA")
    {
        match("COMMA");
        temp_reg_arg = Exp();
        spill_loc = calculate_current_spill_address();
        gen("SW", temp_reg_arg, "", spill_loc);
        arg_spill_locations.push_back(spill_loc);
        current_spill_slot_offset += SPILL_SLOT_SIZE;
        max_spill_slots_needed_for_function = max(max_spill_slots_needed_for_function, current_spill_slot_offset);
    }
    return arg_spill_locations;
}

string FuncCall() {
    string funcName = lookahead().value;
    match("IDENFR");
    match("LPARENT");

    int saved_caller_spill_offset = current_spill_slot_offset;

    vector<string> arg_spill_addrs_on_caller_stack;
    if (lookahead().type != "RPARENT") {
        arg_spill_addrs_on_caller_stack = ArgList();
    }
    match("RPARENT");
    int num_total_args = arg_spill_addrs_on_caller_stack.size();

    // Load first 4 args into $a0-$a3 from their spill locations
    for (int i = 0; i < min(4, num_total_args); ++i) {
        gen("LW", "$a" + to_string(i), "", arg_spill_addrs_on_caller_stack[i]);
    }

    // For args > 4, push them onto the stack.
    // This needs careful offset adjustment if PUSH changes $sp.
    // The reference lexer_phase2(4).cpp has this adjustment.
    int bytes_pushed_for_stack_args = 0;
    for (int i = num_total_args - 1; i >= 4; --i) { // Iterate backwards to push in correct order
        string temp_for_push = getNextTempReg();
        string original_spill_addr_str = arg_spill_addrs_on_caller_stack[i];

        // The following logic is to adjust the load offset if $sp has changed due to prior PUSH ops
        // This is critical if PUSH actually modifies $sp before all stack args are loaded.
        // However, our PUSH op implies 'addiu $sp, $sp, -4; sw reg, 0($sp)'
        // The spill locations in arg_spill_addrs_on_caller_stack are relative to $sp *before* any of these PUSHes.
        // So, we load from the original spill location, then PUSH.
        // The reference compiler's adjustment logic seems more for a scenario where spill locations are calculated
        // differently or PUSH operations are interleaved with calculations differently.
        // Let's try the simpler approach first: LW from original spill, then PUSH.
        // If values are spilled at offsets X, Y, Z from original $sp.
        // LW $t, X($sp) -> PUSH $t  ($sp becomes $sp-4)
        // LW $t, Y($sp) -> PUSH $t  ($sp becomes $sp-4 again from its new value)
        // This is incorrect. The LW for Y needs to be from Y($sp_original) = (Y+4)($sp_current_after_1st_push)
        // The reference logic:
        string adjusted_spill_addr_for_lw_str;
        size_t paren_pos = original_spill_addr_str.find('(');
        if (paren_pos == string::npos || original_spill_addr_str.back() != ')' ||
            original_spill_addr_str.substr(paren_pos) != "($sp)") {
            cerr << "Critical Error: Malformed spill address string '" << original_spill_addr_str << "' in FuncCall." << endl;
            exit(1);
        }
        string offset_part_str = original_spill_addr_str.substr(0, paren_pos);
        int original_offset_val = 0;
        try {
            original_offset_val = stoi(offset_part_str);
        } catch (const std::exception& e) {
            cerr << "Critical Error: Failed to parse offset from spill address string '" << original_spill_addr_str << "' in FuncCall: " << e.what() << endl;
            exit(1);
        }
        // The original_offset_val is from $sp *before* any of these PUSHes.
        // bytes_pushed_for_stack_args is how much $sp has already been decremented by PUSHes *in this loop*.
        // So, the effective address from current $sp is original_offset_val + bytes_pushed_for_stack_args.
        int adjusted_offset_for_lw = original_offset_val + bytes_pushed_for_stack_args;
        adjusted_spill_addr_for_lw_str = to_string(adjusted_offset_for_lw) + "($sp)";


        gen("LW", temp_for_push, "", adjusted_spill_addr_for_lw_str);
        gen("PUSH", temp_for_push, "", ""); // PUSH decrements $sp then stores
        bytes_pushed_for_stack_args += SPILL_SLOT_SIZE; // Track $sp change
    }


    string returnReg = "";
    bool is_void_func = false;
    // Look in global symbol table for function type
    if (!symbolTableStack.empty() && symbolTableStack[0].count(funcName)) {
        if (symbolTableStack[0][funcName].type == "void") {
            is_void_func = true;
        }
    }
    if (!is_void_func) {
      returnReg = getNextTempReg();
    }

    gen("CALL", funcName, "", returnReg);

    // Clean up stack space used for arguments passed on stack (args > 4)
    int num_stack_args_passed = max(0, num_total_args - 4);
    if (num_stack_args_passed > 0) {
        // bytes_pushed_for_stack_args is already the total for these
        gen("ADDIU", "$sp", to_string(bytes_pushed_for_stack_args), "$sp");
    }

    current_spill_slot_offset = saved_caller_spill_offset; // Restore caller's spill state

    return returnReg;
}

// InitVal is for runtime initialization, not const. It uses Exp().
void InitVal()
{
    if (lookahead().type == "LBRACE")
    {
        match("LBRACE");
        if (lookahead().type != "RBRACE")
        {
            InitVal(); // Calls Exp() internally
            while (lookahead().type == "COMMA")
            {
                match("COMMA");
                InitVal(); // Calls Exp() internally
            }
        }
        match("RBRACE");
    }
    else
    {
        Exp(); // Base case is a single expression
    }
}

// === Start of new ConstExp related functions (from lexer_phase2(4).cpp model) ===
string PrimaryExpForConst() {
    if (lookahead().type == "LPARENT") {
        match("LPARENT");
        string val = ConstExp(); 
        match("RPARENT");
        return val;
    } else if (lookahead().type == "INTCON") {
        string num = lookahead().value;
        match("INTCON");
        return num;
    } else if (lookahead().type == "IDENFR") {
        string identName = lookahead().value;
        match("IDENFR");
        int constVal;
        if (getConstValueFromSymbolTable(identName, constVal)) { // Use renamed func
            return to_string(constVal);
        } else {
            cerr << "Error: Identifier '" << identName << "' is not a usable constant or not found in constant expression (PrimaryExpForConst)." << endl;
            exit(1);
        }
    } else {
        cerr << "Error: Invalid token '" << lookahead().type << "' in PrimaryExpForConst: " << lookahead().value << endl;
        exit(1);
    }
}

string UnaryExpForConst_Old() { // Renamed from UnaryExpForConst
    if (lookahead().type == "PLUS" || lookahead().type == "MINU" || lookahead().type == "NOT") {
        string op_type = lookahead().type; 
        match(op_type);
        string operand_val_str = UnaryExpForConst_Old(); 
        if (operand_val_str.empty()) {
             cerr << "Error: Empty value from UnaryExpForConst_Old operand." << endl; exit(1);
        }
        try {
            int num = stoi(operand_val_str);
            if (op_type == "MINU") return to_string(-num);
            else if (op_type == "NOT") return to_string(!num); 
            else return operand_val_str; 
        } catch (const std::exception& e) {
            cerr << "Error parsing number in UnaryExpForConst_Old after op: " << operand_val_str << endl;
            exit(1);
        }
    }
    return PrimaryExpForConst(); 
}

string MulExpForConst_Old() { // Renamed from MulExpForConst
    string left = UnaryExpForConst_Old();
    while (lookahead().type == "MULT" || lookahead().type == "DIV" || lookahead().type == "MOD") {
        string op = lookahead().type;
        match(op);
        string right = UnaryExpForConst_Old();
        if (left.empty() || right.empty()) {
            cerr << "Error: Empty operand in MulExpForConst_Old." << endl; exit(1);
        }
        try {
            int leftVal = stoi(left);
            int rightVal = stoi(right);
            if (op == "MULT") left = to_string(leftVal * rightVal);
            else if (op == "DIV") {
                if (rightVal == 0) { cerr << "Error: Division by zero in constant expression." << endl; exit(1); }
                left = to_string(leftVal / rightVal);
            } else { // MOD
                if (rightVal == 0) { cerr << "Error: Modulo by zero in constant expression." << endl; exit(1); }
                left = to_string(leftVal % rightVal);
            }
        } catch (const std::exception& e) {
            cerr << "Error parsing number in MulExpForConst_Old: " << left << " or " << right << endl;
            exit(1);
        }
    }
    return left;
}

string AddExpForConst_Old() { // Renamed from AddExpForConst
    string left = MulExpForConst_Old();
    while (lookahead().type == "PLUS" || lookahead().type == "MINU") {
        string op = lookahead().type;
        match(op);
        string right = MulExpForConst_Old();
         if (left.empty() || right.empty()) {
            cerr << "Error: Empty operand in AddExpForConst_Old." << endl; exit(1);
        }
        try {
            int leftVal = stoi(left);
            int rightVal = stoi(right);
            left = (op == "PLUS") ? to_string(leftVal + rightVal) : to_string(leftVal - rightVal);
        } catch (const std::exception& e) {
            cerr << "Error parsing number in AddExpForConst_Old: " << left << " or " << right << endl;
            exit(1);
        }
    }
    return left;
}

string RelExpForConst() {
    string left = AddExpForConst_Old();
    while (lookahead().type == "LSS" || lookahead().type == "LEQ" ||
           lookahead().type == "GRE" || lookahead().type == "GEQ") {
        string op = lookahead().type;
        match(op);
        string right = AddExpForConst_Old();
        int lval = stoi(left);
        int rval = stoi(right);
        if (op == "LSS") left = to_string(lval < rval);
        else if (op == "LEQ") left = to_string(lval <= rval);
        else if (op == "GRE") left = to_string(lval > rval);
        else if (op == "GEQ") left = to_string(lval >= rval);
    }
    return left;
}

string EqExpForConst() {
    string left = RelExpForConst();
    while (lookahead().type == "EQL" || lookahead().type == "NEQ") {
        string op = lookahead().type;
        match(op);
        string right = RelExpForConst();
        int lval = stoi(left);
        int rval = stoi(right);
        if (op == "EQL") left = to_string(lval == rval);
        else left = to_string(lval != rval);
    }
    return left;
}

string LAndExpForConst() {
    string left_str = EqExpForConst();
    int lval = stoi(left_str); 

    while (lookahead().type == "AND") {
        match("AND");
        if (!lval) { 
            EqExpForConst(); 
        } else { 
            string right_str = EqExpForConst();
            int rval = stoi(right_str);
            lval = (lval && rval); 
        }
    }
    return to_string(lval); 
}

string LOrExpForConst() {
    string left_str = LAndExpForConst();
    int lval = stoi(left_str); 

    while (lookahead().type == "OR") {
        match("OR");
        if (lval) { 
            LAndExpForConst(); 
        } else { 
            string right_str = LAndExpForConst();
            int rval = stoi(right_str);
            lval = (lval || rval); 
        }
    }
    return to_string(lval); 
}

string ConstExp() { // Top-level const expression parser
    return LOrExpForConst();
}
// === End of new ConstExp related functions ===

vector<string> ConstInitVal() // This is for parsing constant initializers
{
    vector<string> values;
    if (lookahead().type == "LBRACE")
    {
        match("LBRACE");
        if (lookahead().type != "RBRACE")
        {
            values.push_back(ConstExp()); // Use the new ConstExp

            while (lookahead().type == "COMMA")
            {
                match("COMMA");
                values.push_back(ConstExp()); // Use the new ConstExp
            }
        }
        match("RBRACE");
    }
    else
    {
        values.push_back(ConstExp()); // Use the new ConstExp
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
        string sizeStr = ConstExp(); // Use new ConstExp
        try { // From reference
            arraySize = stoi(sizeStr);
        } catch (const std::exception& e) {
            cerr << "Error: Invalid array size in constant expression for '" << varName << "': " << sizeStr << endl;
            exit(1);
        }
        if (arraySize < 0 ) { // From reference
            cerr << "Error: Array size cannot be negative for '" << varName << "'." << endl; exit(1);
        }
        match("RBRACK");
    }
    addSymbol(varName, "int", false, false, arraySize);
    Symbol &sym_ref = symbolTableStack.back()[varName]; // Safe after addSymbol

    if (lookahead().type == "ASSIGN")
    {
        match("ASSIGN");
        if (sym_ref.isGlobal)
        {
            if (arraySize == 1)
            {
                if (lookahead().type == "LBRACE") { // From reference
                    cerr << "错误：全局标量变量初始化 '" << varName << "' 不能使用 '{...}'" << endl;
                    exit(1);
                }
                sym_ref.value = ConstExp(); // Use new ConstExp
            }
            else // Global Array
            {
                 if (lookahead().type != "LBRACE") { // From reference
                    cerr << "错误：全局数组初始化 '" << varName << "' 必须使用 '{...}'" << endl;
                    exit(1);
                }
                match("LBRACE");
                sym_ref.value = "{"; 
                int init_count = 0;
                if (lookahead().type != "RBRACE")
                {
                    string first_val = ConstExp(); // Use new ConstExp
                    sym_ref.value += first_val;
                    init_count++;
                    while (lookahead().type == "COMMA")
                    {
                        match("COMMA");
                        string next_val = ConstExp(); // Use new ConstExp
                        sym_ref.value += "," + next_val;
                        init_count++;
                        if (init_count > arraySize && arraySize > 0) { // Check from reference (allow more if arraySize is 0 initially)
                            cerr << "Error: Too many initializers for global array " << varName << endl;
                            exit(1);
                        }
                    }
                }
                sym_ref.value += "}";
                match("RBRACE");
            }
        }
        else // Local Variable or Array
        {
            if (arraySize == 1 && lookahead().type == "LBRACE") { // From reference
                 cerr << "错误：局部标量变量初始化 '" << varName << "' 不应使用 '{...}' " << endl;
                 exit(1);
            }
            if (arraySize > 1 && lookahead().type == "LBRACE") // Local Array with initializer list
            {
                match("LBRACE");
                int elements_initialized = 0;
                if (lookahead().type != "RBRACE")
                {
                    // Corrected loop structure from reference model
                    string element_val_reg = Exp();
                    // Store element_val_reg at index elements_initialized
                    string baseAddrArr_str = getVariableAddress(varName);
                    string idx_val_reg = getNextTempReg();
                    gen("LI", to_string(elements_initialized), "", idx_val_reg);
                    string offsetBytesReg = getNextTempReg();
                    string four_val_reg = getNextTempReg();
                    gen("LI", "4", "", four_val_reg);
                    gen("MUL", idx_val_reg, four_val_reg, offsetBytesReg);
                    string baseAddrReg_for_calc = getNextTempReg();
                    size_t sp_pos = baseAddrArr_str.find("($sp)");
                    string offset_from_sp_str = baseAddrArr_str.substr(0, sp_pos);
                    gen("ADDIU", "$sp", offset_from_sp_str, baseAddrReg_for_calc);
                    string finalElementAddrReg = getNextTempReg();
                    gen("ADD", baseAddrReg_for_calc, offsetBytesReg, finalElementAddrReg);
                    gen("STORE", element_val_reg, "", finalElementAddrReg);
                    elements_initialized++;

                    while(lookahead().type == "COMMA") {
                        match("COMMA");
                        if (elements_initialized >= arraySize && arraySize > 0) {
                             cerr << "Error: Too many initializers for local array " << varName << endl;
                             exit(1);
                        }
                        element_val_reg = Exp();
                        // Store element_val_reg at index elements_initialized (repeat store logic)
                        idx_val_reg = getNextTempReg(); // Recalculate for new index
                        gen("LI", to_string(elements_initialized), "", idx_val_reg);
                        offsetBytesReg = getNextTempReg(); // Recalculate
                        // four_val_reg already set
                        gen("MUL", idx_val_reg, four_val_reg, offsetBytesReg);
                        // baseAddrReg_for_calc already set if base doesn't change, $sp too
                        finalElementAddrReg = getNextTempReg(); // Recalculate
                        gen("ADD", baseAddrReg_for_calc, offsetBytesReg, finalElementAddrReg);
                        gen("STORE", element_val_reg, "", finalElementAddrReg);
                        elements_initialized++;
                    }
                }
                match("RBRACE");
            }
            else // Local Scalar or (non-standard) local array init with single Exp
            {
                string valReg;
                if (lookahead().type == "GETINTTK")
                {
                    match("GETINTTK");
                    match("LPARENT");
                    match("RPARENT");
                    valReg = getNextTempReg();
                    gen("GETINT", "", "", valReg);
                }
                else
                {
                    valReg = Exp();
                }
                string varAddr = getVariableAddress(varName);
                gen("STORE", valReg, "", varAddr);
            }
        }
    }
    else
    {
        // No initializer
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

void ConstDef() {
    string constName = lookahead().value;
    match("IDENFR");

    int declaredArraySize = 1; 
    bool isArray = false;
    if (lookahead().type == "LBRACK") {
        isArray = true;
        match("LBRACK");
        string sizeStr = ConstExp(); // Use new ConstExp
         try {
            declaredArraySize = stoi(sizeStr);
        } catch (const std::exception& e) {
            cerr << "Error: Invalid array size in constant expression for '" << constName << "': " << sizeStr << endl;
            exit(1);
        }
        if (declaredArraySize < 0) { // From reference
            cerr << "Error: Array size cannot be negative for const '" << constName << "'." << endl; exit(1);
        }
        match("RBRACK");
    }

    match("ASSIGN");
    vector<string> initValues = ConstInitVal(); // Uses new ConstExp

    int finalArraySize;
    if (isArray) {
        finalArraySize = declaredArraySize;
        // Error check from reference
        if (initValues.size() > (size_t)finalArraySize && finalArraySize > 0 ) { // if finalArraySize is 0, it's okay if initValues is also effectively empty for {}
            cerr << "Error: Too many initializers for constant array '" << constName << "' (expected " << finalArraySize << ", got " << initValues.size() << ")." << endl;
            exit(1);
        }
         if (finalArraySize == 0 && !initValues.empty() && !(initValues.size()==1 && initValues[0].empty())) {
             cerr << "Error: Initializers provided for constant array '" << constName << "' declared with size 0." << endl;
             exit(1);
         }

        if (finalArraySize > 0) { 
            while (initValues.size() < (size_t)finalArraySize) {
                initValues.push_back("0");
            }
        }
    } else { // Scalar const
        if (initValues.size() != 1) {
            cerr << "Error: Scalar constant '" << constName << "' must be initialized with a single constant expression." << endl;
            exit(1);
        }
        finalArraySize = 1;
    }

    addSymbol(constName, "int", true, false, finalArraySize);
    Symbol &sym = symbolTableStack.back()[constName];
    sym.constValuesList = initValues; 
    if (finalArraySize == 1 && !initValues.empty()) {
        sym.value = initValues[0]; 
    }

    // Initialize local const array/scalar on stack
    if (!sym.isGlobal && sym.arraySize > 0) { 
        string baseAddrArr_str = getVariableAddress(constName);
        for (size_t k = 0; k < sym.constValuesList.size(); ++k) {
            if (k >= (size_t)sym.arraySize && sym.arraySize > 0) break; 

            string val_to_store_reg = TEMP_REG1; // Use fixed temp for safety here, original used getNextTempReg
            gen("LI", sym.constValuesList[k], "", val_to_store_reg);

            string idx_val_reg = getNextTempReg();
            gen("LI", to_string(k), "", idx_val_reg);

            string offsetBytesReg = getNextTempReg();
            string four_val_reg = getNextTempReg();
            gen("LI", "4", "", four_val_reg);
            gen("MUL", idx_val_reg, four_val_reg, offsetBytesReg);

            string baseAddrReg_for_calc = getNextTempReg();
            size_t sp_pos = baseAddrArr_str.find("($sp)");
            string offset_from_sp_str = baseAddrArr_str.substr(0, sp_pos);
            gen("ADDIU", "$sp", offset_from_sp_str, baseAddrReg_for_calc);

            string finalElementAddrReg = getNextTempReg();
            gen("ADD", baseAddrReg_for_calc, offsetBytesReg, finalElementAddrReg);

            gen("STORE", val_to_store_reg, "", finalElementAddrReg); 
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

void MainFuncDef() {
    match("INTTK");
    match("MAINTK");
    match("LPARENT");
    match("RPARENT");

    string func_name_for_label = "main";
    symbolTableStack[0][func_name_for_label] = Symbol(func_name_for_label, "int", 0, false, true);

    enterScope(true); // true for new function frame
    reset_spill_management(); // Reset spill counts

    string oldEpilogueLabel = currentFunctionEpilogueLabel;
    currentFunctionEpilogueLabel = "L_epilogue_FUNC_main";

    gen("LABEL", "", "", "FUNC_main");
    size_t prologueInsertionPoint = intermediateCode.size();
    intermediateCode.push_back({"NOP_PROLOGUE_PLACEHOLDER", "", "", ""}); // Placeholder

    Block(); // Parse block, tempAddr will be updated

    // Frame size calculation uses tempAddr (max offset of declared vars) and max_spill_slots
    int frameSize = calculateFrameSize(tempAddr, max_spill_slots_needed_for_function);

    vector<Quadruple> prologueQuads;
    prologueQuads.push_back({"ADDIU", "$sp", "-" + to_string(frameSize), "$sp"});
    prologueQuads.push_back({"SW", "$ra", "", to_string(frameSize - 4) + "($sp)"}); // $ra at top of usable frame

    // Insert prologue
    if (prologueInsertionPoint < intermediateCode.size() && intermediateCode[prologueInsertionPoint].op == "NOP_PROLOGUE_PLACEHOLDER") {
        intermediateCode.erase(intermediateCode.begin() + prologueInsertionPoint);
        intermediateCode.insert(intermediateCode.begin() + prologueInsertionPoint,
                                prologueQuads.begin(), prologueQuads.end());
    } else {
         cerr << "Warning: Prologue placeholder issue for main." << endl; // Should not happen
         intermediateCode.insert(intermediateCode.begin() + prologueInsertionPoint, prologueQuads.begin(), prologueQuads.end());
    }

    // Implicit return handling (from reference)
    bool explicitReturnExists = false;
    if (!intermediateCode.empty()) {
        // Search backwards from the end, but stop if we hit the function's own label again (excluding the entry point)
        long func_entry_label_pos = -1;
        for(long k=0; k < (long)intermediateCode.size(); ++k) {
            if(intermediateCode[k].op == "LABEL" && intermediateCode[k].result == "FUNC_main") {
                func_entry_label_pos = k;
                break;
            }
        }

        for (long k_loop = intermediateCode.size() - 1; k_loop >=0 && k_loop > func_entry_label_pos ; --k_loop) {
            const auto& quad = intermediateCode[k_loop];
            if (quad.op == "J" && quad.result == currentFunctionEpilogueLabel) {
                explicitReturnExists = true;
                break;
            }
        }
    }

    if (!explicitReturnExists) {
        bool alreadyJumpingToEpilogue = false;
        if (!intermediateCode.empty()) {
            const auto& last_quad = intermediateCode.back();
            // Check if the last instruction before potential epilogue label is already a jump to it
            string lastMeaningfulOpTarget = "";
            if (last_quad.op == "J") lastMeaningfulOpTarget = last_quad.result;
            else if (last_quad.op == "LABEL" && last_quad.result == currentFunctionEpilogueLabel && intermediateCode.size() > 1) {
                 const auto& second_last_quad = intermediateCode[intermediateCode.size()-2];
                 if (second_last_quad.op == "J") lastMeaningfulOpTarget = second_last_quad.result;
            }
            if (lastMeaningfulOpTarget == currentFunctionEpilogueLabel) {
                alreadyJumpingToEpilogue = true;
            }
        }
        if (!alreadyJumpingToEpilogue) {
            gen("LI", "0", "", "$v0"); // Default return 0 for main
            gen("J", "", "", currentFunctionEpilogueLabel);
        }
    }
    
    gen("LABEL", "", "", currentFunctionEpilogueLabel);
    gen("LW", "$ra", "", to_string(frameSize - 4) + "($sp)");
    gen("ADDIU", "$sp", to_string(frameSize), "$sp");
    gen("JR", "$ra", "", "");

    leaveScope();
    currentFunctionEpilogueLabel = oldEpilogueLabel;
}


void FuncDef() {
    string return_type_str = FuncType();
    string funcName = lookahead().value;
    match("IDENFR");

    if (!symbolTableStack.empty()) {
        symbolTableStack[0][funcName] = Symbol(funcName, return_type_str, 0, false, true);
    } else {
        cerr << "Critical Error: Global scope missing during FuncDef for " << funcName << endl;
        exit(1);
    }

    string oldEpilogueLabel = currentFunctionEpilogueLabel;
    currentFunctionEpilogueLabel = "L_epilogue_FUNC_" + funcName;

    match("LPARENT");
    enterScope(true); // New function frame, reset tempAddr
    reset_spill_management(); // Reset spill counts

    vector<string> paramNamesList;
    if (lookahead().type != "RPARENT") {
        // Simplified param parsing, assuming all are INTTK
        match("INTTK");
        string firstParamName = lookahead().value;
        match("IDENFR");
        paramNamesList.push_back(firstParamName);
        addSymbol(firstParamName, "int", false, false, 1); // Params added to local scope

        while (lookahead().type == "COMMA") {
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
    gen("LABEL", "", "", funcLabelForMIPS);
    size_t prologueInsertionPoint = intermediateCode.size();
    intermediateCode.push_back({"NOP_PROLOGUE_PLACEHOLDER", "", "", ""});

    Block(); // Parse block, tempAddr further updated by local vars

    int frameSize = calculateFrameSize(tempAddr, max_spill_slots_needed_for_function);

    vector<Quadruple> prologueQuads;
    prologueQuads.push_back({"ADDIU", "$sp", "-" + to_string(frameSize), "$sp"});
    prologueQuads.push_back({"SW", "$ra", "", to_string(frameSize - 4) + "($sp)"});

    // Store parameters from $a0-$a3 or caller's stack into their local stack slots
    for (int i = 0; i < (int)paramNamesList.size(); ++i) {
        string paramLocalAddr = getVariableAddress(paramNamesList[i]); // This gives X($sp) relative to new $sp
        if (i < 4) { // First 4 params from $a0-$a3
            prologueQuads.push_back({"SW", "$a" + to_string(i), "", paramLocalAddr});
        } else { // Params > 4 from caller's stack
            // Offset from current $sp to caller's stack where arg was pushed.
            // Caller pushed arg, then jal. $sp for callee is set by prologue.
            // Arg is at frameSize (to get to old $sp) + (i-4)*4 (offset on caller's arg stack area)
            string callerStackArgAddr = to_string(frameSize + (i - 4) * SPILL_SLOT_SIZE) + "($sp)";
            string tempRegForParamLoad = TEMP_REG1; // Use a fixed temp
            prologueQuads.push_back({"LW", tempRegForParamLoad, "", callerStackArgAddr});
            prologueQuads.push_back({"SW", tempRegForParamLoad, "", paramLocalAddr});
        }
    }
    
    if (prologueInsertionPoint < intermediateCode.size() && intermediateCode[prologueInsertionPoint].op == "NOP_PROLOGUE_PLACEHOLDER") {
        intermediateCode.erase(intermediateCode.begin() + prologueInsertionPoint);
        intermediateCode.insert(intermediateCode.begin() + prologueInsertionPoint,
                                prologueQuads.begin(), prologueQuads.end());
    } else {
         cerr << "Warning: Prologue placeholder issue for " << funcName << "." << endl;
         intermediateCode.insert(intermediateCode.begin() + prologueInsertionPoint, prologueQuads.begin(), prologueQuads.end());
    }

    // Implicit return for void functions (from reference)
    bool explicitReturnExists = false;
    if (!intermediateCode.empty()) {
        long func_entry_label_pos = -1;
        for(long k=0; k < (long)intermediateCode.size(); ++k) {
            if(intermediateCode[k].op == "LABEL" && intermediateCode[k].result == funcLabelForMIPS) {
                func_entry_label_pos = k;
                break;
            }
        }
        for (long k_loop = intermediateCode.size() - 1; k_loop >=0 && k_loop > func_entry_label_pos; --k_loop) {
            const auto& quad = intermediateCode[k_loop];
             if (quad.op == "J" && quad.result == currentFunctionEpilogueLabel) {
                explicitReturnExists = true;
                break;
            }
        }
    }

    if (!explicitReturnExists) {
        bool alreadyJumpingToEpilogue = false;
         if (!intermediateCode.empty()) {
            const auto& last_quad = intermediateCode.back();
            string lastMeaningfulOpTarget = "";
            if (last_quad.op == "J") lastMeaningfulOpTarget = last_quad.result;
            else if (last_quad.op == "LABEL" && last_quad.result == currentFunctionEpilogueLabel && intermediateCode.size() > 1) {
                 const auto& second_last_quad = intermediateCode[intermediateCode.size()-2];
                 if (second_last_quad.op == "J") lastMeaningfulOpTarget = second_last_quad.result;
            }

            if (lastMeaningfulOpTarget == currentFunctionEpilogueLabel) {
                alreadyJumpingToEpilogue = true;
            }
        }
        if (!alreadyJumpingToEpilogue) {
            if (return_type_str == "void") {
                gen("J", "", "", currentFunctionEpilogueLabel);
            } else {
                 // For non-void functions, a missing return is a logical error.
                 // To be safe, jump to epilogue. $v0 might contain garbage.
                 // cerr << "Warning: Missing return in non-void function " << funcName << endl;
                 gen("J", "", "", currentFunctionEpilogueLabel);
            }
        }
    }

    gen("LABEL", "", "", currentFunctionEpilogueLabel);
    gen("LW", "$ra", "", to_string(frameSize - 4) + "($sp)");
    gen("ADDIU", "$sp", to_string(frameSize), "$sp");
    gen("JR", "$ra", "", "");

    leaveScope();
    currentFunctionEpilogueLabel = oldEpilogueLabel;
}


void CompUnit()
{
    // Global Decls
    while (lookahead().type == "CONSTTK" ||
           (lookahead().type == "INTTK" && lookahead(1).type == "IDENFR" && lookahead(2).type != "LPARENT"))
    {
        Decl();
    }
    // FuncDefs (non-main)
    while ((lookahead().type == "INTTK" || lookahead().type == "VOIDTK") &&
           lookahead(1).type == "IDENFR" &&
           lookahead(1).value != "main" && 
           lookahead(2).type == "LPARENT")
    {
        FuncDef();
    }
    // MainFuncDef
    if (lookahead().type == "INTTK" && lookahead(1).type == "MAINTK") { // From reference logic
        MainFuncDef();
    } else if (lookahead().type != "EOF") {
        cerr << "Syntax Error: Expected MainFuncDef or EOF after Decls/FuncDefs, got " << lookahead().type
             << " ('" << lookahead().value << "')" << endl;
        exit(1);
    }
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
     else // From reference
    {
        cerr << "Syntax error: expected 'int' or 'void' for function type but got " << lookahead().type << endl;
        exit(1);
    }
    return ""; // Should not reach here
}

string LVal(bool *outIsArrayAccessWithoutIndex) {
    if (outIsArrayAccessWithoutIndex) *outIsArrayAccessWithoutIndex = false;

    string varName = lookahead().value;
    match("IDENFR");

    Symbol sym;
    bool foundSym = false;
    for (auto it = symbolTableStack.rbegin(); it != symbolTableStack.rend(); ++it) {
        const auto &scope = *it;
        if (scope.count(varName)) {
            sym = scope.at(varName);
            foundSym = true;
            break;
        }
    }
    if (!foundSym) {
        cerr << "错误：变量或常量 '" << varName << "' 未声明 (LVal)" << endl;
        exit(1);
    }
    string baseAddrStr = getVariableAddress(varName); // Gives label or X($sp)

    if (lookahead().type == "LBRACK") {
        if (sym.arraySize == 1 && !sym.isFunction) { // From reference
            cerr << "Error: Variable '" << varName << "' is not an array but is being accessed with []." << endl;
            exit(1);
        }
        if (outIsArrayAccessWithoutIndex) *outIsArrayAccessWithoutIndex = false;
        match("LBRACK");
        string idxReg = Exp(); // Value of index
        match("RBRACK");

        string offsetReg = getNextTempReg(); // Will hold byte offset
        string four_val_reg = getNextTempReg();
        gen("LI", "4", "", four_val_reg);
        gen("MUL", idxReg, four_val_reg, offsetReg); // offset_in_bytes = index_value * 4

        string elementAddrReg = getNextTempReg(); // Will hold final address of element
        if (sym.isGlobal) {
            string baseAddrInReg = getNextTempReg();
            gen("LA", sym.name, "", baseAddrInReg);  // Load address of global array base
            gen("ADD", baseAddrInReg, offsetReg, elementAddrReg); // element_addr = base_addr + offset_in_bytes
        } else { // Local array
            string baseAddrActualReg = getNextTempReg(); 
            size_t sp_pos = baseAddrStr.find("($sp)");
            string offset_from_sp_str = (sp_pos != string::npos) ? baseAddrStr.substr(0, sp_pos) : "0"; // Should always be X($sp)
            gen("ADDIU","$sp", offset_from_sp_str, baseAddrActualReg); // baseAddrActualReg = $sp + array_base_stack_offset
            gen("ADD", baseAddrActualReg, offsetReg, elementAddrReg);   // element_addr = base_addr_on_stack + offset_in_bytes
        }
        return elementAddrReg; // This register now holds the *address* of the element
    } else { 
        if (outIsArrayAccessWithoutIndex && sym.arraySize > 1) {
            *outIsArrayAccessWithoutIndex = true; 
        }
        return baseAddrStr; // For scalars: its address. For array name: base address string.
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
        string operand_reg = UnaryExp(); // Recursive call
        string temp_res_reg = getNextTempReg();
        if (op == "MINU")
        {
            gen("NEG", operand_reg, "", temp_res_reg);
        }
        else if (op == "NOT")
        {
            // Assuming operand_reg is 0 for false, non-zero for true.
            // NOT should make 0 -> 1, non-zero -> 0.
            gen("NOT", operand_reg, "", temp_res_reg); // MIPS for NOT will handle this
        }
        else // PLUS
        {
            if (temp_res_reg != operand_reg) gen("MOVE", operand_reg, "", temp_res_reg);
            else return operand_reg; // Optimization
        }
        return temp_res_reg;
    }
    else
    {
        return PrimaryExp();
    }
}

string MulExp() {
    string current_accumulated_value_reg = UnaryExp();

    while (lookahead().type == "MULT" || lookahead().type == "DIV" || lookahead().type == "MOD") {
        string op_token_type = lookahead().type;
        string op_code = (op_token_type == "MULT") ? "MUL"
                       : (op_token_type == "DIV" ? "DIV" : "MOD");
        match(op_token_type);

        string lhs_operand_for_this_op = current_accumulated_value_reg;
        bool did_spill_lhs = false;
        string spill_addr_str_for_lhs;
        int saved_spill_offset_before_lhs_spill = current_spill_slot_offset;

        if (is_temp_register(lhs_operand_for_this_op)) { // Use helper and tempAddr
            spill_addr_str_for_lhs = to_string(tempAddr + current_spill_slot_offset) + "($sp)";
            gen("SW", lhs_operand_for_this_op, "", spill_addr_str_for_lhs);
            did_spill_lhs = true;
            current_spill_slot_offset += SPILL_SLOT_SIZE;
            max_spill_slots_needed_for_function = max(max_spill_slots_needed_for_function, current_spill_slot_offset);
        }

        string rhs_value_reg = UnaryExp();

        string final_lhs_reg_for_gen;
        if (did_spill_lhs) {
            final_lhs_reg_for_gen = getNextTempReg();
            gen("LW", final_lhs_reg_for_gen, "", spill_addr_str_for_lhs);
            current_spill_slot_offset = saved_spill_offset_before_lhs_spill; 
        } else {
            final_lhs_reg_for_gen = lhs_operand_for_this_op;
        }
        string result_of_this_op_reg = getNextTempReg();
        gen(op_code, final_lhs_reg_for_gen, rhs_value_reg, result_of_this_op_reg);
        current_accumulated_value_reg = result_of_this_op_reg;
    }
    return current_accumulated_value_reg;
}


string AddExp() {
    string current_accumulated_value_reg = MulExp();

    while (lookahead().type == "PLUS" || lookahead().type == "MINU") {
        string op_code = (lookahead().type == "PLUS") ? "ADD" : "SUB";
        match(lookahead().type);

        string lhs_operand_for_this_op = current_accumulated_value_reg;
        bool did_spill_lhs = false;
        string spill_addr_str_for_lhs;
        int saved_spill_offset_before_lhs_spill = current_spill_slot_offset;

        if (is_temp_register(lhs_operand_for_this_op)) { // Use helper and tempAddr
            spill_addr_str_for_lhs = to_string(tempAddr + current_spill_slot_offset) + "($sp)";
            gen("SW", lhs_operand_for_this_op, "", spill_addr_str_for_lhs);
            did_spill_lhs = true;
            current_spill_slot_offset += SPILL_SLOT_SIZE;
            max_spill_slots_needed_for_function = max(max_spill_slots_needed_for_function, current_spill_slot_offset);
        }

        string rhs_value_reg = MulExp(); 

        string final_lhs_reg_for_gen;
        if (did_spill_lhs) {
            final_lhs_reg_for_gen = getNextTempReg();
            gen("LW", final_lhs_reg_for_gen, "", spill_addr_str_for_lhs);
            current_spill_slot_offset = saved_spill_offset_before_lhs_spill; 
        } else {
            final_lhs_reg_for_gen = lhs_operand_for_this_op;
        }

        string result_of_this_op_reg = getNextTempReg();
        gen(op_code, final_lhs_reg_for_gen, rhs_value_reg, result_of_this_op_reg);
        current_accumulated_value_reg = result_of_this_op_reg;
    }
    return current_accumulated_value_reg;
}


string Exp()
{
    string result_reg = AddExp(); // AddExp is the entry point for SysY expressions
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
        for (auto it = symbolTableStack.rbegin(); it != symbolTableStack.rend(); ++it) {
            if (it->count(varName)) { 
                sym = it->at(varName);
                foundSym = true;
                break;
            }
        }
        if (!foundSym) { cerr << "Error: Symbol " << varName << " not found in PrimaryExp (after LVal)." << endl; exit(1);}
        
        bool lvalReturnedRegisterHoldingAddress = is_temp_register(lvalResult); // Use helper

        if (lvalReturnedRegisterHoldingAddress) { 
            string tempValueReg = getNextTempReg();
            gen("LOAD", lvalResult, "", tempValueReg); 
            return tempValueReg;
        } else { 
            string tempDestReg = getNextTempReg();
            if (sym.isConst && sym.arraySize == 1) { 
                string const_val_str;
                if (!sym.value.empty()) {
                    const_val_str = sym.value;
                } else if (!sym.constValuesList.empty()) { 
                    const_val_str = sym.constValuesList[0];
                } else {
                    cerr << "Error: Constant scalar " << sym.name << " has no value." << endl;
                    exit(1);
                }
                gen("LI", const_val_str, "", tempDestReg);
            } else if (sym.arraySize > 1 && isArrayAccessWithoutIndex) {
                if (sym.isGlobal) {
                    gen("LA", lvalResult, "", tempDestReg); 
                } else { 
                    size_t sp_pos = lvalResult.find("($sp)");
                    if (sp_pos == string::npos) { // Should not happen for local array base
                        cerr << "Error: Malformed local array address string in PrimaryExp: " << lvalResult << endl;
                        exit(1);
                    }
                    string offset_str = lvalResult.substr(0, sp_pos);
                    gen("ADDIU", "$sp", offset_str, tempDestReg); 
                }
            } else { // Scalar variable, or (less common here) array element whose LVal returned address string
                gen("LOAD", lvalResult, "", tempDestReg); 
            }
            return tempDestReg;
        }
    }
    else if (lookahead().type == "INTCON")
    {
        string num = lookahead().value;
        match("INTCON");
        string temp_reg = getNextTempReg();
        gen("LI", num, "", temp_reg);
        return temp_reg;
    }
    else if (lookahead().type == "GETINTTK") // This is a primary expression that evaluates to a value
    {
        match("GETINTTK");
        match("LPARENT");
        match("RPARENT");
        string temp_reg = getNextTempReg();
        gen("GETINT", "", "", temp_reg); // Result of getint() is in temp_reg
        return temp_reg;
    }
    else // From reference
    {
        cerr << "语法错误：PrimaryExp 遇到意外的 Token '" << lookahead().type << "'" << endl;
        exit(1);
    }
    return ""; // Should not reach
}


void skip_comments(const string &code, size_t &i) { // From reference
    if (i + 1 < code.length() && code[i] == '/' && code[i + 1] == '/') {
        i += 2;
        while (i < code.length() && code[i] != '\n') i++;
    } else if (i + 1 < code.length() && code[i] == '/' && code[i + 1] == '*') {
        size_t comment_start_pos = i;
        i += 2;
        while (i < code.length()) {
            if (i + 1 < code.length() && code[i] == '*' && code[i + 1] == '/') {
                i += 2;
                return;
            }
            i++;
        }
        cerr << "Error: Unterminated block comment starting at position " << comment_start_pos << endl;
        // Potentially exit or handle as error
    }
}

void tokenize(const string &code) { // String literal part from reference
    size_t i = 0, len = code.length();
    while (i < len) {
        if (isspace(code[i])) {
            i++;
            continue;
        }
        if (i + 1 < len && (code.substr(i, 2) == "//" || code.substr(i, 2) == "/*")) {
            skip_comments(code, i);
            continue;
        }
        if (code[i] == '"') {
            string str_val;
            size_t str_start_pos = i;
            i++;
            while (i < len && code[i] != '"') {
                if (code[i] == '\\') {
                    i++;
                    if (i < len) {
                        switch (code[i]) {
                            case 'n': str_val += '\n'; break;
                            case 't': str_val += '\t'; break;
                            case '\\': str_val += '\\'; break;
                            case '"': str_val += '"'; break; // Allow escaped quote inside string
                            default: // Store unrecognized escape sequences as is (e.g., \q -> \q)
                                str_val += '\\'; 
                                str_val += code[i]; 
                                break;
                        }
                    } else {
                        cerr << "Error: Dangling backslash in string literal at end of input." << endl;
                        str_val += '\\'; 
                        break; 
                    }
                } else {
                    str_val += code[i];
                }
                i++;
            }
            if (i < len && code[i] == '"') {
                tokens.push_back({"STRCON", "\"" + str_val + "\""}); 
                i++;
            } else {
                cerr << "Error: Unterminated string literal starting at " << str_start_pos << endl;
                tokens.push_back({"STRCON", "\"" + str_val}); 
            }
            continue;
        }
        if (i + 1 < len) {
            string two_char_op = code.substr(i, 2);
            if (symbols.count(two_char_op)) {
                tokens.push_back({symbols[two_char_op], two_char_op});
                i += 2;
                continue;
            }
        }
        string one_char_op = code.substr(i, 1);
        if (symbols.count(one_char_op)) {
            tokens.push_back({symbols[one_char_op], one_char_op});
            i++;
            continue;
        }
        if (isalpha(code[i]) || code[i] == '_') {
            size_t start = i;
            while (i < len && (isalnum(code[i]) || code[i] == '_')) {
                i++;
            }
            string word = code.substr(start, i - start);
            tokens.push_back({keywords.count(word) ? keywords[word] : "IDENFR", word});
            continue;
        }
        if (isdigit(code[i])) {
            size_t start = i;
            while (i < len && isdigit(code[i])) {
                i++;
            }
            tokens.push_back({"INTCON", code.substr(start, i - start)});
            continue;
        }
        cerr << "Error: Unrecognized character '" << code[i] << "' at position " << i << endl; // From reference
        i++; // Move past unrecognized char to avoid infinite loop
    }
}


// ======================= MIPS生成 =======================
void DataSection(ofstream &mipsFile)
{
    mipsFile << ".data\n";
    if (!symbolTableStack.empty()) // Use renamed global
    {
        for (const auto &symPair : symbolTableStack[0]) // Use renamed global
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
                    // Added from reference for const array size 0 or uninit
                    else if (sym.arraySize > 0 && sym.arraySize != 1) { // arraySize > 0 implies it's not empty {}
                        for (int k=0; k < sym.arraySize; ++k) mipsFile << "0" << (k == sym.arraySize -1 ? "" : ", ");
                    }
                    else // Scalar or error case
                    {
                        mipsFile << (sym.value.empty() ? "0" : sym.value); // Default to 0 if no value (should be caught by parser)
                    }
                    mipsFile << "\n";
                }
                else // Global variable
                {
                    if (sym.arraySize > 1)
                    {
                        // Logic from reference for global array init string (e.g. "{1,2,3}" or empty "{}" )
                        if (!sym.value.empty() && sym.value.front() == '{' && sym.value.back() == '}') {
                            string innerValues = sym.value.substr(1, sym.value.length() - 2);
                            if (innerValues.empty() && sym.arraySize > 0) { // {} initializer for sized array
                                mipsFile << sym.name << ": .space " << sym.arraySize * 4 << "\n";
                            } else if (!innerValues.empty()) { // {v1, v2...}
                                stringstream ss(innerValues);
                                string item;
                                vector<string> initializers;
                                while (getline(ss, item, ',')) {
                                    item.erase(0, item.find_first_not_of(" \t\n\r\f\v"));
                                    item.erase(item.find_last_not_of(" \t\n\r\f\v") + 1);
                                    initializers.push_back(item);
                                }
                                mipsFile << sym.name << ": .word ";
                                for (size_t k = 0; k < initializers.size(); ++k) {
                                    mipsFile << initializers[k] << (k == initializers.size() - 1 ? "" : ", ");
                                }
                                // Pad with zeros if fewer initializers than array size
                                for (int k_init = initializers.size(); k_init < sym.arraySize; ++k_init) {
                                     if (!initializers.empty() || k_init > initializers.size()) mipsFile << ", ";
                                     else if (k_init > 0 && initializers.empty()) mipsFile << ", "; // Handles cases like .word ,0,0 for {,,} which is invalid
                                     else if (k_init==0 && initializers.empty() && sym.arraySize > 0) {} // No comma for first of several 0s if list was empty
                                     else if (k_init > 0 ) mipsFile << ", "; // Default comma for subsequent elements
                                    mipsFile << "0";
                                }
                                mipsFile << "\n";
                            } else { // No value string, or not {} form, but arraySize > 1
                                 mipsFile << sym.name << ": .space " << sym.arraySize * 4 << "\n";
                            }
                        } else { // No initializer string, or not in {..} format
                             mipsFile << sym.name << ": .space " << sym.arraySize * 4 << "\n";
                        }
                    }
                    else // Global scalar
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
    mipsFile << ".globl main\n"; // Standard MIPS global declaration
    mipsFile << "main:\n";       // Entry point for MARS execution if .globl main is used
    mipsFile << "    li   $sp, 0x7ffffffc\n"; // Initialize stack pointer
    mipsFile << "    jal  FUNC_main\n";       // Jump to the actual main function
    mipsFile << "    li   $v0, 10\n";         // Exit syscall code
    mipsFile << "    syscall\n";             // Exit

    for (const auto &quad : intermediateCode) // Use renamed global
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
        else if (quad.op == "SYSCALL") // Generic syscall, if used (not typical for this subset)
        {
            mipsFile << "syscall\n";
        }
        else if (quad.op == "LOAD") // lw wrapper
        {
            // Logic from reference file's MIPS gen for LOAD
            if (!quad.arg1.empty() && quad.arg1[0] == '$') { // arg1 is a register holding an address
                mipsFile << "lw " << quad.result << ", 0(" << quad.arg1 << ")\n";
            } else if (quad.arg1.find("($sp)") == string::npos && !isdigit(quad.arg1[0]) && quad.arg1.find("str") != 0 && quad.arg1.find("L_") != 0) {
                // arg1 is a global data label (heuristic check)
                bool isDataLabel = false;
                if(!symbolTableStack.empty() && symbolTableStack[0].count(quad.arg1)) {
                    isDataLabel = !symbolTableStack[0][quad.arg1].isFunction;
                }
                if(isDataLabel) {
                     mipsFile << "lw " << quad.result << ", " << quad.arg1 << "\n"; // MARS handles `lw $reg, label`
                } else { // Fallback or other label types (should ideally not happen for LOAD from non-address)
                     mipsFile << "lw " << quad.result << ", " << quad.arg1 << "\n";
                }
            } else { // arg1 is stack address like "offset($sp)"
                mipsFile << "lw " << quad.result << ", " << quad.arg1 << "\n";
            }
        }
        else if (quad.op == "LW") // Direct lw
        {
            mipsFile << "lw " << quad.arg1 << ", " << quad.result << "\n"; // Note: arg1 is dest, result is src for LW quad
        }
        else if (quad.op == "STORE") // sw wrapper
        {
            // Logic from reference file's MIPS gen for STORE
             if (!quad.result.empty() && quad.result[0] == '$') { // result is a register holding an address
                mipsFile << "sw " << quad.arg1 << ", 0(" << quad.result << ")\n";
            } else if (quad.result.find("($sp)") == string::npos && !isdigit(quad.result[0]) && quad.result.find("str") != 0 && quad.result.find("L_") != 0) {
                // result is a global data label
                 mipsFile << "sw " << quad.arg1 << ", " << quad.result << "\n"; // MARS handles `sw $reg, label`
            } else { // result is stack address like "offset($sp)"
                mipsFile << "sw " << quad.arg1 << ", " << quad.result << "\n";
            }
        }
        else if (quad.op == "SW") // Direct sw
        {
            mipsFile << "sw " << quad.arg1 << ", " << quad.result << "\n"; // arg1 is src, result is dest for SW quad
        }
        else if (quad.op == "ADD")
        {
            mipsFile << "add " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "SUB")
        {
            mipsFile << "sub " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "MUL") // Use pseudo-instruction
        {
            mipsFile << "mul " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "DIV") // Use pseudo-instruction
        {
            mipsFile << "div " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "MOD") // Use pseudo-instruction
        {
            mipsFile << "rem " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "NEG") // Use pseudo-instruction
        {
            mipsFile << "neg " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "POS") // This was 'add $res, $arg1, $zero'. Effectively a move.
        {
            if (quad.result != quad.arg1) mipsFile << "move " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "ASSIGN") // LI then SW (original 111.cpp was a bit complex here)
        {
            // Assuming arg2 is immediate. If arg2 can be register, need move.
            // This ASSIGN op might be redundant if LI + STORE is used by parser.
            mipsFile << "li " << TEMP_REG1 << ", " << quad.arg2 << "\n"; // Load immediate into temp
            // Store logic similar to STORE op
            if (!quad.result.empty() && quad.result[0] == '$') {
                 mipsFile << "sw " << TEMP_REG1 << ", 0(" << quad.result << ")\n";
            } else if (quad.result.find("($sp)") == string::npos && !isdigit(quad.result[0])) {
                mipsFile << "sw " << TEMP_REG1 << ", " << quad.result << "\n"; // Global label
            } else {
                mipsFile << "sw " << TEMP_REG1 << ", " << quad.result << "\n"; // Stack address
            }
        }
        else if (quad.op == "IMM") // This is same as LI
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
            mipsFile << "jal FUNC_" << quad.arg1 << "\n"; // Assumes func name in arg1, not full label
            if (!quad.result.empty()) // If non-void function, move result from $v0
            {
                mipsFile << "move " << quad.result << ", $v0\n";
            }
        }
        else if (quad.op == "AND") // Bitwise AND (not used by LAndExp anymore if it uses short-circuit jumps)
        {
            mipsFile << "and " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "OR") // Bitwise OR (not used by LOrExp anymore)
        {
            mipsFile << "or " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "NOT") // Logical NOT (0 -> 1, non-zero -> 0)
        {
            mipsFile << "seq " << quad.result << ", " << quad.arg1 << ", $zero\n"; // Aligned with reference
        }
        else if (quad.op == "LA")
        {
            mipsFile << "la " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "ADDIU")
        {
            mipsFile << "addiu " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        // Relational operators using pseudo-instructions (aligned with reference)
        else if (quad.op == "LSS") // <
        {
            mipsFile << "slt " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << endl;
        }
        else if (quad.op == "LEQ") // <=
        {
             mipsFile << "sle " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "GRE") // >
        {
            mipsFile << "sgt " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << endl;
        }
        else if (quad.op == "GEQ") // >=
        {
             mipsFile << "sge " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "EQL") // ==
        {
            mipsFile << "seq " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else if (quad.op == "NEQ") // !=
        {
            mipsFile << "sne " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        // Branch instructions
        else if (quad.op == "BEQ")
        {
            mipsFile << "beq " << quad.arg1 << ", " << quad.arg2 << ", " << quad.result << "\n";
        }
        else if (quad.op == "BEQZ" || quad.op == "BNEZ")
        {
            mipsFile << (quad.op == "BEQZ" ? "beqz " : "bnez ")
                     << quad.arg1 << ", " << quad.result << "\n";
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
        else if (quad.op == "FUNC_DEF") // This was used in 111.cpp, but LABEL FUNC_Name is more standard
        {
            mipsFile << quad.arg1 << ":\n"; // Assuming arg1 is the full label FUNC_Name
        }
        else if (quad.op == "LI")
        {
            mipsFile << "li " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "JR")
        {
            mipsFile << "jr " << quad.arg1 << "\n";
        }
         else
        {
            cerr << "错误：未支持的中间代码操作类型 '" << quad.op << "'" << endl;
        }
    }
}

void MIPS() // Wrapper for MIPS generation
{
    ofstream mipsFile("mips.txt");
    if (!mipsFile.is_open()) {
        cerr << "Error: Could not open mips.txt for writing." << endl;
        return;
    }
    DataSection(mipsFile);
    TextSection(mipsFile);
    mipsFile.close(); // Explicitly close
}

int main()
{
    ifstream infile("testfile.txt");
    if (!infile.is_open()) { // Check if file opened
        cerr << "Error: Could not open testfile.txt" << endl;
        return 1;
    }
    stringstream buffer;
    buffer << infile.rdbuf();
    infile.close();

    enterScope(true); // Global scope, also acts as a "function frame" for global initializers if any were dynamic
    tokenize(buffer.str());
    CompUnit();
    MIPS(); // Generate MIPS code

    // Check if mips.txt was created (optional, MIPS() handles its own error)
    ifstream mips_check("mips.txt");
    if (!mips_check.good()) {
        cerr << "Error: mips.txt was not generated or is not accessible after MIPS()." << endl;
        // return 1; // Don't return here if MIPS() already reported error
    }
    mips_check.close();
    
    // The system call to MARS is usually done outside the compiler itself,
    // but if it was part of the original 111.cpp requirement, it can be kept.
    // For now, let's assume it's separate.
    // system("java -jar mars.jar nc mips.txt < input.txt > output.txt"); // Example from reference

    return 0;
}