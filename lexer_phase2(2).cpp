#include <iostream>
#include <fstream>
#include <cctype>
#include <unordered_map>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm> // Required for std::max

using namespace std;
int labelCount = 0;
unordered_map<string, string> keywords = {
    {"const", "CONSTTK"}, {"int", "INTTK"}, {"break", "BREAKTK"}, {"continue", "CONTINUETK"}, {"if", "IFTK"}, {"else", "ELSETK"}, {"while", "WHILETK"}, {"return", "RETURNTK"}, {"void", "VOIDTK"}, {"main", "MAINTK"}, {"getint", "GETINTTK"}, {"printf", "PRINTFTK"}};

unordered_map<string, string> symbols = {
    {"!=", "NEQ"}, {"!", "NOT"}, {"&&", "AND"}, {"||", "OR"}, {"+", "PLUS"}, {"-", "MINU"}, {"*", "MULT"}, {"/", "DIV"}, {"%", "MOD"}, {"<", "LSS"}, {"<=", "LEQ"}, {">", "GRE"}, {">=", "GEQ"}, {"==", "EQL"}, {"=", "ASSIGN"}, {";", "SEMICN"}, {",", "COMMA"}, {"(", "LPARENT"}, {")", "RPARENT"}, {"[", "LBRACK"}, {"]", "RBRACK"}, {"{", "LBRACE"}, {"}", "RBRACE"}};

struct Token
{
    string type;
    string value;
};
size_t pos = 0;
string currentFunctionEpilogueLabel = "";
vector<Token> tokens;
vector<string> loopStartLabels; // Stores labels for 'continue' targets
vector<string> loopEndLabels;   // Stores labels for 'break' targets
void CompUnit();                  // 解析 <CompUnit>
void MainFuncDef();               // 解析 <MainFuncDef>
void Block();                     // 解析 <Block>
void Stmt();                      // 解析 <Stmt>
string Exp();                     // 解析 <Exp>
string AddExp();                  // 解析 <AddExp>
string MulExp();                  // 解析 <MulExp>
string UnaryExp();                // 解析 <UnaryExp>
string PrimaryExp();              // 解析 <PrimaryExp>
string LVal(bool* outIsArrayAccessWithoutIndex = nullptr); 
void VarDecl();                   // 解析 <VarDecl>
void VarDef();                    // 解析 <VarDef>
string FuncCall();                  // 解析函数调用
void match(string expectedType);  // 匹配并消耗一个特定类型的Token
// bool isStmt();                    // 判断当前Token是否为 <Stmt> 的起始 // Unused
// void advance();                   // 读取下一个Token // Unused
// void outputSyntax(string syntax); // 输出语法成分 // Unused
struct Quadruple
{
    string op;
    string arg1;
    string arg2;
    string result;
};
vector<Quadruple> intermediateCode;

// In your struct Symbol definition
struct Symbol
{
    string name;
    string type; // For functions, stores "int" or "void" (return type)
    int addr; // 内存地址偏移量
    bool isGlobal = true;
    bool isConst = false;    // 是否为常量
    bool isFunction = false; // 新增标识
    int arraySize = 1;
    string value = ""; // For single const value, or scalar global var init, or string like "{v1,v2}" for global array init
    vector<string> constValuesList; // NEW: For const array initializers (and can store scalar const too)

    Symbol(const string &n, const string &t, int a, bool c, bool b) : name(n), type(t), addr(a), isConst(c), isFunction(b) {}
    Symbol()
    {
        name = "";
        type = "";
        addr = 0;
        arraySize = 1; // Default
        isGlobal = true;
        isConst = false;
        isFunction = false;
    }
};
vector<unordered_map<string, Symbol>> symbolTableStack;
int tempAddr = 0;
int strCount = 0;
vector<string> stringLiterals;

static int current_spill_slot_offset = 0; 
static int max_spill_slots_needed_for_function = 0; 
const int SPILL_SLOT_SIZE = 4; 

const string MIPS_TEMP_REG_1 = "$t8"; 
const string MIPS_TEMP_REG_2 = "$t9"; 


void reset_spill_management() {
    current_spill_slot_offset = 0;
    max_spill_slots_needed_for_function = 0;
}

void enterScope(bool isGlobal = false)
{
    symbolTableStack.emplace_back();
    if (!isGlobal)
        tempAddr = 0;
}

void leaveScope()
{
    if (!symbolTableStack.empty())
    {
        symbolTableStack.pop_back();
    }
}

int calculateFrameSize(const unordered_map<string, Symbol> &symbols_in_scope, int spill_bytes_needed) {
    int maxLocalVarParamAddr = 0; 
    for (const auto &pair_map_item : symbols_in_scope) { // Renamed pair to avoid conflict if used below
        const Symbol &sym = pair_map_item.second;
        if (!sym.isGlobal && !sym.isFunction) { 
            int endAddr = sym.addr + (sym.arraySize * 4); 
            if (endAddr > maxLocalVarParamAddr) {
                maxLocalVarParamAddr = endAddr;
            }
        }
    }
    int totalVariableSpace = maxLocalVarParamAddr + spill_bytes_needed; 
    int frameSize = (totalVariableSpace + 4 + 7) & ~7; // +4 for $ra, aligned to 8 bytes

    return frameSize;
}

void addSymbol(const string &name, const string &type, bool isConst, bool isFunction, int arraySize = 1) {
    Symbol sym;
    sym.name = name;
    sym.type = type;
    sym.addr = tempAddr; 
    sym.isConst = isConst;
    sym.isFunction = isFunction;
    sym.arraySize = arraySize;

    if (symbolTableStack.size() > 1) { 
        sym.isGlobal = false;
    } else {
        sym.isGlobal = true; 
    }

    symbolTableStack.back()[name] = sym;
    if(!sym.isGlobal && !isFunction) { 
      tempAddr += arraySize * 4;
    }
}
bool getConstValueFromSymbolTable(const string& identName, int& outValue) {
    for (auto it_s = symbolTableStack.rbegin(); it_s != symbolTableStack.rend(); ++it_s) {
        if (it_s->count(identName)) {
            const Symbol& sym = it_s->at(identName);
            if (sym.isConst) {
                if (sym.arraySize > 1) {
                    cerr << "Error: Array constant '" << identName << "' cannot be used directly as a scalar in a constant expression." << endl;
                    return false;
                }
                string valToParse;
                if (!sym.value.empty()) {
                    valToParse = sym.value;
                } else if (!sym.constValuesList.empty()) {
                    valToParse = sym.constValuesList[0]; 
                } else {
                    cerr << "Error: Constant '" << identName << "' has no value." << endl;
                    return false;
                }

                try {
                    outValue = stoi(valToParse);
                    return true;
                } catch (const std::exception& e) {
                    cerr << "Error: Invalid numeric value for constant '" << identName << "': " << valToParse << endl;
                    return false;
                }
            } else {
                return false;
            }
        }
    }
    return false;
}
string getVariableAddress(const string &varName) {
    for (auto it = symbolTableStack.rbegin(); it != symbolTableStack.rend(); ++it) {
        const auto& scope = *it;
        if (scope.count(varName)) { 
            const Symbol &sym = scope.at(varName); 
            if (sym.isGlobal) {
                return sym.name; 
            } else {
                return to_string(sym.addr) + "($sp)"; 
            }
        }
    }
    cerr << "错误：变量 '" << varName << "' 未声明 (getVariableAddress)" << endl;
    exit(1);
}

void gen(const string &op, const string &arg1_param, const string &arg2_param, const string &result_param)
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
        if (name.find("($sp)") != string::npos) { 
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
        if (!symbolTableStack.empty() && symbolTableStack[0].count(name)) {
             const Symbol& sym = symbolTableStack[0].at(name);
             if (sym.isGlobal && !sym.isFunction) return name;
        }
        if (name.length() >= 1 && name[0] == 't' && (name.length() == 1 || isdigit(name[1]))) { 
            bool all_digits_after_t = true;
            for(size_t k=1; k < name.length(); ++k) {
                if (!isdigit(name[k])) {
                    all_digits_after_t = false;
                    break;
                }
            }
            if (all_digits_after_t) return "$" + name; 
        }
        for (auto it_s = symbolTableStack.rbegin(); it_s != symbolTableStack.rend(); ++it_s) {
            if (it_s->count(name)) {
                const Symbol& sym_s = it_s->at(name);
                if (!sym_s.isGlobal && !sym_s.isFunction) { 
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
        intermediateCode.push_back({"GETINT", "", "", result_mapped});
    }
    else if (op == "LABEL")
    {
        intermediateCode.push_back({op, "", "", result_mapped}); 
    }
    else if (op == "NOP_PROLOGUE_PLACEHOLDER") { 
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
string getNextTempReg()
{
    static int tempCount = 0;
    string reg = "t" + to_string(tempCount % 8); 
    tempCount++;
    return reg; 
}
void generateMIPS()
{
    ofstream mipsFile("mips.txt");
    mipsFile << ".data\n";
    if (!symbolTableStack.empty()) {
        auto &globalSymbols = symbolTableStack[0];
        for (const auto &symPair : globalSymbols) {
            const Symbol &sym = symPair.second;
            if (sym.isFunction) continue;
            if (sym.isGlobal) { 
                if (sym.isConst) { 
                    mipsFile << sym.name << ": .word ";
                    if (!sym.constValuesList.empty()) {
                        for (size_t k = 0; k < sym.constValuesList.size(); ++k) {
                            mipsFile << sym.constValuesList[k] << (k == sym.constValuesList.size() - 1 ? "" : ", ");
                        }
                    } else if (sym.arraySize > 0 && sym.arraySize != 1) { 
                        for (int k=0; k < sym.arraySize; ++k) mipsFile << "0" << (k == sym.arraySize -1 ? "" : ", ");
                    } else { 
                        mipsFile << (sym.value.empty() ? "0" : sym.value); 
                    }
                    mipsFile << "\n";
                } else { 
                    if (sym.arraySize > 1) {
                        if (!sym.value.empty() && sym.value.front() == '{' && sym.value.back() == '}') {
                            string innerValues = sym.value.substr(1, sym.value.length() - 2);
                            if (innerValues.empty() && sym.arraySize > 0) { 
                                mipsFile << sym.name << ": .space " << sym.arraySize * 4 << "\n"; 
                            } else if (!innerValues.empty()) {
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
                                for (int k_init = initializers.size(); k_init < sym.arraySize; ++k_init) { // Renamed k
                                    if (!initializers.empty() || k_init > initializers.size()) mipsFile << ", ";
                                    else if (k_init > 0 && initializers.empty()) mipsFile << ", "; 
                                    else if (k_init==0 && initializers.empty() && sym.arraySize > 0) {} 
                                    else if (k_init>0) mipsFile << ", ";
                                    mipsFile << "0";
                                }
                                mipsFile << "\n";
                            } else { 
                                mipsFile << sym.name << ": .space " << sym.arraySize * 4 << "\n";
                            }
                        } else { 
                             mipsFile << sym.name << ": .space " << sym.arraySize * 4 << "\n";
                        }
                    } else {
                        string initVal = sym.value.empty() ? "0" : sym.value;
                        mipsFile << sym.name << ": .word " << initVal << "\n";
                    }
                }
            }
        }
    }
    for (size_t i = 0; i < stringLiterals.size(); ++i) {
        mipsFile << "str" << i << ": .asciiz " << stringLiterals[i] << "\n";
    }
    mipsFile << ".text\n";
    mipsFile << ".globl main\n";
    mipsFile << "main:\n"; 
    mipsFile << "    li   $sp, 0x7ffffffc\n";  
    mipsFile << "    jal  FUNC_main\n";         
    mipsFile << "    li   $v0, 10\n";           
    mipsFile << "    syscall\n";
    for (const auto &quad : intermediateCode)
    {
        if (quad.op == "NOP_PROLOGUE_PLACEHOLDER") { 
            continue;
        }
        else if (quad.op == "LSS") 
        {
            mipsFile << "slt " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << endl;
        }
        else if (quad.op == "LEQ") { 
            mipsFile << "slt " << MIPS_TEMP_REG_1 << ", " << quad.arg1 << ", " << quad.arg2 << "\n"; 
            mipsFile << "seq " << MIPS_TEMP_REG_2 << ", " << quad.arg1 << ", " << quad.arg2 << "\n"; 
            mipsFile << "or "  << quad.result << ", " << MIPS_TEMP_REG_1 << ", " << MIPS_TEMP_REG_2 << "\n";
        }
        else if (quad.op == "GRE") 
        {
            mipsFile << "slt " << quad.result << ", " << quad.arg2 << ", " << quad.arg1 << endl;
        }
        else if (quad.op == "GEQ") { 
            mipsFile << "slt " << MIPS_TEMP_REG_1 << ", " << quad.arg2 << ", " << quad.arg1 << "\n"; 
            mipsFile << "seq " << MIPS_TEMP_REG_2 << ", " << quad.arg2 << ", " << quad.arg1 << "\n"; 
            mipsFile << "or "  << quad.result << ", " << MIPS_TEMP_REG_1 << ", " << MIPS_TEMP_REG_2 << "\n";
        }
        else if (quad.op == "EQL") { 
            mipsFile << "xor " << MIPS_TEMP_REG_1 << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
            mipsFile << "sltiu " << quad.result << ", " << MIPS_TEMP_REG_1 << ", 1\n"; 
        }
        else if (quad.op == "NEQ") { 
            mipsFile << "xor " << MIPS_TEMP_REG_1 << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
            mipsFile << "sltu " << quad.result << ", $zero, " << MIPS_TEMP_REG_1 << "\n"; 
        }
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
        else if (quad.op == "FUNC_DEF") 
        {
            mipsFile << quad.arg1 << ":\n"; 
        }
        else if (quad.op == "LI") 
        {
            mipsFile << "li " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "JR") { 
            mipsFile << "jr " << quad.arg1 << "\n";
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
        else if (quad.op == "LOAD") { 
            if (!quad.arg1.empty() && quad.arg1[0] == '$') { 
                mipsFile << "lw " << quad.result << ", 0(" << quad.arg1 << ")\n";
            } else if (quad.arg1.find("($sp)") == string::npos && !isdigit(quad.arg1[0]) && quad.arg1.find("str") != 0 && quad.arg1.find("L_") != 0) {
                bool isDataLabel = false;
                if(!symbolTableStack.empty() && symbolTableStack[0].count(quad.arg1)) {
                    isDataLabel = !symbolTableStack[0][quad.arg1].isFunction;
                }
                if(isDataLabel) { 
                    string addrReg = "$at"; 
                    mipsFile << "la " << addrReg << ", " << quad.arg1 << "\n";
                    mipsFile << "lw " << quad.result << ", 0(" << addrReg << ")\n";
                } else { 
                     mipsFile << "lw " << quad.result << ", " << quad.arg1 << "\n";
                }
            } else { 
                mipsFile << "lw " << quad.result << ", " << quad.arg1 << "\n";
            }
        }
        else if (quad.op == "LW"){ 
            mipsFile << "lw " << quad.arg1 << ", " << quad.result << "\n";
        }
        else if (quad.op == "STORE") { 
            if (!quad.result.empty() && quad.result[0] == '$') { 
                mipsFile << "sw " << quad.arg1 << ", 0(" << quad.result << ")\n";
            } else if (quad.result.find("($sp)") == string::npos && !isdigit(quad.result[0]) && quad.result.find("str") != 0 && quad.result.find("L_") != 0) {
                string addrReg = "$at"; 
                mipsFile << "la " << addrReg << ", " << quad.result << "\n";       
                mipsFile << "sw " << quad.arg1 << ", 0(" << addrReg << ")\n";       
            } else { 
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
        else if (quad.op == "DIV") { 
            mipsFile << "div " << quad.arg1 << ", " << quad.arg2 << "\n";
            mipsFile << "mflo " << quad.result << "\n"; 
        }
        else if (quad.op == "MOD") { 
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
            string temp_assign_reg = MIPS_TEMP_REG_1; 
            mipsFile << "li " << temp_assign_reg << ", " << quad.arg2 << "\n";
            if (!quad.result.empty() && quad.result[0] == '$') { 
                 mipsFile << "sw " << temp_assign_reg << ", 0(" << quad.result << ")\n";
            } else if (quad.result.find("($sp)") == string::npos && !isdigit(quad.result[0])) { 
                string addrReg = "$at"; 
                mipsFile << "la " << addrReg << ", " << quad.result << "\n";       
                mipsFile << "sw " << temp_assign_reg << ", 0(" << addrReg << ")\n";       
            } else { 
                mipsFile << "sw " << temp_assign_reg << ", " << quad.result << "\n";
            }
        }
        else if (quad.op == "IMM") 
        {
            mipsFile << "li " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "MOVE") { 
            mipsFile << "move " << quad.result << ", " << quad.arg1 << "\n";
        }
        else if (quad.op == "PUSH") 
        {
            mipsFile << "addiu $sp, $sp, -4\n";
            mipsFile << "sw " << quad.arg1 << ", 0($sp)\n";
        }
        else if (quad.op == "CALL") { 
            mipsFile << "jal FUNC_" << quad.arg1 << "\n";
            if (!quad.result.empty()) { 
                 mipsFile << "move " << quad.result << ", $v0\n";
            }
        }
        else if (quad.op == "AND") { 
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
        else if (quad.op == "OR") { 
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
        else if (quad.op == "ADDIU") { 
           mipsFile << "addiu " << quad.result << ", " << quad.arg1 << ", " << quad.arg2 << "\n";
        }
        else
        {
            cerr << "错误：未支持的中间代码操作类型 '" << quad.op << "'" << endl;
        }
    }
}

Token lookahead()
{
    return (pos < tokens.size()) ? tokens[pos] : Token{"EOF", ""};
}
Token lookahead(int offset) 
{
    if (pos + offset < tokens.size() && pos + offset >= 0) 
        return tokens[pos + offset];
    else
        return Token{"EOF", ""}; 
}


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
    string left = AddExp();
    while (lookahead().type == "LSS" || lookahead().type == "GRE" || 
           lookahead().type == "LEQ" || lookahead().type == "GEQ") {
        string op_type = lookahead().type; 
        match(op_type);
        string right = AddExp();
        string temp_res = getNextTempReg();
        gen(op_type, left, right, temp_res); 
        left = temp_res; 
    }
    return left;
}
string EqExp() {
    string left = RelExp(); 
    while (lookahead().type == "EQL" || lookahead().type == "NEQ") {
        string op_type = lookahead().type; 
        match(op_type);
        string right = RelExp(); 
        string temp_res = getNextTempReg();
        gen(op_type, left, right, temp_res); 
        left = temp_res; 
    }
    return left;
}
string LAndExp() {
    string left = EqExp(); 
    while (lookahead().type == "AND") { 
        match("AND");
        string right = EqExp(); 
        string temp = getNextTempReg();
        gen("AND", left, right, temp); 
        left = temp;
    }
    return left;
}

string LOrExp() {
    string left = LAndExp(); 
    while (lookahead().type == "OR") { 
        match("OR");
        string right = LAndExp(); 
        string temp = getNextTempReg();
        gen("OR", left, right, temp); 
        left = temp;
    }
    return left;
}

string Cond() {
    string condReg = LOrExp(); 
    return condReg; 
}
vector<pair<string, bool>> parseFormatString(const string &fmt) {
    vector<pair<string, bool>> parts;
    string processedPart;
    for (size_t i = 0; i < fmt.size(); ++i) {
        if (fmt[i] == '\\' && i + 1 < fmt.size()) {
            switch (fmt[i + 1]) {
                case 'n': processedPart += '\n'; break;  
                case 't': processedPart += '\t'; break;  
                case '\\': processedPart += '\\'; break; 
                default: processedPart += fmt[i + 1];    
            }
            i++; 
        } else if (fmt[i] == '%' && i + 1 < fmt.size() && fmt[i + 1] == 'd') {
            if (!processedPart.empty()) {
                parts.emplace_back(processedPart, false);
                processedPart.clear();
            }
            parts.emplace_back("%d", true); 
            i++; 
        } else {
            processedPart += fmt[i];
        }
    }
    if (!processedPart.empty()) {
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
    else if (lookahead().type == "IFTK") {
        match("IFTK");
        match("LPARENT");
        string condReg = Cond();
        match("RPARENT");
        string falsePathLabel = "L_if_false_path_" + to_string(labelCount++);
        gen("BEQZ", condReg, "", falsePathLabel); 
        Stmt(); 
        if (lookahead().type == "ELSETK") {
            string endOfIfElseLabel = "L_endifelse_" + to_string(labelCount++);
            gen("J", "", "", endOfIfElseLabel); 
            gen("LABEL", "", "", falsePathLabel); 
            match("ELSETK"); 
            Stmt();
            gen("LABEL", "", "", endOfIfElseLabel); 
        } else {
            gen("LABEL", "", "", falsePathLabel); 
        }
    }
    else if (lookahead().type == "WHILETK") {
        match("WHILETK");
        string startLabel = "L_while_" + to_string(labelCount++); 
        string endLabel = "L_endwhile_" + to_string(labelCount++); 

        loopStartLabels.push_back(startLabel);
        loopEndLabels.push_back(endLabel);

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
        if (loopEndLabels.empty()) {
            cerr << "Syntax error: 'break' not in a loop." << endl;
            exit(1);
        }
        gen("J", "", "", loopEndLabels.back()); 
        match("SEMICN");
    }
    else if (lookahead().type == "CONTINUETK")
    {
        match("CONTINUETK");
        if (loopStartLabels.empty()) {
            cerr << "Syntax error: 'continue' not in a loop." << endl;
            exit(1);
        }
        gen("J", "", "", loopStartLabels.back()); 
        match("SEMICN");
    }
    else if (lookahead().type == "RETURNTK") {
        match("RETURNTK");
        if (lookahead().type != "SEMICN") {
            string retReg = Exp();
            gen("MOVE", retReg, "", "$v0"); 
        }
        if (!currentFunctionEpilogueLabel.empty()) {
            gen("J", "", "", currentFunctionEpilogueLabel);
        } else {
            // This case should ideally not happen if main always has an epilogue and other funcs do too.
            // For robustness, one might consider a default jump if outside a known function,
            // but current design relies on currentFunctionEpilogueLabel.
            cerr << "Error: return statement context error or epilogue label missing." << endl;
        }
        match("SEMICN");
    }
    else if (lookahead().type == "PRINTFTK")
    {
        match("PRINTFTK");
        match("LPARENT");
        string rawFormatStr = lookahead().value;
        if (rawFormatStr.length() >= 2 && rawFormatStr.front() == '"' && rawFormatStr.back() == '"') {
            rawFormatStr = rawFormatStr.substr(1, rawFormatStr.length() - 2);
        } else {
            cerr << "Error: Malformed string literal for printf: " << rawFormatStr << endl;
            exit(1);
        }
        match("STRCON");

        vector<string> arg_spill_addrs; 
        int saved_spill_offset_for_printf = current_spill_slot_offset; 

        auto calculate_printf_arg_spill_address = [&]() -> string { 
            int current_func_max_declared_vars_size = 0;
            if (!symbolTableStack.empty() && !symbolTableStack.back().empty()) {
                for (const auto& pair_sym : symbolTableStack.back()) {
                    const Symbol& sym = pair_sym.second;
                    if (!sym.isGlobal && !sym.isFunction) { 
                        current_func_max_declared_vars_size = max(current_func_max_declared_vars_size, sym.addr + sym.arraySize * 4);
                    }
                }
            }
            int total_offset = current_func_max_declared_vars_size + current_spill_slot_offset;
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
        for (const auto &part_pair : parts) {
            const string &part_content = part_pair.first;
            bool isArgPlaceholder = part_pair.second;

            if (isArgPlaceholder) { 
                // int expected_args = count_if(parts.begin(), parts.end(), [](const pair<string, bool>& p) { return p.second; }); // Not strictly needed for correctness of generation
                if (argIdx >= arg_spill_addrs.size()) { 
                    cerr << "Error: Not enough arguments provided to printf for format string." << endl;
                    exit(1);
                }
                string temp_for_print = getNextTempReg();
                gen("LW", temp_for_print, "", arg_spill_addrs[argIdx]); 
                gen("PRINTF_ARG", temp_for_print, "", ""); 
                argIdx++;
            } else { 
                string label = "str" + to_string(strCount++);
                string mipsEscapedPart;
                for (char c : part_content) {
                    if (c == '\n') mipsEscapedPart += "\\n";
                    else if (c == '\t') mipsEscapedPart += "\\t";
                    else if (c == '"') mipsEscapedPart += "\\\""; 
                    else if (c == '\\') mipsEscapedPart += "\\\\"; 
                    else mipsEscapedPart += c;
                }
                stringLiterals.push_back("\"" + mipsEscapedPart + "\""); 
                gen("PRINTF_STR", label, "", "");
            }
        }
        current_spill_slot_offset = saved_spill_offset_for_printf; 
    }
    else if (lookahead().type == "IDENFR" && (lookahead(1).type == "ASSIGN" || lookahead(1).type == "LBRACK")) {
        // LVal = Exp ; or LVal = getint();
        string lvalResult_addr_str_or_reg = LVal(); 
        
        string finalLvalTargetForStore = lvalResult_addr_str_or_reg;
        string lvalSpillLocForRegisterAddress;      
        bool lvalAddressWasInRegisterAndSpilled = false;
        int stmt_level_spill_offset_backup = current_spill_slot_offset; 

        bool lvalIsRegister = (!lvalResult_addr_str_or_reg.empty() && 
                               lvalResult_addr_str_or_reg[0] == 't' && 
                               lvalResult_addr_str_or_reg.length() > 1 && 
                               isdigit(lvalResult_addr_str_or_reg[1]));

        if (lvalIsRegister) {
            int current_func_max_declared_vars_size = 0;
            if (!symbolTableStack.empty() && !symbolTableStack.back().empty()) {
                for (const auto& pair_sym : symbolTableStack.back()) {
                    const Symbol& sym = pair_sym.second;
                    if (!sym.isGlobal && !sym.isFunction) {
                        current_func_max_declared_vars_size = max(current_func_max_declared_vars_size, sym.addr + sym.arraySize * 4);
                    }
                }
            }
            lvalSpillLocForRegisterAddress = to_string(current_func_max_declared_vars_size + current_spill_slot_offset) + "($sp)";
            
            gen("SW", lvalResult_addr_str_or_reg , "", lvalSpillLocForRegisterAddress); 
            lvalAddressWasInRegisterAndSpilled = true;
            current_spill_slot_offset += SPILL_SLOT_SIZE;
            max_spill_slots_needed_for_function = max(max_spill_slots_needed_for_function, current_spill_slot_offset);
        }
        
        match("ASSIGN");
        string rhsReg_val; 
        if (lookahead().type == "GETINTTK") {
            match("GETINTTK");
            match("LPARENT");
            match("RPARENT");
            rhsReg_val = getNextTempReg();
            gen("GETINT", "", "", rhsReg_val); 
        } else {
            rhsReg_val = Exp(); 
        }

        if (lvalAddressWasInRegisterAndSpilled) {
            string reloadedLvalAddressReg = getNextTempReg(); 
            gen("LW", reloadedLvalAddressReg, "", lvalSpillLocForRegisterAddress); 
            finalLvalTargetForStore = reloadedLvalAddressReg; 
        }
        
        gen("STORE", rhsReg_val, "", finalLvalTargetForStore); 

        if (lvalAddressWasInRegisterAndSpilled) {
            current_spill_slot_offset = stmt_level_spill_offset_backup;
        }
        match("SEMICN");
    }
    else // Exp; or ;
    {
        if (lookahead().type != "SEMICN") // If it's not an empty statement ;
        {
            Exp(); // Evaluate expression for side effects, result discarded
        }
        match("SEMICN");
    }
}
vector<string> ArgList() 
{
    vector<string> arg_spill_locations;

    auto calculate_current_spill_address = [&]() -> string { 
        int current_func_max_declared_vars_size = 0;
        if (!symbolTableStack.empty() && !symbolTableStack.back().empty()) {
            for (const auto& pair_sym : symbolTableStack.back()) {
                const Symbol& sym = pair_sym.second;
                 if (!sym.isGlobal && !sym.isFunction) {
                    current_func_max_declared_vars_size = max(current_func_max_declared_vars_size, sym.addr + sym.arraySize * 4);
                 }
            }
        }
        return to_string(current_func_max_declared_vars_size + current_spill_slot_offset) + "($sp)";
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

    for (int i = 0; i < min(4, num_total_args); ++i) {
        gen("LW", "$a" + to_string(i), "", arg_spill_addrs_on_caller_stack[i]); 
    }

    for (int i = num_total_args - 1; i >= 4; --i) {
        string temp_for_push = getNextTempReg();
        gen("LW", temp_for_push, "", arg_spill_addrs_on_caller_stack[i]); 
        gen("PUSH", temp_for_push, "", "");              
    }
    
    string returnReg = ""; 
    bool is_void_func = false; 
    if (!symbolTableStack.empty() && symbolTableStack[0].count(funcName)) {
        if (symbolTableStack[0][funcName].type == "void") { 
            is_void_func = true;
        }
    }
    if (!is_void_func) {
      returnReg = getNextTempReg(); 
    }

    gen("CALL", funcName, "", returnReg); 

    int num_stack_args_passed = max(0, num_total_args - 4);
    if (num_stack_args_passed > 0) {
        gen("ADDIU", "$sp", to_string(num_stack_args_passed * 4), "$sp"); 
    }
    
    current_spill_slot_offset = saved_caller_spill_offset; 
    
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
string AddExpForConst();
string UnaryExpForConst() {
    if (lookahead().type == "PLUS" || lookahead().type == "MINU" || lookahead().type == "NOT") {
        string op = lookahead().type;
        match(op);
        string valStr = UnaryExpForConst(); 
        if (valStr.empty()) { 
             cerr << "Error: Empty value from UnaryExpForConst." << endl; exit(1);
        }
        try {
            int num = stoi(valStr);
            if (op == "MINU") return to_string(-num);
            else if (op == "NOT") return to_string(!num); 
            else return valStr; 
        } catch (const std::exception& e) {
            cerr << "Error parsing number in UnaryExpForConst: " << valStr << endl;
            exit(1);
        }
    } else if (lookahead().type == "INTCON") {
        string num = lookahead().value;
        match("INTCON");
        return num;
    } else if (lookahead().type == "IDENFR") { 
        string identName = lookahead().value;
        match("IDENFR");
        int constVal;
        if (getConstValueFromSymbolTable(identName, constVal)) {
            return to_string(constVal);
        } else {
            cerr << "Error: Identifier '" << identName << "' is not a usable constant or not found in constant expression." << endl;
            exit(1);
        }
    } else if (lookahead().type == "LPARENT") {
        match("LPARENT");
        string val = AddExpForConst(); 
        match("RPARENT");
        return val;
    } else {
        cerr << "Error: Invalid token '" << lookahead().type << "' in constant expression during UnaryExpForConst." << endl;
        exit(1);
    }
}
string MulExpForConst() {
    string left = UnaryExpForConst();
    while (lookahead().type == "MULT" || lookahead().type == "DIV" || lookahead().type == "MOD") {
        string op = lookahead().type;
        match(op);
        string right = UnaryExpForConst();
        if (left.empty() || right.empty()) {
            cerr << "Error: Empty operand in MulExpForConst." << endl; exit(1);
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
            cerr << "Error parsing number in MulExpForConst: " << left << " or " << right << endl;
            exit(1);
        }
    }
    return left;
}
string AddExpForConst() {
    string left = MulExpForConst();
    while (lookahead().type == "PLUS" || lookahead().type == "MINU") {
        string op = lookahead().type;
        match(op);
        string right = MulExpForConst();
         if (left.empty() || right.empty()) {
            cerr << "Error: Empty operand in AddExpForConst." << endl; exit(1);
        }
        try {
            int leftVal = stoi(left);
            int rightVal = stoi(right);
            left = (op == "PLUS") ? to_string(leftVal + rightVal) : to_string(leftVal - rightVal);
        } catch (const std::exception& e) {
            cerr << "Error parsing number in AddExpForConst: " << left << " or " << right << endl;
            exit(1);
        }
    }
    return left;
}
string ConstExp() { 
    string numStr = AddExpForConst();
    return numStr;
}
void VarDef()
{
    string varName = lookahead().value;
    match("IDENFR");
    int arraySize = 1;
    if (lookahead().type == "LBRACK") {
        match("LBRACK");
        string sizeStr = ConstExp(); 
        arraySize = stoi(sizeStr);
        if (arraySize <= 0 && lookahead().type == "RBRACK") { 
            cerr << "Warning: Array '" << varName << "' declared with size " << arraySize << "." << endl;
             if (arraySize < 0) {cerr << "Error: Array size cannot be negative." << endl; exit(1);}
        }
        match("RBRACK");
    }
    addSymbol(varName, "int", false, false, arraySize); 
    Symbol& sym_ref = symbolTableStack.back()[varName]; 

    if (lookahead().type == "ASSIGN")
    {
        match("ASSIGN");
        if (sym_ref.isGlobal) { 
            if (arraySize == 1) { 
                if (lookahead().type == "LBRACE") {
                    cerr << "错误：全局标量变量初始化 '" << varName << "' 不能使用 '{...}'" << endl;
                    exit(1);
                }
                sym_ref.value = ConstExp(); 
            } else { 
                if (lookahead().type != "LBRACE") {
                    cerr << "错误：全局数组初始化 '" << varName << "' 必须使用 '{...}'" << endl;
                    exit(1);
                }
                match("LBRACE");
                sym_ref.value = "{"; 
                int init_count = 0;
                if (lookahead().type != "RBRACE") {
                    string first_val = ConstExp(); 
                    sym_ref.value += first_val;
                    init_count++;
                    while (lookahead().type == "COMMA") {
                        match("COMMA");
                        string next_val = ConstExp();
                        sym_ref.value += "," + next_val;
                        init_count++;
                    }
                }
                sym_ref.value += "}";
                match("RBRACE");
            }
        } else { 
            if (arraySize == 1 && lookahead().type == "LBRACE") { 
                 cerr << "错误：局部标量变量初始化 '" << varName << "' 不应使用 '{...}' " << endl;
                 exit(1);
            }
            if (arraySize > 1 && lookahead().type == "LBRACE") { 
                match("LBRACE");
                int elements_initialized = 0;
                if (lookahead().type != "RBRACE") {
                    do {
                        if (elements_initialized > 0 && lookahead().type == "COMMA") match("COMMA");
                        else if (elements_initialized > 0) break; 

                        if (elements_initialized >= arraySize) {
                            cerr << "Error: Too many initializers for local array " << varName << endl;
                            exit(1);
                        }
                        
                        string element_val_reg = Exp(); 
                        
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
                    } while (lookahead().type == "COMMA");
                }
                match("RBRACE");
            } else { 
                string valReg;
                if (lookahead().type == "GETINTTK") {
                    match("GETINTTK");
                    match("LPARENT");
                    match("RPARENT");
                    valReg = getNextTempReg();
                    gen("GETINT", "", "", valReg);
                } else {
                    valReg = Exp(); 
                }
                string varAddr = getVariableAddress(varName); 
                gen("STORE", valReg, "", varAddr); 
            }
        }
    } else {
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
vector<string> ConstInitVal() { 
    vector<string> values;
    if (lookahead().type == "LBRACE") {
        match("LBRACE");
        if (lookahead().type != "RBRACE") {
            values.push_back(ConstExp()); 

            while (lookahead().type == "COMMA") {
                match("COMMA");
                values.push_back(ConstExp()); 
            }
        }
        match("RBRACE");
    } else { 
        values.push_back(ConstExp());
    }
    return values;
}
void ConstDef() {
    string constName = lookahead().value;
    match("IDENFR"); 

    int declaredArraySize = -1; 
    if (lookahead().type == "LBRACK") { 
        match("LBRACK");
        string sizeStr = ConstExp(); 
        if (sizeStr.empty()) {
             cerr << "Error: Could not evaluate constant expression for array size of '" << constName << "'." << endl; exit(1);
        }
        declaredArraySize = stoi(sizeStr);
        if (declaredArraySize <= 0 && lookahead().type == "RBRACK") { 
            cerr << "Warning: Constant array '" << constName << "' declared with size " << declaredArraySize << "." << endl;
            if (declaredArraySize < 0) { cerr << "Error: Array size cannot be negative." << endl; exit(1); }
        }
        match("RBRACK");
    }

    match("ASSIGN");
    vector<string> initValues = ConstInitVal(); 

    int finalArraySize;
    if (declaredArraySize != -1) { 
        finalArraySize = declaredArraySize;
        if (initValues.size() > (size_t)finalArraySize && !(initValues.empty() && finalArraySize == 0) ) {
            if (finalArraySize == 0 && !initValues.empty()){ 
                cerr << "Error: Initializers provided for constant array '" << constName << "' declared with size 0." << endl;
                exit(1);
            } else if (finalArraySize > 0) { 
                 cerr << "Error: Too many initializers for constant array '" << constName << "' (expected " << finalArraySize << ", got " << initValues.size() << ")." << endl;
                 exit(1);
            }
        }
        if (finalArraySize > 0) { 
            while (initValues.size() < (size_t)finalArraySize) {
                initValues.push_back("0");
            }
        }
    } else { 
        bool isEmptyBraceInitializer = false; 
        if (initValues.empty() && pos > 0 && tokens[pos-1].type == "RBRACE" && 
            pos > 1 && tokens[pos-2].type == "LBRACE" && 
            pos > 2 && tokens[pos-3].type == "ASSIGN") { 
             isEmptyBraceInitializer = true;
        }

        if (initValues.empty() && isEmptyBraceInitializer) { 
             finalArraySize = 0; 
        } else if (initValues.empty() && declaredArraySize == -1){ 
             cerr << "Error: No initializer provided for constant '" << constName << "' and size not explicitly given." << endl;
             exit(1);
        } else { 
            finalArraySize = initValues.size(); 
        }
    }
    
    addSymbol(constName, "int", true, false, finalArraySize);
    if (!symbolTableStack.back().count(constName)) {
         cerr << "CRITICAL Error: Symbol '" << constName << "' not found in current scope immediately after addSymbol." << endl;
         exit(1);
    }

    Symbol& sym = symbolTableStack.back()[constName];
    sym.constValuesList = initValues; 
    if (finalArraySize == 1 && !initValues.empty()) {
        sym.value = initValues[0]; 
    }
    if (!sym.isGlobal && sym.arraySize > 0) { 
        string baseAddrArr_str = getVariableAddress(constName); 
        
        for (size_t k = 0; k < sym.constValuesList.size(); ++k) {
            if (k >= (size_t)sym.arraySize) break; 

            string val_to_store_reg = getNextTempReg();
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

    enterScope(); 
    reset_spill_management();
    
    string oldEpilogueLabel = currentFunctionEpilogueLabel;
    currentFunctionEpilogueLabel = "L_epilogue_FUNC_main"; 

    gen("LABEL", "", "", "FUNC_main"); 
    size_t prologueInsertionPoint = intermediateCode.size();
    intermediateCode.push_back({"NOP_PROLOGUE_PLACEHOLDER", "", "", ""}); 

    Block(); 
    
    int frameSize = calculateFrameSize(symbolTableStack.back(), max_spill_slots_needed_for_function);

    vector<Quadruple> prologueQuads;
    prologueQuads.push_back({"ADDIU", "$sp", "-" + to_string(frameSize), "$sp"});
    prologueQuads.push_back({"SW", "$ra", "", to_string(frameSize - 4) + "($sp)"}); 
    if (prologueInsertionPoint < intermediateCode.size() && intermediateCode[prologueInsertionPoint].op == "NOP_PROLOGUE_PLACEHOLDER") {
        intermediateCode.erase(intermediateCode.begin() + prologueInsertionPoint);
        intermediateCode.insert(intermediateCode.begin() + prologueInsertionPoint,
                                prologueQuads.begin(), prologueQuads.end());
    } else {
        cerr << "Error: Prologue placeholder missing or misplaced for main." << endl;
    }
    bool explicitReturnExists = false;
    if (!intermediateCode.empty()) {
        for (long k_loop = intermediateCode.size() - 1; k_loop >= 0; --k_loop) { // Renamed k
            const auto& quad = intermediateCode[k_loop];
            if (quad.op == "J" && quad.result == currentFunctionEpilogueLabel) {
                explicitReturnExists = true;
                break;
            }
            if (quad.op == "LABEL" && quad.result.rfind("FUNC_",0) == 0 && quad.result != currentFunctionEpilogueLabel) { 
                break;
            }
        }
    }
    if (!explicitReturnExists) {
        gen("LI", "0", "", "$v0"); 
        gen("J", "", "", currentFunctionEpilogueLabel); 
    }
    
    gen("LABEL", "", "", currentFunctionEpilogueLabel);
    gen("LW", "$ra", "", to_string(frameSize - 4) + "($sp)"); 
    gen("ADDIU", "$sp", to_string(frameSize), "$sp");    
    gen("JR", "$ra", "", "");                            

    leaveScope();
    currentFunctionEpilogueLabel = oldEpilogueLabel; 
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
    else
    {
        cerr << "Syntax error: expected 'int' or 'void' for function type but got " << lookahead().type << endl;
        exit(1);
    }
}

void FuncDef() {
    string return_type_str = FuncType(); 
    string funcName = lookahead().value;
    match("IDENFR");
    if (!symbolTableStack.empty()) {
        symbolTableStack[0][funcName] = Symbol(funcName, return_type_str , 0, false, true);
        symbolTableStack[0][funcName].type = return_type_str; 
    } else { 
        cerr << "Critical Error: Global scope missing during FuncDef for " << funcName << endl;
        exit(1);
    }

    string oldEpilogueLabel = currentFunctionEpilogueLabel;
    currentFunctionEpilogueLabel = "L_epilogue_FUNC_" + funcName; 

    match("LPARENT");
    enterScope(); 
    reset_spill_management(); 

    vector<string> paramNamesList;
    if (lookahead().type != "RPARENT") { 
        if (lookahead().type != "INTTK") { 
            cerr << "Syntax Error: Expected 'int' for parameter type in function " << funcName << endl;
            exit(1);
        }
        match("INTTK"); 
        string firstParamName = lookahead().value;
        match("IDENFR");
        paramNamesList.push_back(firstParamName);
        addSymbol(firstParamName, "int", false, false, 1); 

        while(lookahead().type == "COMMA") {
            match("COMMA");
            if (lookahead().type != "INTTK") {
                 cerr << "Syntax Error: Expected 'int' for parameter type in function " << funcName << endl;
                 exit(1);
            }
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

    Block(); 

    int frameSize = calculateFrameSize(symbolTableStack.back(), max_spill_slots_needed_for_function);

    vector<Quadruple> prologueQuads;
    prologueQuads.push_back({"ADDIU", "$sp", "-" + to_string(frameSize), "$sp"});
    prologueQuads.push_back({"SW", "$ra", "", to_string(frameSize - 4) + "($sp)"}); 
    for (int i = 0; i < (int)paramNamesList.size(); ++i) {
        string paramLocalAddr = getVariableAddress(paramNamesList[i]); 
        if (i < 4) { 
            prologueQuads.push_back({"SW", "$a" + to_string(i), "", paramLocalAddr}); 
        } else { 
            string callerStackArgAddr = to_string(frameSize + (i - 4) * 4) + "($sp)";
            string tempRegForParamLoad = MIPS_TEMP_REG_1; 
            prologueQuads.push_back({"LW", tempRegForParamLoad, "", callerStackArgAddr}); 
            prologueQuads.push_back({"SW", tempRegForParamLoad, "", paramLocalAddr});     
        }
    }
    
    if (prologueInsertionPoint < intermediateCode.size() && intermediateCode[prologueInsertionPoint].op == "NOP_PROLOGUE_PLACEHOLDER") {
        intermediateCode.erase(intermediateCode.begin() + prologueInsertionPoint);
        intermediateCode.insert(intermediateCode.begin() + prologueInsertionPoint,
                                prologueQuads.begin(), prologueQuads.end());
    } else {
         cerr << "Error: Prologue placeholder missing or misplaced for " << funcName << endl;
    }

    if (return_type_str == "void") {
         bool explicitReturnExists = false;
         if (!intermediateCode.empty()) {
            for (long k_loop = intermediateCode.size() - 1; k_loop >= 0; --k_loop) { // Renamed k
                const auto& quad = intermediateCode[k_loop];
                if (quad.op == "J" && quad.result == currentFunctionEpilogueLabel) {
                    explicitReturnExists = true;
                    break;
                }
                 if (quad.op == "LABEL" && quad.result == funcLabelForMIPS && quad.result != currentFunctionEpilogueLabel) { 
                    break; 
                }
            }
         }
         if (!explicitReturnExists) {
            gen("J", "", "", currentFunctionEpilogueLabel); 
         }
    } else { 
         // For non-void, ensure there's a path to return. 
         // A simple check: if the last instruction isn't a jump to epilogue, add one.
         // This is a heuristic. A more robust check would involve control flow graph analysis.
         bool lastInstructionJumpsToEpilogue = false;
         if(!intermediateCode.empty()){
             const auto& lastQuad = intermediateCode.back();
             if(lastQuad.op == "J" && lastQuad.result == currentFunctionEpilogueLabel){
                 lastInstructionJumpsToEpilogue = true;
             }
         }
         if(!lastInstructionJumpsToEpilogue && (intermediateCode.empty() || intermediateCode.back().op != "LABEL" || intermediateCode.back().result != currentFunctionEpilogueLabel ) ){
             // No explicit return at the end of the block and not already at epilogue label
             // This case is tricky: if the function is guaranteed to return via other paths (e.g. if/else with returns in both)
             // adding an unconditional jump here might be wrong or create dead code.
             // For now, let's assume if it falls through, it's an error or needs implicit return handling if allowed by language.
             // SysY usually requires explicit return for non-void.
             // We could add a warning here or rely on the epilogue's JR $ra.
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
    while (lookahead().type == "CONSTTK" || 
           (lookahead().type == "INTTK" && lookahead(1).type == "IDENFR" && lookahead(2).type != "LPARENT") ) 
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
string LVal(bool* outIsArrayAccessWithoutIndex) { 
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
    string baseAddrStr = getVariableAddress(varName); 

    if (lookahead().type == "LBRACK") { 
        if (outIsArrayAccessWithoutIndex) *outIsArrayAccessWithoutIndex = false; 
        match("LBRACK");
        string idxReg = Exp();  
        match("RBRACK");
        
        string offsetReg = getNextTempReg(); 
        string four_val_reg = getNextTempReg(); 
        gen("LI", "4", "", four_val_reg); 
        gen("MUL", idxReg, four_val_reg, offsetReg); 

        string elementAddrReg = getNextTempReg(); 
        if (sym.isGlobal) { 
            string baseAddrInReg = getNextTempReg(); 
            gen("LA", sym.name, "", baseAddrInReg);  
            gen("ADD", baseAddrInReg, offsetReg, elementAddrReg); 
        } else { 
            string baseAddrInReg = getNextTempReg();
            size_t sp_pos = baseAddrStr.find("($sp)");
            string offset_from_sp_str = (sp_pos != string::npos) ? baseAddrStr.substr(0, sp_pos) : "0";
            gen("ADDIU","$sp", offset_from_sp_str, baseAddrInReg); 
            gen("ADD", baseAddrInReg, offsetReg, elementAddrReg);   
        }
        return elementAddrReg; 
    } else { 
        if (outIsArrayAccessWithoutIndex && sym.arraySize > 1) {
            *outIsArrayAccessWithoutIndex = true;
        }
        return baseAddrStr; 
    }
}
string UnaryExp()
{
    if (lookahead().type == "IDENFR" && lookahead(1).type == "LPARENT") {
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
            gen("NEG", operand_reg, "", temp_res_reg); 
        }
        else if (op == "NOT")
        {
            gen("NOT", operand_reg, "", temp_res_reg); 
        }
        else 
        {
            gen("MOVE", operand_reg, "", temp_res_reg); 
        }
        return temp_res_reg; 
    }
    else
    {
        return PrimaryExp(); 
    }
}
// In MulExp()
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
        
        bool lhs_is_temp_reg = (!lhs_operand_for_this_op.empty() && // Ensure not empty string
                                lhs_operand_for_this_op[0] == 't' && 
                                lhs_operand_for_this_op.length() > 1 && 
                                isdigit(lhs_operand_for_this_op[1]));

        if (lhs_is_temp_reg) { 
            // Always spill LHS if it's in a temp reg before evaluating RHS UnaryExp
            int current_func_max_declared_vars_size = 0;
            if (!symbolTableStack.empty() && !symbolTableStack.back().empty()) {
                for (const auto& pair_sym : symbolTableStack.back()) {
                    const Symbol& sym_s = pair_sym.second; 
                     if (!sym_s.isGlobal && !sym_s.isFunction) {
                        current_func_max_declared_vars_size = max(current_func_max_declared_vars_size, sym_s.addr + sym_s.arraySize * 4);
                     }
                }
            }
            int spill_offset_on_stack = current_func_max_declared_vars_size + current_spill_slot_offset;
            spill_addr_str_for_lhs = to_string(spill_offset_on_stack) + "($sp)";
            
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

// In AddExp()
string AddExp() {
    string current_accumulated_value_reg = MulExp(); 

    while (lookahead().type == "PLUS" || lookahead().type == "MINU") {
        string op_code = (lookahead().type == "PLUS") ? "ADD" : "SUB";
        match(lookahead().type);

        string lhs_operand_for_this_op = current_accumulated_value_reg;
        
        bool did_spill_lhs = false;
        string spill_addr_str_for_lhs;
        int saved_spill_offset_before_lhs_spill = current_spill_slot_offset; 
        
        bool lhs_is_temp_reg = (!lhs_operand_for_this_op.empty() && // Ensure not empty string
                                lhs_operand_for_this_op[0] == 't' && 
                                lhs_operand_for_this_op.length() > 1 && 
                                isdigit(lhs_operand_for_this_op[1]));

        if (lhs_is_temp_reg) { 
            // Always spill LHS if it's in a temp reg before evaluating RHS MulExp
            int current_func_max_declared_vars_size = 0;
            if (!symbolTableStack.empty() && !symbolTableStack.back().empty()) {
                for (const auto& pair_sym : symbolTableStack.back()) {
                    const Symbol& sym_s = pair_sym.second; 
                     if (!sym_s.isGlobal && !sym_s.isFunction) { 
                        current_func_max_declared_vars_size = max(current_func_max_declared_vars_size, sym_s.addr + sym_s.arraySize * 4);
                    }
                }
            }
            
            int spill_offset_on_stack = current_func_max_declared_vars_size + current_spill_slot_offset;
            spill_addr_str_for_lhs = to_string(spill_offset_on_stack) + "($sp)";
            
            gen("SW", lhs_operand_for_this_op, "", spill_addr_str_for_lhs); 
            did_spill_lhs = true;

            current_spill_slot_offset += SPILL_SLOT_SIZE; 
            max_spill_slots_needed_for_function = max(max_spill_slots_needed_for_function, current_spill_slot_offset);
        }

        string rhs_value_reg = MulExp(); // Evaluate RHS

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
        for (auto it = symbolTableStack.rbegin(); it != symbolTableStack.rend(); ++it) {
            if (it->count(varName)) { 
                sym = it->at(varName);
                foundSym = true;
                break;
            }
        }
        if (!foundSym) { cerr << "Error: Symbol " << varName << " not found in PrimaryExp (after LVal)." << endl; exit(1);}

        bool lvalReturnedRegisterHoldingAddress = (!lvalResult.empty() && lvalResult[0] == 't' && lvalResult.length() > 1 && isdigit(lvalResult[1]));


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
                    if (sp_pos == string::npos) { 
                        cerr << "Error: Malformed local array address string in PrimaryExp: " << lvalResult << endl;
                        exit(1);
                    }
                    string offset_str = lvalResult.substr(0, sp_pos);
                    gen("ADDIU", "$sp", offset_str, tempDestReg); 
                }
            } else { 
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
    else if (lookahead().type == "GETINTTK") 
    {
        match("GETINTTK");
        match("LPARENT");
        match("RPARENT");
        string temp_reg = getNextTempReg();
        gen("GETINT", "", "", temp_reg); 
        return temp_reg;
    }
    else
    {
        cerr << "语法错误：PrimaryExp 遇到意外的 Token '" << lookahead().type << "'" << endl;
        exit(1);
    }
}
void skip_comments(const string &code, size_t &i) {
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
                            case '"': str_val += '"'; break; 
                            default: 
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
        if (isalpha(code[i]) || code[i] == '_')
        {
            size_t start = i;
            while (i < len && (isalnum(code[i]) || code[i] == '_'))
            {
                i++;
            }
            string word = code.substr(start, i - start);
            tokens.push_back({keywords.count(word) ? keywords[word] : "IDENFR", word});
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
        cerr << "Error: Unrecognized character '" << code[i] << "' at position " << i << endl;
        i++; 
    }
}

int main()
{
    ifstream infile("testfile.txt");
    if (!infile.is_open()) {
        cerr << "Error: Could not open testfile.txt" << endl;
        return 1;
    }
    stringstream buffer;
    buffer << infile.rdbuf();
    infile.close();
    enterScope(true); 
    tokenize(buffer.str());
    CompUnit(); 
    generateMIPS(); 
    leaveScope(); 

    ifstream mips_check("mips.txt");
    if (!mips_check.good()) {
        cerr << "Error: mips.txt was not generated or is not accessible." << endl;
        return 1;
    }
    mips_check.close();
    int mars_exit_code = system("java -jar mars.jar nc mips.txt < input.txt > output.txt"); 
    return 0;
}