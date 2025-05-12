#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <stack>
#include <stdexcept>
#include <cctype>
#include <algorithm>
#include <memory> // For smart pointers if needed later

// Define token types based on lexer_phase2(2).cpp's keywords and symbols
enum class TokenType {
    // Keywords (from 'keywords' map)
    CONSTTK, INTTK, BREAKTK, CONTINUETK, IFTK, ELSETK, WHILETK, RETURNTK, VOIDTK, MAINTK, GETINTTK, PRINTFTK,
    // Identifier
    IDENFR,
    // Integer Constant
    INTCON,
    // String Constant
    STRCON,
    // Operators & Delimiters (from 'symbols' map)
    NEQ, NOT, AND, OR, PLUS, MINU, MULT, DIV, MOD, LSS, LEQ, GRE, GEQ, EQL, ASSIGN,
    SEMICN, COMMA, LPARENT, RPARENT, LBRACK, RBRACK, LBRACE, RBRACE,
    // Special Tokens
    EOFTK, // End Of File
    UNKNOWN // For errors or unrecognized tokens
};

// Forward declarations for classes we will define
class Lexer;
class SymbolTable;
class IRGenerator;
class Parser;
class MIPSGenerator;
class Compiler; // Orchestrator

// ============================================================================
// Core Data Structures
// ============================================================================

struct Token {
    TokenType type;
    std::string value; // Original text of the token (e.g., identifier name, literal value)
    int line;          // Line number where the token starts

    Token(TokenType t = TokenType::UNKNOWN, std::string v = "", int l = 0)
        : type(t), value(std::move(v)), line(l) {}
};

// Symbol structure, mirroring fields from lexer_phase2(2).cpp's Symbol struct
// and related global variables.
struct Symbol {
    std::string name;
    std::string type_str; // For functions: "int" or "void" (return type). For variables: usually "int".
                        // This replaces the TokenType member in the previous iteration for more clarity
                        // similar to lexer_phase2(2).cpp's string type.
    int addr_offset;    // For local variables/params: stack offset.
                        // For global variables: will be 0, name used as label.
    bool isGlobal = true;
    bool isConst = false;
    bool isFunction = false;
    int arraySize = 1;       // Default to 1 for scalars.
    std::string const_scalar_value; // For single const int value.
    std::vector<std::string> const_array_values; // For const array initializers.
    std::string global_init_value; // For global var initializers (scalar or string like "{v1,v2}")
    int paramCount = 0;      // Number of parameters for a function.

    // Default constructor
    Symbol() = default;

    // Parameterized constructor for convenience, similar to lexer_phase2(2).cpp
    Symbol(const std::string& n, const std::string& t_str, int offset, bool is_c, bool is_f, bool is_g = true, int arr_sz = 1)
        : name(n), type_str(t_str), addr_offset(offset), isGlobal(is_g), isConst(is_c), isFunction(is_f), arraySize(arr_sz) {}
};

// Quadruple structure for Intermediate Representation
struct Quadruple {
    std::string op;
    std::string arg1;
    std::string arg2;
    std::string result;
    // std::string comment; // Optional: for debugging, can be added if useful

    Quadruple(std::string o, std::string a1, std::string a2, std::string r)
        : op(std::move(o)), arg1(std::move(a1)), arg2(std::move(a2)), result(std::move(r)) {}
};

// ============================================================================
// Lexer Class Implementation
// ============================================================================

Lexer::Lexer(const std::string& sourceCode)
    : source(sourceCode), pos(0), line(1) {
    // Initialize keywords map based on lexer_phase2(2).cpp
    keywords = {
        {"const", TokenType::CONSTTK}, {"int", TokenType::INTTK}, {"break", TokenType::BREAKTK},
        {"continue", TokenType::CONTINUETK}, {"if", TokenType::IFTK}, {"else", TokenType::ELSETK},
        {"while", TokenType::WHILETK}, {"return", TokenType::RETURNTK}, {"void", TokenType::VOIDTK},
        {"main", TokenType::MAINTK}, {"getint", TokenType::GETINTTK}, {"printf", TokenType::PRINTFTK}
    };
    // Note: symbols map from original is handled directly in operator/delimiter recognition
}

// Helper: Peek at the current character without advancing
char Lexer::peek() const {
    return (pos < source.length()) ? source[pos] : '\0';
}

// Helper: Peek ahead n characters
char Lexer::peek(size_t offset) const {
    return (pos + offset < source.length()) ? source[pos + offset] : '\0';
}

// Helper: Consume the current character and return it, advancing position and line number
char Lexer::advance() {
    if (pos >= source.length()) return '\0';
    char current = source[pos];
    if (current == '\n') {
        line++;
    }
    pos++;
    return current;
}

// Helper: Skip whitespace and comments, like skip_comments in original
void Lexer::skipWhitespaceAndComments() {
    while (pos < source.length()) {
        if (std::isspace(peek())) {
            advance(); // Consumes whitespace, handles newline counting via advance()
        } else if (peek() == '/') {
            if (peek(1) == '/') { // Single-line comment
                advance(); // Consume '/'
                advance(); // Consume '/'
                while (pos < source.length() && peek() != '\n') {
                    advance();
                }
                // Don't consume the newline here, let the outer loop handle it if needed
            } else if (peek(1) == '*') { // Multi-line comment
                size_t comment_start_line = line;
                size_t comment_start_pos = pos;
                advance(); // Consume '/'
                advance(); // Consume '*'
                while (pos < source.length()) {
                    if (peek() == '*' && peek(1) == '/') {
                        advance(); // Consume *
                        advance(); // Consume /
                        goto next_iteration; // Use goto to easily break outer loop check
                    }
                    advance(); // Consumes comment content, handles newlines
                }
                // If loop finishes, we hit EOF without comment termination
                std::cerr << "Error line " << comment_start_line << " (pos " << comment_start_pos << "): Unterminated block comment." << std::endl;
                // Consider throwing an exception or setting an error state
                pos = source.length(); // Force EOF
            } else {
                // Just a division operator, not a comment start
                break;
            }
        } else {
            // Not whitespace or start of comment
            break;
        }
        next_iteration:;
    }
}

// Recognizes identifiers and keywords
Token Lexer::recognizeIdentifierOrKeyword() {
    size_t startPos = pos;
    while (pos < source.length() && (std::isalnum(peek()) || peek() == '_')) {
        advance();
    }
    std::string value = source.substr(startPos, pos - startPos);
    auto it = keywords.find(value);
    TokenType type = (it != keywords.end()) ? it->second : TokenType::IDENFR;
    return Token(type, value, line); // Line number is current line after consuming the token
}

// Recognizes integer literals
Token Lexer::recognizeNumber() {
    size_t startPos = pos;
    int startLine = line;
    while (pos < source.length() && std::isdigit(peek())) {
        advance();
    }
    std::string value = source.substr(startPos, pos - startPos);
    // TODO: Add error checking for integer range if necessary
    return Token(TokenType::INTCON, value, startLine);
}

// Recognizes string literals, handling escapes like original
Token Lexer::recognizeStringLiteral() {
    int startLine = line;
    size_t startPos = pos;
    std::string contentValue; // Store the processed content without quotes/escapes
    advance(); // Consume opening '"'

    while (pos < source.length() && peek() != '"') {
        if (peek() == '\\') { // Escape sequence
            advance(); // Consume '\'
            if (pos >= source.length()) { // Dangling backslash at EOF
                std::cerr << "Error line " << line << ": Dangling backslash in string literal." << std::endl;
                // Optionally throw or return error token
                break; // Exit loop
            }
            char escapedChar = advance(); // Consume character after backslash
            switch (escapedChar) {
                case 'n': contentValue += '\n'; break;
                case 't': contentValue += '\t'; break;
                case '\\': contentValue += '\\'; break;
                case '"': contentValue += '"'; break;
                // Note: Original code added \ and the char for others.
                // Let's replicate that specific behavior if needed, or stick to standard C escapes.
                // Assuming standard for now, but can adjust.
                default:
                    // As per original: treat unknown escapes as literal backslash + char
                    // Although standard C behavior might differ or warn. Let's follow original.
                    contentValue += '\\';
                    contentValue += escapedChar;
                    std::cerr << "Warning line " << line << ": Possibly unknown escape sequence \\" << escapedChar << " in string literal." << std::endl;
                    break;
            }
        } else if (peek() == '\n') { // Newline within string literal (usually error)
            std::cerr << "Error line " << line << ": Newline encountered within string literal." << std::endl;
            // Optionally throw or return error token
            advance(); // Consume newline to potentially continue parsing after error
            break; // Exit loop for this literal
        } else {
            contentValue += advance(); // Consume regular character
        }
    }

    std::string originalValue = source.substr(startPos, pos - startPos + (peek() == '"' ? 1 : 0) );

    if (pos < source.length() && peek() == '"') {
        advance(); // Consume closing '"'
        // Return the *original* string including quotes as the value, like the original code seems to do.
        // The processed `contentValue` could be stored elsewhere or used differently if needed.
        return Token(TokenType::STRCON, originalValue, startLine);
    } else {
        std::cerr << "Error line " << line << ": Unterminated string literal starting at line " << startLine << "." << std::endl;
        // Return error token or throw
        return Token(TokenType::UNKNOWN, originalValue, startLine);
    }
}

// Recognizes operators and delimiters based on original 'symbols' map logic
Token Lexer::recognizeOperatorOrDelimiter() {
     int startLine = line;
    // Check for two-character operators first
    if (pos + 1 < source.length()) {
        std::string twoCharOp = source.substr(pos, 2);
        if (twoCharOp == "!=") { advance(); advance(); return Token(TokenType::NEQ, "!=", startLine); }
        if (twoCharOp == "&&") { advance(); advance(); return Token(TokenType::AND, "&&", startLine); }
        if (twoCharOp == "||") { advance(); advance(); return Token(TokenType::OR, "||", startLine); }
        if (twoCharOp == "<=") { advance(); advance(); return Token(TokenType::LEQ, "<=", startLine); }
        if (twoCharOp == ">=") { advance(); advance(); return Token(TokenType::GEQ, ">=", startLine); }
        if (twoCharOp == "==") { advance(); advance(); return Token(TokenType::EQL, "==", startLine); }
    }

    // Check for one-character operators/delimiters
    char currentChar = peek();
    advance(); // Consume the character
    switch (currentChar) {
        case '!': return Token(TokenType::NOT, "!", startLine);
        case '+': return Token(TokenType::PLUS, "+", startLine);
        case '-': return Token(TokenType::MINU, "-", startLine);
        case '*': return Token(TokenType::MULT, "*", startLine);
        case '/': return Token(TokenType::DIV, "/", startLine);
        case '%': return Token(TokenType::MOD, "%", startLine);
        case '<': return Token(TokenType::LSS, "<", startLine);
        case '>': return Token(TokenType::GRE, ">", startLine);
        case '=': return Token(TokenType::ASSIGN, "=", startLine);
        case ';': return Token(TokenType::SEMICN, ";", startLine);
        case ',': return Token(TokenType::COMMA, ",", startLine);
        case '(': return Token(TokenType::LPARENT, "(", startLine);
        case ')': return Token(TokenType::RPARENT, ")", startLine);
        case '[': return Token(TokenType::LBRACK, "[", startLine);
        case ']': return Token(TokenType::RBRACK, "]", startLine);
        case '{': return Token(TokenType::LBRACE, "{", startLine);
        case '}': return Token(TokenType::RBRACE, "}", startLine);
        default:
            std::cerr << "Error line " << startLine << ": Unrecognized character '" << currentChar << "'." << std::endl;
            return Token(TokenType::UNKNOWN, std::string(1, currentChar), startLine);
    }
}

// Main lexer function: gets the next token from the source
Token Lexer::getNextToken() {
    skipWhitespaceAndComments();

    if (pos >= source.length()) {
        return Token(TokenType::EOFTK, "", line);
    }

    int startLine = line; // Record line *before* consuming token characters
    char current = peek();

    if (std::isalpha(current) || current == '_') {
        // Need to capture line number *before* recognizing, as recognize advances line number.
        int tokenStartLine = line;
        Token token = recognizeIdentifierOrKeyword();
        token.line = tokenStartLine;
        return token;
    } else if (std::isdigit(current)) {
         int tokenStartLine = line;
         Token token = recognizeNumber();
         token.line = tokenStartLine;
         return token;
    } else if (current == '"') {
         int tokenStartLine = line;
         Token token = recognizeStringLiteral();
         token.line = tokenStartLine;
         return token;
    } else {
        // Assume operator or delimiter
         int tokenStartLine = line;
         Token token = recognizeOperatorOrDelimiter();
         token.line = tokenStartLine;
         return token;
    }
}

// We will add implementations and other classes (Token, Symbol, Quadruple, SymbolTable, etc.) next.

// Placeholder main function for now
int main(int argc, char* argv[]) {
    // Basic command line argument handling (input/output files)
    // Will instantiate Compiler class later
    std::cout << "Compiler refactoring starting..." << std::endl;
    return 0;
}

// ============================================================================
// SymbolTable Class Definition & Implementation
// ============================================================================

class SymbolTable {
private:
    std::vector<std::unordered_map<std::string, Symbol>> scopeStack;
    // We need to replicate the offset calculation based on `tempAddr` from the original.
    // `tempAddr` seems to represent the *next available positive offset* from the start
    // of the local variable area for the current scope.
    int currentScopeLocalOffset = 0; // Tracks the next available offset (like tempAddr)
    int globalScopeLevel = 0; // Explicitly track the global scope index (usually 0)

    // Store the starting offset for each scope to calculate relative addresses if needed,
    // or just use the absolute offset stored in the symbol.
    std::vector<int> scopeStartOffsets; // Stores the 'currentScopeLocalOffset' value when entering a scope

public:
    SymbolTable() {
        // Initialize by entering the global scope
        enterScope(true);
    }

    // Enter a new scope (nested or the initial global scope)
    void enterScope(bool isGlobal = false) {
        scopeStack.emplace_back(); // Add a new map for the new scope
        if (isGlobal) {
            currentScopeLocalOffset = 0; // Globals don't use this offset mechanism
            scopeStartOffsets.push_back(0);
            globalScopeLevel = scopeStack.size() - 1;
        } else {
            // For local scopes, the starting offset is the current offset value.
            // The *next* local variable will be allocated *at* this offset.
            scopeStartOffsets.push_back(currentScopeLocalOffset);
            // Resetting currentScopeLocalOffset to 0 here is *incorrect* based on the original
            // tempAddr logic. tempAddr continued accumulating offset within the function.
            // Let's keep currentScopeLocalOffset accumulating unless we explicitly reset it
            // for a new function (which the parser/semantic analyzer will manage).
        }
    }

    // Leave the current scope
    void leaveScope() {
        if (scopeStack.size() > 1) { // Don't leave the global scope (index 0)
            // Restore the offset counter to what it was when entering this scope
            currentScopeLocalOffset = scopeStartOffsets.back();
            scopeStack.pop_back();
            scopeStartOffsets.pop_back();
        } else {
            std::cerr << "Warning: Attempted to leave the global scope." << std::endl;
        }
    }

    // Add a symbol to the *current* (innermost) scope.
    // Replicates the logic of the global addSymbol function.
    // Returns false if redeclaration occurs in the current scope.
    bool addSymbol(const std::string& name, const std::string& type_str, bool isConst, bool isFunction, int arraySize = 1) {
        if (scopeStack.empty()) {
            std::cerr << "Internal Error: Symbol table scope stack is empty." << std::endl;
            return false; // Should not happen
        }

        auto& currentScopeMap = scopeStack.back();
        if (currentScopeMap.count(name)) {
            std::cerr << "Error: Redeclaration of identifier '" << name << "' in the current scope." << std::endl;
            return false;
        }

        Symbol sym;
        sym.name = name;
        sym.type_str = type_str;
        sym.isConst = isConst;
        sym.isFunction = isFunction;
        sym.arraySize = (arraySize <= 0) ? 1 : arraySize; // Ensure arraySize is at least 1
        sym.isGlobal = (scopeStack.size() - 1 == globalScopeLevel);

        if (!sym.isGlobal && !isFunction) {
            // Assign the *current* offset and then increment for the next symbol
            sym.addr_offset = currentScopeLocalOffset;
            currentScopeLocalOffset += sym.arraySize * 4; // Increment by size (4 bytes per element/int)
        } else {
            // Globals or functions don't use the local offset counter
            sym.addr_offset = 0;
        }

        currentScopeMap[name] = std::move(sym); // Use std::move if Symbol is complex
        return true;
    }

    // Lookup a symbol starting from the current scope outwards to global.
    // Returns pointer to the found symbol, or nullptr if not found.
    Symbol* lookupSymbol(const std::string& name) {
        for (auto it = scopeStack.rbegin(); it != scopeStack.rend(); ++it) {
            if (it->count(name)) {
                return &((*it).at(name)); // Return pointer to the symbol in the map
            }
        }
        return nullptr; // Not found in any scope
    }

    // Helper to get a reference to the symbol (caller must check for nullptr first)
    // Provides mutable access, useful for updating symbol info (e.g., init values).
    Symbol& getSymbolRef(const std::string& name) {
        Symbol* symPtr = lookupSymbol(name);
        if (!symPtr) {
            throw std::runtime_error("Internal Error: getSymbolRef called for non-existent symbol: " + name);
        }
        return *symPtr;
    }

     // Get the *total* size allocated for local variables in the current scope stack
     // (relative to the start of the function's locals).
     // This is equivalent to the final value of `tempAddr` in the original code for a function scope.
    int getCurrentFunctionLocalSize() const {
        // Assuming this is called just before leaving the function scope,
        // currentScopeLocalOffset holds the total size accumulated.
        return currentScopeLocalOffset;
    }

    // Reset the local offset counter, typically called when entering a new function scope
    // *after* processing parameters.
    void resetLocalOffsetCounter() {
        currentScopeLocalOffset = 0;
        if (!scopeStartOffsets.empty()) {
            scopeStartOffsets.back() = 0; // Reset the base for the current function scope
        }
    }

    // Method specifically for adding function parameters, which might need
    // different offset handling (e.g., relative to $fp or a dedicated param area).
    // The original code treats params like other locals using `tempAddr`.
    // We will replicate that for now.
    bool addParameter(const std::string& name, const std::string& type_str) {
         // Replicate original: treat params like locals using the same offset counter
         return addSymbol(name, type_str, false, false, 1); // Params are scalars (arraySize=1)
    }

    // Expose the global scope map (read-only) for MIPS generator
    const std::unordered_map<std::string, Symbol>& getGlobalScope() const {
         if (scopeStack.empty()) {
             // Should not happen, return an empty map to avoid crash
             static const std::unordered_map<std::string, Symbol> emptyMap;
             return emptyMap;
         }
         return scopeStack[globalScopeLevel];
    }

    // Expose the current scope map (read-only) if needed
    const std::unordered_map<std::string, Symbol>& getCurrentScope() const {
        if (scopeStack.empty()) {
             static const std::unordered_map<std::string, Symbol> emptyMap;
             return emptyMap;
        }
        return scopeStack.back();
    }

};

// ============================================================================
// IRGenerator Class Definition & Implementation
// ============================================================================

class IRGenerator {
private:
    std::vector<Quadruple> quads;
    int tempCounter = 0; // For generating t0, t1, ...
    int labelCounter = 0; // For generating L0, L1, ...

public:
    IRGenerator() = default; // Default constructor is sufficient

    // Generates a new temporary variable name (t0, t1, etc.)
    // Note: The original code uses % 8, suggesting a limited pool, but
    //       a simple counter is often sufficient for IR generation.
    //       Let's use a simple counter first.
    std::string newTemp() {
        return "t" + std::to_string(tempCounter++);
    }

    // Generates a new unique label name (L0, L1, etc.)
    std::string newLabel() {
        return "L_" + std::to_string(labelCounter++); // Prefix L_ like original
    }

    // Adds a quadruple to the list.
    // The original 'gen' function did some operand mapping (e.g., adding $ to temps).
    // We'll keep the IR operands clean (e.g., "t0", variable names, labels)
    // and let the MIPS generator handle register mapping and address modes.
    void addQuad(const std::string& op, const std::string& arg1, const std::string& arg2, const std::string& result) {
        quads.emplace_back(op, arg1, arg2, result);
    }

     // Special overload for operations like Labels or Jumps that might only use the result field.
    void addQuad(const std::string& op, const std::string& result) {
         quads.emplace_back(op, "", "", result);
    }

    // Gets the generated list of quadruples (usually called by MIPSGenerator).
    const std::vector<Quadruple>& getQuads() const {
        return quads;
    }

    // Allows inserting quads at a specific position (needed for function prologues)
    void insertQuads(size_t position, const std::vector<Quadruple>& quadsToInsert) {
        if (position <= quads.size()) {
            quads.insert(quads.begin() + position, quadsToInsert.begin(), quadsToInsert.end());
        } else {
             std::cerr << "Error: Attempted to insert quads at invalid position " << position << std::endl;
        }
    }

    // Allows removing quads (e.g., removing prologue placeholder)
    void removeQuad(size_t position) {
         if (position < quads.size()) {
             quads.erase(quads.begin() + position);
         } else {
             std::cerr << "Error: Attempted to remove quad at invalid position " << position << std::endl;
         }
    }

    // Get current size of quads list (useful for placeholder positions)
    size_t getCurrentQuadIndex() const {
        return quads.size();
    }

    // Get a specific quad (e.g., to check placeholder)
    const Quadruple& getQuad(size_t index) const {
         if (index < quads.size()) {
            return quads[index];
         } else {
             // Return a dummy or throw? Let's throw for safety.
             throw std::out_of_range("Invalid index accessing Quadruple list.");
         }
    }
};

// ============================================================================
// Parser Class Definition (Placeholder)
// ============================================================================

class Parser {
    // ... To be implemented ...
};

// ============================================================================
// MIPSGenerator Class Definition (Placeholder)
// ============================================================================

class MIPSGenerator {
    // ... To be implemented ...
};

// ============================================================================
// Compiler Class Definition (Placeholder)
// ============================================================================

class Compiler {
    // ... To be implemented ...
}; 