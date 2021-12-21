#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <set>
#include <memory>
#include <string>
#include <vector>
#include <iostream>
using namespace llvm;
using namespace llvm::sys;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.

enum Token {
  // EOF
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number_int = -5,
  tok_number_double = -6,
  tok_char = -7,

  // operation
  tok_op = -8,

  // function
  tok_if = -9,
  tok_else = -10,
  tok_while = -11,
  tok_for = -12,
  tok_return = -13,

  // user-defined
  tok_unary_def = -14,
  tok_binary_def = -15,
};

enum Types {
  type_int = 1,     // int
  type_double = 2,  // double
  type_char = 3,    // char
  type_struct = 4,
};

enum Ops {
  op_add = 1,       // +
  op_sub = 2,       // -
  op_mul = 3,       // *
  op_lt = 4,        // <
  op_eq = 5,        // ==
  op_ne = 6,        // !=
  op_le = 7,        // <=
};


static std::map<std::string, int> TypeValues;   // Map typeString to int
static std::map<std::string, int> OpValues;     // Map opString to int
static FILE *fip;                               // File pointer
static std::string IdentifierStr;               // Filled in if tok_identifier
static int NumValI;                             // Filled in if tok_number_int
static double NumValD;                          // Filled in if tok_number_double
static int ValType;                             // Filled in if tok_def
static int CharVal;                             // Filled in if tok_char
static std::string OpVal;                       // Filled in if tok_op

static void InitializeTypeValue(){
  TypeValues["int"] = type_int;
  TypeValues["double"] = type_double;
  TypeValues["char"] = type_char;
  TypeValues["struct"] = type_struct;
}

static void InitializeOpValue(){
  OpValues["+"] = op_add;
  OpValues["-"] = op_sub;
  OpValues["*"] = op_mul;
  OpValues["<"] = op_lt;
  OpValues["=="] = op_eq;
  OpValues["!="] = op_ne;
  OpValues["<="] = op_le;
}

/// gettok - Return the next token from standard input.
static int gettok() {
  static int LastChar = ' ';
  std::map<std::string, int>::iterator iter;

  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = fgetc(fip);


  // Keyword & identifier.
  if (isalpha(LastChar)) {
    // Get next word.
    std::string word = "";
    do {
        word.push_back(LastChar);
        LastChar = fgetc(fip);
    } while (isalpha(LastChar) || isdigit(LastChar));

    // int & double & char & struct
    if (word == "int" || word == "double" || word == "char" || word == "struct") {
      ValType = TypeValues[word];
      return tok_def;
    }

    // extern
    if (word == "extern") 
      return tok_extern;

    // return
    if (word == "return")
      return tok_return;

    // if & else
    if (word == "if")
      return tok_if;
    if (word == "else")
      return tok_else;

    // while
    if (word == "while")
      return tok_while;

    // for
    if (word == "for")
      return tok_for;

    // user-defined
    if (word == "unary")
      return tok_unary_def;
    if (word == "binary")
      return tok_binary_def;

    // identifier
    IdentifierStr = word;
    return tok_identifier;
  }

  // Number.
  if (isdigit(LastChar)) {
    int integer = 0;
    double decimal = 0;
    int tok_type = tok_number_int;

    // Get next number.
    while (isdigit(LastChar) || LastChar == '.') {
      if (LastChar == '.') {
        if (tok_type == tok_number_int)     // xxx.
          tok_type = tok_number_double;
        else                                // xxx.xxx.
          // invalid input
          return 0;
      } else {
        if (tok_type == tok_number_int)     // xx
          integer = integer * 10 + (LastChar - '0');
        else                                // xxx.xx
          decimal += 0.1 * (LastChar - '0');
      }
      LastChar = fgetc(fip);
    }

    if (tok_type == tok_number_int)
      NumValI = integer;
    if (tok_type == tok_number_double)
      NumValD = integer + decimal;
    return tok_type;
  }

  // Character.
  if (LastChar == '\'') {
    LastChar = fgetc(fip);
    int character = LastChar;
    LastChar = fgetc(fip);
    if (LastChar != '\'')
      // invalid input
      return 0;
    LastChar = fgetc(fip);
    CharVal = character;
    return tok_char;
  }

  // Operator.
  if (LastChar == '+' || LastChar == '-' || LastChar == '*' || LastChar == '<') {
    std::string op = "";
    op.push_back(LastChar);
    LastChar = fgetc(fip);
    if (op == "<" && LastChar == '=') {
      op.push_back(LastChar);
      LastChar = fgetc(fip);
    }
    OpVal = op;
    return tok_op;
  }
  if (LastChar == '=' || LastChar == '!') {
    std::string op = "";
    op.push_back(LastChar);
    LastChar = fgetc(fip);
    if (LastChar == '=') {
      op.push_back(LastChar);
      LastChar = fgetc(fip);
      OpVal = op;
      return tok_op;
    }
    return op[0];
  }

  // Comment.
  if (LastChar == '#') {
    do
      LastChar = fgetc(fip);
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
    
    if (LastChar != EOF) 
      return gettok();
  }

  // Check for end of file.
  if (LastChar == EOF)
    return tok_eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = fgetc(fip);
  return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

/// StructAST - Class for struct type.
class StructAST {
  std::string Name;
  std::vector<std::string> Elements;
  std::vector<int> ElementTypes;
  std::vector<std::string> StructTypeNames;

public:
  StructAST(const std::string &Name, std::vector<std::string> Elements, 
            std::vector<int> ElementTypes, std::vector<std::string> StructTypeNames)
    : Name(Name), Elements(std::move(Elements)), 
      ElementTypes(std::move(ElementTypes)), StructTypeNames(std::move(StructTypeNames)) {}

  const std::string &getName() const { return Name; }
  Value *codegen();
};

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;

  virtual Value *codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberDoubleExprAST : public ExprAST {
  double Val;

public:
  NumberDoubleExprAST(double Val) : Val(Val) {}

  Value *codegen() override;
};

class NumberIntExprAST : public ExprAST {
  int Val;

public:
  NumberIntExprAST(int Val) : Val(Val) {}

  Value *codegen() override;
};

class CharExprAST : public ExprAST {
  int Val;

public:
  CharExprAST(int Val) : Val(Val) {}

  Value *codegen() override;
};

struct PtrAndTy {
  Value *V;
  Type *T;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;
  std::vector<std::string> Element;

public:
  VariableExprAST(const std::string &Name, std::vector<std::string> Element)
    : Name(Name), Element(std::move(Element)) {}

  PtrAndTy *getVarPtrAndTy();
  Value *codegen() override;
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST {
  std::string Op;
  std::unique_ptr<ExprAST> RHS;

public:
  UnaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> RHS)
    : Op(Op), RHS(std::move(RHS)) {}

  Value *codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  std::string Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

  Value *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}

  Value *codegen() override;
};

/// StmtAST - Base class for all statement nodes.
class StmtAST {
public:
  virtual ~StmtAST() = default;

  virtual Value *codegen() = 0;
};

/// DeclStmtAST - Statement class for declaration.
class DeclStmtAST : public StmtAST {
  int DeclType;
  std::string StructName;
  std::vector<std::string> Decls;
  std::vector<std::unique_ptr<ExprAST>> Vals;

public:
  DeclStmtAST(int DeclType, const std::string &StructName, std::vector<std::string> Decls, 
              std::vector<std::unique_ptr<ExprAST>> Vals)
    : DeclType(DeclType), StructName(StructName), Decls(Decls), Vals(std::move(Vals)) {}

  Value *codegen();
};

/// SimpStmtAST - Statement class for simple statement "=".
class SimpStmtAST : public StmtAST {
  std::unique_ptr<VariableExprAST> Var;
  std::unique_ptr<ExprAST> RHS;

public:
  SimpStmtAST(std::unique_ptr<VariableExprAST> Var, std::unique_ptr<ExprAST> RHS)
    : Var(std::move(Var)), RHS(std::move(RHS)) {}

  Value *codegen();  
};

/// BlockAST - This class represents the block in control flow.
class BlockAST {
  std::vector<std::unique_ptr<StmtAST>> Stmts;

  public:
  BlockAST(std::vector<std::unique_ptr<StmtAST>> Stmts)
      : Stmts(std::move(Stmts)) {}

  Value *codegen();  
};

/// IfStmtAST - Statement class for if control flow.
class IfStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> Cond;
  std::unique_ptr<BlockAST> Then;
  std::unique_ptr<BlockAST> Else;

public:
  IfStmtAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<BlockAST> Then, std::unique_ptr<BlockAST> Else)
    : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  Value *codegen() override;
};

/// WhileStmtAST - Statement class for while control flow.
class WhileStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> Cond;
  std::unique_ptr<BlockAST> Loop;

public:
  WhileStmtAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<BlockAST> Loop)
    : Cond(std::move(Cond)), Loop(std::move(Loop)) {}

  Value *codegen() override;
};

/// ForStmtAST - Statement class for for control flow.
class ForStmtAST : public StmtAST {
  std::unique_ptr<StmtAST> Decl;
  std::unique_ptr<ExprAST> Cond;
  std::unique_ptr<StmtAST> Simp;
  std::unique_ptr<BlockAST> Loop;

public:
  ForStmtAST(std::unique_ptr<StmtAST> Decl, std::unique_ptr<ExprAST> Cond, 
  std::unique_ptr<StmtAST> Simp, std::unique_ptr<BlockAST> Loop)
    : Decl(std::move(Decl)), Cond(std::move(Cond)), Simp(std::move(Simp)), Loop(std::move(Loop)) {}

  Value *codegen() override;
};

/// RetStmtAST - Statement class for return statement.
class RetStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> Ret;

public:
  RetStmtAST(std::unique_ptr<ExprAST> Ret)
    : Ret(std::move(Ret)) {}

  Value *codegen();  
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;
  std::vector<int> ArgTypes;
  std::vector<std::string> StructTypeNames;
  int FnType;
  std::string FnStructTypeName;

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args, std::vector<int> ArgTypes, 
              std::vector<std::string> StructTypeNames, int FnType, const std::string &FnStructTypeName)
      : Name(Name), Args(std::move(Args)), ArgTypes(std::move(ArgTypes)), 
              StructTypeNames(std::move(StructTypeNames)), FnType(FnType), FnStructTypeName(FnStructTypeName) {}

  Function *codegen();
  const std::string &getName() const { return Name; }
  const int getReturnType() {return FnType;}
  const std::vector<int> &getArgTypes() {return ArgTypes;}
  const std::vector<std::string> &getStructTypeNames() {return StructTypeNames;}
};

/// BodyAST - This class represents the body for a function.
class BodyAST {
  std::vector<std::unique_ptr<StmtAST>> Stmts;

public:
  BodyAST(std::vector<std::unique_ptr<StmtAST>> Stmts)
      : Stmts(std::move(Stmts)) {}

  Value *codegen();      
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<BodyAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<BodyAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}

  Function *codegen();
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }


/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<int, int> BinopPrecedence;

/// DefUnopProto - This holds the name for each user-defined unary operator.
static std::set<char> DefUnopNames;

/// DefBinopProto - This holds the prototype for each user-defined binary operator.
static std::set<char> DefBinopNames;

/// DefOpPrecedence - This holds the precedence for each user-defined operator.
static std::map<char, int> DefOpPrecedence;


/// isUnop - Return if this token is an unary operator.
static bool isUnop(int tok) {
  return DefUnopNames.find(tok) != DefUnopNames.end();
}

/// isBinop - Return if this token is a binary operator.
static bool isBinop(int tok) {
  return (tok == tok_op) || (DefBinopNames.find(tok) != DefBinopNames.end());
}

/// getBinop - Get the name of the operator the token represents.
static std::string getBinop(int tok) {
  if (tok == tok_op)
    return OpVal;
  if (DefBinopNames.find(tok) != DefBinopNames.end()) {
    std::string Op = "";
    Op.push_back(tok);
    return Op;
  }
  // Not found.
  return "";
}

/// getOpPrecedence - Get the precedence of the operator the token represents.
static int getOpPrecedence(int tok) {
  if (tok == tok_op)
    return BinopPrecedence[OpValues[OpVal]];
  return DefOpPrecedence[tok];
}


/// getStructName - Return struct name if this is a struct; "" instead.
std::string getStructName() {
  if (ValType == type_struct) {
    getNextToken(); // eat "struct"
    return IdentifierStr;
  }
  return "";
}


/// parseOnceParam - Parse once parameter, e.g., int a.
void parseOnceParam(std::vector<int> &ArgTypes, std::vector<std::string> &StructTypeNames,
                    std::vector<std::string> &ArgNames) {
  if (CurTok != tok_def)
    return ;
  ArgTypes.push_back(ValType);
  StructTypeNames.push_back(getStructName());
  getNextToken(); // eat <type>

  if (CurTok != tok_identifier) {
    ArgNames.push_back("");
  } else {
    ArgNames.push_back(IdentifierStr);
    getNextToken(); // eat <ident>
  }
}


/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

std::unique_ptr<FunctionAST> LogErrorF(const char *Str) {
  LogError(Str);
  return nullptr;
}

std::unique_ptr<BlockAST> LogErrorBlock(const char *Str) {
  LogError(Str);
  return nullptr;
}

std::unique_ptr<BodyAST> LogErrorB(const char *Str) {
  LogError(Str);
  return nullptr;
}

std::unique_ptr<StmtAST> LogErrorS(const char *Str) {
  LogError(Str);
  return nullptr;
}

std::unique_ptr<StructAST> LogErrorStruct(const char *Str) {
  LogError(Str);
  return nullptr;
}


static std::unique_ptr<ExprAST> ParseExpression(int precedence);
static std::unique_ptr<StmtAST> ParseStatement();


/// struct
static std::unique_ptr<StructAST> ParseStruct() {
  getNextToken(); // eat "struct"

  if (CurTok != tok_identifier)
    return LogErrorStruct("Expected valid struct name");
  std::string Name = IdentifierStr;
  getNextToken(); // eat <ident>

  if (CurTok != '{')
    return LogErrorStruct("Expected '{' in struct definition");
  getNextToken(); // eat '{'

  std::vector<std::string> Elements;
  std::vector<int> ElementTypes;
  std::vector<std::string> StructTypeNames;
  while (CurTok != '}') {
    parseOnceParam(ElementTypes, StructTypeNames, Elements);
    if (CurTok != ';')
      return LogErrorStruct("Expected ';' after a element");
    getNextToken(); // eat ';'
  }

  getNextToken(); // eat '}'
  if (CurTok != ';')
    return LogErrorStruct("Expected ';' after a element");
  getNextToken(); // eat ';'

  return std::make_unique<StructAST>(Name, std::move(Elements), std::move(ElementTypes), std::move(StructTypeNames));
}


/// <intconst> & <doubleconst>
static std::unique_ptr<ExprAST> ParseNumberExpr(int NumberType) {
  if (NumberType == type_double){
    auto Result = std::make_unique<NumberDoubleExprAST>(NumValD);
    getNextToken(); // eat <number>
    return Result;
  } else {
    auto Result = std::make_unique<NumberIntExprAST>(NumValI);
    getNextToken(); // eat <number>
    return Result;
  }
}


/// <charconst>
static std::unique_ptr<ExprAST> ParseCharExpr() {
  auto Result = std::make_unique<CharExprAST>(CharVal);
  getNextToken(); // eat <char>
  return Result;
}


/// <var> & <callee>
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string ident = IdentifierStr;
  getNextToken(); // eat <ident>

  if (CurTok != '(') {
    std::vector<std::string> Element;
    while (CurTok == '.') {
      getNextToken(); // eat '.'

      if (CurTok != tok_identifier)
        return LogError("Expected '.' when using struct element");
      Element.push_back(IdentifierStr);
      getNextToken(); // eat <ident>
    }
    return std::make_unique<VariableExprAST>(ident, std::move(Element));
  }
  getNextToken(); // eat '('

  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    Args.push_back(ParseExpression(0));
    while (CurTok == ',') {
      getNextToken(); // eat ','
      Args.push_back(ParseExpression(0));
    }
  }

  if (CurTok != ')')
    return LogError("Expected ')' in callee");
  getNextToken(); // eat ')'

  return std::make_unique<CallExprAST>(ident, std::move(Args));
}


/// <unop><exp>
static std::unique_ptr<ExprAST> ParseUnaryExpr() {
  std::string Op = "";
  Op.push_back(CurTok);
  getNextToken(); // eat <unop>

  auto E = ParseExpression(0);

  return std::make_unique<UnaryExprAST>(Op, std::move(E));
}


/// <exp>
static std::unique_ptr<ExprAST> ParseExpression(int precedence) {
  std::unique_ptr<ExprAST> Result;
  switch (CurTok)
  {
  case '(':
    getNextToken(); // eat '('
    Result = ParseExpression(0);
    if (CurTok != ')')
      return LogError("Expected ')' in expression");
    getNextToken(); // eat ')'
    break;
  case tok_number_int:
    Result = ParseNumberExpr(type_int);
    break;
  case tok_number_double:
    Result = ParseNumberExpr(type_double);
    break;
  case tok_char:
    Result = ParseCharExpr();
    break;
  case tok_identifier:
    Result = ParseIdentifierExpr();
    break;
  default:
    if (!isUnop(CurTok))
      return LogError("Invalid expression input");
    Result = ParseUnaryExpr();
    break;
  }

  while (isBinop(CurTok)) {
    std::string op = getBinop(CurTok);
    int rp = getOpPrecedence(CurTok);
    int lp = rp - 1;
    if (lp < precedence)
      break;
    getNextToken();
    auto Right = ParseExpression(rp);
    Result = std::make_unique<BinaryExprAST>(op, std::move(Result), std::move(Right));
  }

  return Result;
}


/// <decl>
static std::unique_ptr<StmtAST> ParseDeclStatement() {
  int DeclType = ValType;
  std::string StructName = getStructName();

  std::vector<std::string> DeclNames;
  std::vector<std::unique_ptr<ExprAST>> Vals;

  do {
    getNextToken(); // first eat <type>, then eat ','
    if (CurTok != tok_identifier)
      return LogErrorS("Expected variable name in declaration");
    DeclNames.push_back(IdentifierStr);
    getNextToken(); // eat <ident>
    if (CurTok == '=') {
      getNextToken(); // eat '='
      Vals.push_back(ParseExpression(0));
    } else {
      Vals.push_back(nullptr);
    }
  } while (CurTok == ',');

  return std::make_unique<DeclStmtAST>(DeclType, StructName, DeclNames, std::move(Vals));
}


/// <simp>
static std::unique_ptr<StmtAST> ParseSimpStatement() {
  std::string ident = IdentifierStr;
  getNextToken(); // eat <ident>

  std::vector<std::string> Element;
  while (CurTok == '.') {
    getNextToken(); // eat '.'

    if (CurTok != tok_identifier)
      return LogErrorS("Expected '.' when using struct element");
    Element.push_back(IdentifierStr);
    getNextToken(); // eat <ident>
  }
  auto V = std::make_unique<VariableExprAST>(ident, std::move(Element));
  
  if (CurTok != '=')
    return LogErrorS("Expected '=' in simple statement");
  getNextToken(); // eat '='

  auto E = ParseExpression(0);

  return std::make_unique<SimpStmtAST>(std::move(V), std::move(E));
}


/// <block>
static std::unique_ptr<BlockAST> ParseBlock() {
  std::vector<std::unique_ptr<StmtAST>> Stmts;
  if (CurTok != '{') {
    auto S = ParseStatement();
    Stmts.push_back(std::move(S));
  } else {
    getNextToken(); // eat '{'
    while (CurTok != '}')
    {
      auto S = ParseStatement();
      if (S == nullptr)
        return nullptr;
      Stmts.push_back(std::move(S));
    }
    getNextToken(); // eat '}'
  }
  return std::make_unique<BlockAST>(std::move(Stmts));
}


/// if statement in <control>
static std::unique_ptr<StmtAST> ParseIfStatement() {
  getNextToken(); // eat "if"

  if (CurTok != '(')
    return LogErrorS("Expected '(' in if statement");
  getNextToken(); // eat '('

  auto Cond = ParseExpression(0);

  if (CurTok != ')')
    return LogErrorS("Expected ')' in if statement");
  getNextToken(); // eat ')'

  auto Then = ParseBlock();

  std::unique_ptr<BlockAST> Else = nullptr;
  if (CurTok == tok_else) {
    getNextToken(); // eat "else"
    Else = ParseBlock();
  }

  return std::make_unique<IfStmtAST>(std::move(Cond), std::move(Then), std::move(Else));
}


/// while statement in <control>
static std::unique_ptr<StmtAST> ParseWhileStatement() {
  getNextToken(); // eat "while"

  if (CurTok != '(')
    return LogErrorS("Expected '(' in while statement");
  getNextToken(); // eat '('

  auto Cond = ParseExpression(0);

  if (CurTok != ')')
    return LogErrorS("Expected ')' in while statement");
  getNextToken(); // eat ')'

  auto Loop = ParseBlock();

  return std::make_unique<WhileStmtAST>(std::move(Cond), std::move(Loop));
}


/// for statement in <control>
static std::unique_ptr<StmtAST> ParseForStatement() {
  getNextToken(); // eat "for"

  if (CurTok != '(')
    return LogErrorS("Expected '(' in for statemet");
  getNextToken(); // eat '('

  auto Decl = ParseDeclStatement();

  if (CurTok != ';')
    return LogErrorS("Expected ';' in for statement");
  getNextToken(); // eat ';'

  auto Cond = ParseExpression(0);

  if (CurTok != ';')
    return LogErrorS("Expected ';' in for statement");
  getNextToken(); // eat ';'

  auto Simp = ParseSimpStatement();

  if (CurTok != ')')
    return LogErrorS("Expected ')' in for statement");
  getNextToken(); // eat ')'

  auto Loop = ParseBlock();

  return std::make_unique<ForStmtAST>(std::move(Decl), std::move(Cond), std::move(Simp), std::move(Loop));
}


/// <return>
static std::unique_ptr<StmtAST> ParseRetStatement() {
  getNextToken(); // eat "return"
  auto E = ParseExpression(0);
  return std::make_unique<RetStmtAST>(std::move(E));
}


/// <stmt>
static std::unique_ptr<StmtAST> ParseStatement() {
  std::unique_ptr<StmtAST> Result;
  switch (CurTok)
  {
  case tok_def:
    Result = ParseDeclStatement();
    if (CurTok != ';')
      return LogErrorS("Expected ';' after a statement");
    getNextToken(); // eat ';'
    break;
  case tok_identifier:
    Result = ParseSimpStatement();
    if (CurTok != ';')
      return LogErrorS("Expected ';' after a statement");
    getNextToken(); // eat ';'
    break;
  case tok_return:
    Result = ParseRetStatement();
    if (CurTok != ';')
      return LogErrorS("Expected ';' after a statement");
    getNextToken(); // eat ';'
    break;
  case tok_if:
    Result = ParseIfStatement();
    break;
  case tok_while:
    Result = ParseWhileStatement();
    break;
  case tok_for:
    Result = ParseForStatement();
    break;
  default:
    return LogErrorS("Invalid statement input");
  }
  return Result;
}


/// <prototype>
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  int FnType = ValType;
  std::string FnStructTypeName = getStructName();
  
  getNextToken(); // eat <type>
  if (CurTok != tok_identifier)
    return LogErrorP("Expected function name in prototype");
  
  std::string FnName = IdentifierStr;
  getNextToken(); // eat <ident>

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");
  getNextToken(); // eat '('

  std::vector<std::string> ArgNames;
  std::vector<int> ArgTypes;
  std::vector<std::string> StructTypeNames;
  if (CurTok == tok_def) {
    parseOnceParam(ArgTypes, StructTypeNames, ArgNames);
    while (CurTok == ',') {
      getNextToken(); // eat ','
      parseOnceParam(ArgTypes, StructTypeNames, ArgNames);
    }
  }
  
  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");
  getNextToken(); // eat ')'

  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), std::move(ArgTypes), 
                                        std::move(StructTypeNames), FnType, FnStructTypeName);
}


/// <body>
static std::unique_ptr<BodyAST> ParseBody() {
  if (CurTok != '{')
    return LogErrorB("Expected '{' in function body");
  getNextToken(); // eat '{'
  std::vector<std::unique_ptr<StmtAST>> Stmts;
  while (CurTok != '}')
  {
    auto S = ParseStatement();
    if (S == nullptr)
      return nullptr;
    Stmts.push_back(std::move(S));
  }
  getNextToken(); // eat '}'
  return std::make_unique<BodyAST>(std::move(Stmts));
}


/// <function>
static std::unique_ptr<FunctionAST> ParseDefinition() {
  auto Proto = ParsePrototype();
  auto Body = ParseBody();
  return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
}


/// binary operator definition in <opdef>
static std::unique_ptr<FunctionAST> ParseBinopDef() {
  getNextToken(); // eat "binary"

  std::string FnName = "";
  FnName.push_back(CurTok);
  getNextToken(); // eat <any>

  if (CurTok != tok_number_int) {
    return LogErrorF("Expected precedence in binary operator definition");
  }
  int Pre = NumValI;
  getNextToken(); // eat <intconst>

  if (CurTok != tok_def) {
    return LogErrorF("Expected type in binary operator definition");
  }
  int FnType = ValType;
  std::string FnStructTypeName = getStructName();
  getNextToken(); // eat <type>

  if (CurTok != '(') {
    return LogErrorF("Expected '(' in binary operator definition");
  }
  getNextToken(); // eat '('

  std::vector<int> ArgTypes;
  std::vector<std::string> StructTypeNames;
  std::vector<std::string> ArgNames;

  parseOnceParam(ArgTypes, StructTypeNames, ArgNames);

  if (CurTok != ',') {
    return LogErrorF("Lack of arguments in binary operator definition");
  }
  getNextToken(); // eat ','

  parseOnceParam(ArgTypes, StructTypeNames, ArgNames);

  if (CurTok != ')') {
    return LogErrorF("Expected ')' in binary operator definition");
  }
  getNextToken(); // eat ')'

  DefBinopNames.insert(FnName[0]);
  DefOpPrecedence[FnName[0]] = Pre;

  auto Proto = std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), std::move(ArgTypes), 
                                              std::move(StructTypeNames),FnType, FnStructTypeName);
  auto Body = ParseBody();
  return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
}


/// unary operator definition in <opdef>
static std::unique_ptr<FunctionAST> ParseUnopDef() {
  getNextToken(); // eat "unary"

  std::string FnName = "";
  FnName.push_back(CurTok);
  getNextToken(); // eat <any>

  if (CurTok != tok_number_int) {
    return LogErrorF("Expected precedence in unary operator definition");
  }
  int Pre = NumValI;
  getNextToken(); // eat <intconst>

  if (CurTok != tok_def) {
    return LogErrorF("Expected type in unary operator definition");
  }
  int FnType = ValType;
  std::string FnStructTypeName = getStructName();
  getNextToken(); // eat <type>

  if (CurTok != '(') {
    return LogErrorF("Expected '(' in unary operator definition");
  }
  getNextToken(); // eat '('

  std::vector<int> ArgTypes;
  std::vector<std::string> StructTypeNames;
  std::vector<std::string> ArgNames;

  parseOnceParam(ArgTypes, StructTypeNames, ArgNames);

  if (CurTok != ')') {
    return LogErrorF("Expected ')' in unary operator definition");
  }
  getNextToken(); // eat ')'

  DefUnopNames.insert(FnName[0]);
  DefOpPrecedence[FnName[0]] = Pre;

  auto Proto = std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), std::move(ArgTypes), 
                                              std::move(StructTypeNames), FnType, FnStructTypeName);
  auto Body = ParseBody();
  return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
}


/// <gdecl>
static std::unique_ptr<PrototypeAST> ParseExtern() {
  int isdef = getNextToken(); // eat "extern"
  if (isdef != tok_def)
    return LogErrorP("Expected type declaration");
  auto Proto = ParsePrototype();
  if (CurTok != ';')
    return LogErrorP("Expected ';' in global declaration");
  getNextToken(); // eat ';'
  return Proto;
}


//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
// Struct
static std::map<std::string, StructType*> StructTypes;                      // struct name to struct type
static std::map<std::string, std::vector<std::string>*> StructElementNames; // struct name to struct element names
static std::stack<std::map<std::string, std::string>*> OldStructValues;     // hidden struct variables
static std::map<std::string, std::string> StructValues;                     // visible struct variables       
// Block
static std::stack<std::set<std::string>*> BlockValueNames;                  // block local variables
static std::stack<bool> BlockReturned;                                      // if the block has returned
static std::stack<std::map<std::string, AllocaInst*>*> OldNamedValues;      // hidden variables
static std::map<std::string, AllocaInst*> NamedValues;                      // visible variables
// Function
static bool HasReturned;                                                    // if the function has returned
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;


Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

Function *LogErrorFn(const char *Str) {
  LogError(Str);
  return nullptr;
}


// getFunction(Name) can return a Function structure variable, F, to caller, which can 
// be used to creat a callee statement in codegen.
Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = TheModule->getFunction(Name))
    return F;

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return FI->second->codegen();

  // If no existing prototype exists, return null.
  return nullptr;
}


/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          const std::string &VarName,
                                          Type *ty) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                  TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(ty, nullptr, VarName.c_str());
}


/// TypeMerge - Merge two type with the following rules:
/// int ~ int -> int          int ~ double -> double
/// double ~ int -> double    double ~ double -> double
static bool TypeMerge(Value* &L, Value* &R) {
  bool isIntOp;
  if (L->getType()->isDoubleTy()) {
    if (R->getType()->isIntegerTy()) 
      R = Builder->CreateUIToFP(R, Type::getDoubleTy(*TheContext), "tmp");
    isIntOp = false;
  } else {
    if (R->getType()->isDoubleTy()) {
      L = Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "tmp");
      isIntOp = false;
    } else
      isIntOp = true;
  }
  return isIntOp;
}

/// TypeCast - Cast the type of R to the type T.
/// Assume that Value can only be IntegerTy or DoubleTy.
static void TypeCast(Type *T, Value* &R) {
  if (T->isIntegerTy() && R->getType()->isDoubleTy())
    R = Builder->CreateFPToUI(R, Type::getInt32Ty(*TheContext), "tmp");
  if (T->isDoubleTy() && R->getType()->isIntegerTy())
    R = Builder->CreateUIToFP(R, Type::getDoubleTy(*TheContext), "tmp");
}


/// EnterBlock - Enter a block.
static void EnterBlock(std::set<std::string> *scopeNames, 
                      std::map<std::string, AllocaInst*> *scopeValues,
                      std::map<std::string, std::string> *scopeStructValues) {
  BlockValueNames.push(scopeNames);
  OldNamedValues.push(scopeValues);
  OldStructValues.push(scopeStructValues);
}

/// LeaveBlock - Leave a block.
/// Restore the variables hidden by block local variables.
static void LeaveBlock(std::set<std::string> *scopeNames, 
                      std::map<std::string, AllocaInst*> *scopeValues,
                      std::map<std::string, std::string> *scopeStructValues) {
  for (auto name : *scopeNames) {
    AllocaInst *val = (*scopeValues)[name];
    if (val) {
      NamedValues[name] = val;
      StructValues[name] = (*scopeStructValues)[name];
    } else {
      NamedValues.erase(name);
      StructValues.erase(name);
    }
  }
  BlockValueNames.pop();
  OldNamedValues.pop();
  OldStructValues.pop();
}


Value *StructAST::codegen() {
  StructType *structType = StructType::create(*TheContext, Name);
  std::vector<Type*> elements;
  std::vector<std::string> *elementNames = new std::vector<std::string>;
  int size = Elements.size();
  for (int i = 0; i < size; i++) {
    Type *T;
    switch (ElementTypes[i]) {
      case type_int:
        T = Type::getInt32Ty(*TheContext);
        break;
      case type_double:
        T = Type::getDoubleTy(*TheContext);
        break;
      case type_char:
        T = Type::getInt32Ty(*TheContext);
        break;
      case type_struct:
        T = StructTypes[StructTypeNames[i]];
        break;
      default:
        return LogErrorFn("Invalid return value type");
    }
    elements.push_back(T);
    elementNames->push_back(Elements[i]);
  }
  structType->setBody(elements);

  StructTypes[Name] = structType;
  StructElementNames[Name] = elementNames;
  
  return nullptr;
}


Value *NumberDoubleExprAST::codegen() {
  return ConstantFP::get(*TheContext, APFloat(Val));
}


Value *NumberIntExprAST::codegen() {
  return ConstantInt::get(*TheContext, APInt(32,Val));
}


Value *CharExprAST::codegen() {
  return ConstantInt::get(*TheContext, APInt(32,Val));
}


PtrAndTy *VariableExprAST::getVarPtrAndTy() {
  struct PtrAndTy *R = new struct PtrAndTy;

  if (Element.empty()) {
    AllocaInst *V = NamedValues[Name];
    if (!V)
        return nullptr;
    R->V = V;
    R->T = V->getAllocatedType();
    return R;
  }

  Value *V = NamedValues[Name];
  if (!V)
      return nullptr;

  std::string StructName = StructValues[Name];
  Type *T;
  for (std::string str : Element) {
    std::vector<std::string> *ElementNames = StructElementNames[StructName];
    int size = ElementNames->size(), i = 0;
    for ( ; i < size; i++)
      if (str == ElementNames->at(i))
        break;
    Value *const_0 = ConstantInt::get(IntegerType::getInt32Ty(*TheContext), 0);
    Value *const_index = ConstantInt::get(IntegerType::getInt32Ty(*TheContext), i);
    SmallVector<Value*, 2> indexVector;
    indexVector.push_back(const_0);
    indexVector.push_back(const_index);
    
    V = Builder->CreateGEP(V, indexVector);
    T = StructTypes[StructName]->getElementType(i);
    if (T->isStructTy())
      StructName = T->getStructName().str();
  }

  R->V = V;
  R->T = T;
  return R;
}


Value *VariableExprAST::codegen() {
  PtrAndTy *VT = getVarPtrAndTy();
  return Builder->CreateLoad(VT->T, VT->V, Name.c_str());
}


Value *UnaryExprAST::codegen() {
  Value *R = RHS->codegen();
  if (!R)
    return nullptr;
  
  Function *CalleeF = TheModule->getFunction(Op);
  if (!CalleeF)
    return LogErrorV("Unknown unary operation");

  if (CalleeF->arg_size() != 1)
    return LogErrorV("Arguments size for unary operation must be 1");

  std::vector<Value *> ArgValues;

  Type *T = CalleeF->getArg(0)->getType();
  TypeCast(T, R);
  ArgValues.push_back(R);

  return Builder->CreateCall(CalleeF, ArgValues, "calltmp");
}


Value *BinaryExprAST::codegen() {
  Value *L = LHS->codegen();
  if (!L)
    return nullptr;
  Value *R = RHS->codegen();
  if (!R)
    return nullptr;

  switch (OpValues[Op]) {
    case op_add:
      if (TypeMerge(L, R)) 
        return Builder->CreateAdd(L, R, "addtmp");
      else
        return Builder->CreateFAdd(L, R, "addtmp");
    case op_sub:
      if (TypeMerge(L, R))
        return Builder->CreateSub(L, R, "subtmp");
      else
        return Builder->CreateFSub(L, R, "subtmp");
    case op_mul:
      if (TypeMerge(L, R))
        return Builder->CreateMul(L, R, "multmp");
      else
        return Builder->CreateFMul(L, R, "multmp");
    case op_lt:
      // Lack of bool type (i1), so use "double" type as cmp return type...
      if (TypeMerge(L, R))
        return Builder->CreateUIToFP(Builder->CreateICmpULT(L, R, "cmptmp"), Type::getDoubleTy(*TheContext), "booltmp");
      else
        return Builder->CreateUIToFP(Builder->CreateFCmpULT(L, R, "cmptmp"), Type::getDoubleTy(*TheContext), "booltmp");
    case op_eq:
      if (TypeMerge(L, R))
        return Builder->CreateUIToFP(Builder->CreateICmpEQ(L, R, "cmptmp"), Type::getDoubleTy(*TheContext), "booltmp");
      else
        return Builder->CreateUIToFP(Builder->CreateFCmpUEQ(L, R, "cmptmp"), Type::getDoubleTy(*TheContext), "booltmp");
    case op_ne:
      if (TypeMerge(L, R))
        return Builder->CreateUIToFP(Builder->CreateICmpNE(L, R, "cmptmp"), Type::getDoubleTy(*TheContext), "booltmp");
      else
        return Builder->CreateUIToFP(Builder->CreateFCmpUNE(L, R, "cmptmp"), Type::getDoubleTy(*TheContext), "booltmp");
    case op_le:
      if (TypeMerge(L, R))
        return Builder->CreateUIToFP(Builder->CreateICmpULE(L, R, "cmptmp"), Type::getDoubleTy(*TheContext), "booltmp");
      else
        return Builder->CreateUIToFP(Builder->CreateFCmpULE(L, R, "cmptmp"), Type::getDoubleTy(*TheContext), "booltmp");
    default:
      Function *CalleeF = TheModule->getFunction(Op);
      if (!CalleeF)
        return LogErrorV("Unknown binary opreation");

      if (CalleeF->arg_size() != 2)
        return LogErrorV("Arguments size for binary operation must be 2");

      std::vector<Value *> ArgValues;

      Type *FT = CalleeF->getArg(0)->getType();
      TypeCast(FT, L);
      ArgValues.push_back(L);

      Type *ST = CalleeF->getArg(1)->getType();
      TypeCast(ST, R);
      ArgValues.push_back(R);

      return Builder->CreateCall(CalleeF, ArgValues, "calltmp");
  }
}


Value *CallExprAST::codegen() {
  Function *CalleeF = TheModule->getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function call");

  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Arguments size error");

  std::vector<Value *> ArgValues;

  int size = Args.size();
  for (int i = 0; i < size; i++) {
    Value *R = Args[i]->codegen();
    Type *T = CalleeF->getArg(i)->getType();
    TypeCast(T, R);
    ArgValues.push_back(R);
    if (!ArgValues.back())
      return nullptr;
  }

  return Builder->CreateCall(CalleeF, ArgValues, "calltmp");
}


Function *PrototypeAST::codegen() {
  llvm::Type *Result;
  switch (FnType) {
    case type_int:
      Result = Type::getInt32Ty(*TheContext);
      break;
    case type_double:
      Result = Type::getDoubleTy(*TheContext);
      break;
    case type_char:
      Result = Type::getInt32Ty(*TheContext);
      break;
    case type_struct:
      Result = PointerType::get(StructTypes[FnStructTypeName], 0);
      break;
    default:
      return LogErrorFn("Invalid return value type");
  }
  
  std::vector<Type *> Params;
  int size = Args.size();
  for (int i = 0; i < size; i++) {
    switch (ArgTypes[i]) {
      case type_int:
        Params.push_back(Type::getInt32Ty(*TheContext));
        break;
      case type_double:
        Params.push_back(Type::getDoubleTy(*TheContext));
        break;
      case type_char:
        Params.push_back(Type::getInt32Ty(*TheContext));
        break;
      case type_struct:
        Params.push_back(PointerType::get(StructTypes[StructTypeNames[i]], 0));
        break;
      default:
        return LogErrorFn("Invalid parameter value type");
    }
  }

  FunctionType *FT =
      FunctionType::get(Result, Params, false);
  Function *F =
      Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++]);

  return F;
}


Value *BodyAST::codegen() {
  Value *Result;
  for (auto &S : Stmts) {
    Result = S->codegen();
    if (HasReturned) break;
  }
  return Result;
}


Value *DeclStmtAST::codegen() {
  llvm::Type *ty;
  if (DeclType == type_int || DeclType == type_char)
    ty = Type::getInt32Ty(*TheContext);
  if (DeclType == type_double)
    ty = Type::getDoubleTy(*TheContext);
  if (DeclType == type_struct)
    ty = StructTypes[StructName];

  int size = Decls.size();
  for (int i = 0; i < size; i++) {
    // Compute right value first.
    Value *R;
    if (Vals[i] != nullptr) {
      R = Vals[i]->codegen();
      TypeCast(ty, R);
    }

    auto Decl = Decls[i];
    // Get the current function.
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    // Create an alloca for this variable.
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Decl, ty);
    if (!BlockValueNames.empty()) {
      std::set<std::string> *scopeNames = BlockValueNames.top();
      if (scopeNames->find(Decl) == scopeNames->end()) {
        // Decl not exist in scopeNames
        // Record this scope variable into block variable symbol table.
        scopeNames->insert(Decl);
        // Restore the existing variable with the same name.
        AllocaInst *OldValue = NamedValues[Decl];
        if (OldValue) {
          (*(OldNamedValues.top()))[Decl] = OldValue;
          (*(OldStructValues.top()))[Decl] = StructValues[Decl];
        }
      }
    }
    // Add arguments to variable symbol table.
    NamedValues[Decl] = Alloca;
    StructValues[Decl] = StructName;

    // Initialize
    if (Vals[i] != nullptr)
      Builder->CreateStore(R, Alloca);
  }

  return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}


Value *SimpStmtAST::codegen() {
  Value *R = RHS->codegen();
  if (!R)
    return nullptr;

  PtrAndTy *VT = Var->getVarPtrAndTy();
  Value *Variable = VT->V;
  if (!Variable)
    return LogErrorV("Unknown variable name");
  
  Type *T = VT->T;
  TypeCast(T, R);

  Builder->CreateStore(R, Variable);

  return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}


Value *BlockAST::codegen() {
  Value *Result;
  for (auto &S : Stmts) {
    Result = S->codegen();
    if (BlockReturned.top()) break;
  }
  return Result;
}


Value *IfStmtAST::codegen() {
  Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else cases. Insert the 'then' block at the
  // end of the function.
  BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
  BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else", TheFunction);
  BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont", TheFunction);

  // Compute the value of he condition statement.
  Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;
  if (CondV->getType()->isIntegerTy())
    CondV = Builder->CreateUIToFP(CondV, Type::getDoubleTy(*TheContext), "tmp");
  // Convert condition to a bool by comparing non-equal to 0.0.
  CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");
  Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then block.
  Builder->SetInsertPoint(ThenBB);
  BlockReturned.push(false);

  std::set<std::string> scopeNames;
  std::map<std::string, AllocaInst*> scopeValues;
  std::map<std::string, std::string> scopeStructValues;
  EnterBlock(&scopeNames, &scopeValues, &scopeStructValues);
  Then->codegen();
  LeaveBlock(&scopeNames, &scopeValues, &scopeStructValues);

  if (!BlockReturned.top())
    Builder->CreateBr(MergeBB);
  BlockReturned.pop();

  // Emit else block.
  Builder->SetInsertPoint(ElseBB);
  BlockReturned.push(false);
  
  if (Else) {
    std::set<std::string> scopeNames;
    std::map<std::string, AllocaInst*> scopeValues;
    std::map<std::string, std::string> scopeStructValues;
    EnterBlock(&scopeNames, &scopeValues, &scopeStructValues);
    Else->codegen();
    LeaveBlock(&scopeNames, &scopeValues, &scopeStructValues);
  }

  if (!BlockReturned.top())
    Builder->CreateBr(MergeBB);
  BlockReturned.pop();

  // Emit merge block.
  Builder->SetInsertPoint(MergeBB);

  return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}


Value *WhileStmtAST::codegen() {
  Function *TheFunction = Builder->GetInsertBlock()->getParent();

  BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);
  BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "after", TheFunction);

  Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;
  if (CondV->getType()->isIntegerTy())
    CondV = Builder->CreateUIToFP(CondV, Type::getDoubleTy(*TheContext), "tmp");
  CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");
  Builder->CreateCondBr(CondV, LoopBB, AfterBB);

  Builder->SetInsertPoint(LoopBB);
  BlockReturned.push(false);

  std::set<std::string> scopeNames;
  std::map<std::string, AllocaInst*> scopeValues;
  std::map<std::string, std::string> scopeStructValues;
  EnterBlock(&scopeNames, &scopeValues, &scopeStructValues);
  Loop->codegen();
  LeaveBlock(&scopeNames, &scopeValues, &scopeStructValues);

  if (!BlockReturned.top()) {

  CondV = Cond->codegen();
  if (!CondV)
    return nullptr;
  if (CondV->getType()->isIntegerTy())
    CondV = Builder->CreateUIToFP(CondV, Type::getDoubleTy(*TheContext), "tmp");
  CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");
  Builder->CreateCondBr(CondV, LoopBB, AfterBB);

  }
  BlockReturned.pop();

  Builder->SetInsertPoint(AfterBB);

  return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}


Value *ForStmtAST::codegen() {
  Function *TheFunction = Builder->GetInsertBlock()->getParent();

  BasicBlock *InitBB = BasicBlock::Create(*TheContext, "init", TheFunction);
  BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);
  BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "after", TheFunction);

  Builder->CreateBr(InitBB);

  Builder->SetInsertPoint(InitBB);

  std::set<std::string> scopeNames;
  std::map<std::string, AllocaInst*> scopeValues;
  std::map<std::string, std::string> scopeStructValues;
  EnterBlock(&scopeNames, &scopeValues, &scopeStructValues);
  
  Decl->codegen();

  Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;
  if (CondV->getType()->isIntegerTy())
    CondV = Builder->CreateUIToFP(CondV, Type::getDoubleTy(*TheContext), "tmp");
  CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");
  Builder->CreateCondBr(CondV, LoopBB, AfterBB);

  Builder->SetInsertPoint(LoopBB);

  BlockReturned.push(false);

  Loop->codegen();

  if (!BlockReturned.top()) {

  Simp->codegen();

  CondV = Cond->codegen();
  if (!CondV)
    return nullptr;
  if (CondV->getType()->isIntegerTy())
    CondV = Builder->CreateUIToFP(CondV, Type::getDoubleTy(*TheContext), "tmp");
  CondV = Builder->CreateFCmpONE(CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");
  Builder->CreateCondBr(CondV, LoopBB, AfterBB);

  }
  BlockReturned.pop();

  LeaveBlock(&scopeNames, &scopeValues, &scopeStructValues);

  Builder->SetInsertPoint(AfterBB);

  return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}


Value *RetStmtAST::codegen() {
  Function *TheFunction = Builder->GetInsertBlock()->getParent();
  Type *T = TheFunction->getReturnType();
  Value *RetVal = Ret->codegen();
  TypeCast(T, RetVal);

  Builder->CreateRet(RetVal);
  
  if (BlockValueNames.empty()) {
    HasReturned = true;
  } else {
    BlockReturned.pop();
    BlockReturned.push(true);
  }

  return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}


Function *FunctionAST::codegen() {
  // Transfer ownership of the prototype to the FunctionProtos map, but keep a
  // reference to it for use below.
  auto &P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  Function *TheFunction = getFunction(P.getName());
  if (!TheFunction)
    return nullptr;

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  // Reset NamedValues & HasReturned
  NamedValues.clear();
  StructValues.clear();
  HasReturned = false;

  // Allocate for arguments
  for (auto &Arg : TheFunction->args()) {
    AllocaInst *Alloca;
    Type *T = Arg.getType();
    if (T->isPointerTy() && T->getPointerElementType()->isStructTy()) {
      Alloca = CreateEntryBlockAlloca(TheFunction, std::string(Arg.getName()), T->getPointerElementType());
      Value *V = Builder->CreateLoad(&Arg);
      Builder->CreateStore(V, Alloca);
      StructValues[std::string(Arg.getName())] = T->getPointerElementType()->getStructName().str();
    } else {
      Alloca = CreateEntryBlockAlloca(TheFunction, std::string(Arg.getName()), T);
      Builder->CreateStore(&Arg, Alloca);
    }
    NamedValues[std::string(Arg.getName())] = Alloca;
  }

  if (Value *RetVal = Body->codegen()) {
    if (!HasReturned) {
      if (P.getReturnType() == type_double && RetVal->getType()->isIntegerTy())
        RetVal = Builder->CreateUIToFP(RetVal, Type::getDoubleTy(*TheContext), "tmp");
      if (P.getReturnType() == type_int && RetVal->getType()->isDoubleTy())
        RetVal = Builder->CreateFPToUI(RetVal, Type::getInt32Ty(*TheContext), "tmp");
      if (P.getReturnType() == type_char && RetVal->getType()->isDoubleTy())
        RetVal = Builder->CreateFPToUI(RetVal, Type::getInt32Ty(*TheContext), "tmp");
      Builder->CreateRet(RetVal);
    }
    // Validate the generated code, checking for consistency.
    if (verifyFunction(*TheFunction)) {
      // Error reading body, remove function
      std::cout << "Error reading body, remove function." << std::endl;
      TheFunction->eraseFromParent();
      return nullptr;
    }
    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level
//===----------------------------------------------------------------------===//
//don't modify this part

static void InitializeModuleAndPassManager() {
  // Open a new context and module.
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("my cool jit", *TheContext);

  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:");
      FnIR->print(errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleStructDef() {
  if (auto SAST = ParseStruct()) {
    fprintf(stderr, "Read struct definition for %s\n", SAST->getName().c_str());
    SAST->codegen();
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleBinopDef() {
  if (auto BinopAST = ParseBinopDef()) {
    if (auto *FnIR = BinopAST->codegen()) {
      fprintf(stderr, "Read binary operation definition:");
      FnIR->print(errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleUnopDef() {
  if (auto UnopAST = ParseUnopDef()) {
    if (auto *FnIR = UnopAST->codegen()) {
      fprintf(stderr, "Read unary operation definition:");
      FnIR->print(errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern: ");
      FnIR->print(errs());
      fprintf(stderr, "\n");
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}


/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    switch (CurTok) {
    case tok_eof:
      return;
    case tok_def:
      if (ValType == type_struct)
        HandleStructDef();
      else
        HandleDefinition();
      break;
    case tok_binary_def:
      HandleBinopDef();
      break;
    case tok_unary_def:
      HandleUnopDef();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      std::cout << "invalid input" << std::endl;
      getNextToken();
      break;
    }
  }
}
//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//
//don't modify this part

int main(int argc, char* argv[]) {  
  if(argc < 2){
    errs() << "You need to specify the file to compile";
    return 1;
  }
  char* FileName = argv[1];
  fip = fopen(FileName, "r");
  if(fip == nullptr){
    errs() << "The file '" << FileName << "' is not existed";
    return 1;
  }

  InitializeTypeValue();
  InitializeOpValue();

  BinopPrecedence[op_eq] = 10;
  BinopPrecedence[op_ne] = 10;
  BinopPrecedence[op_le] = 10;
  BinopPrecedence[op_lt] = 10;
  BinopPrecedence[op_add] = 20;
  BinopPrecedence[op_sub] = 20;
  BinopPrecedence[op_mul] = 40; // highest.
  getNextToken();

  InitializeModuleAndPassManager();
  MainLoop();

  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

  auto TargetTriple = sys::getDefaultTargetTriple();
  TheModule->setTargetTriple(TargetTriple);
  
  std::string Error;
  auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    errs() << Error;
    return 1;
  }

  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  auto RM = Optional<Reloc::Model>();
  auto TheTargetMachine =
      Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);
  TheModule->setDataLayout(TheTargetMachine->createDataLayout());

  auto Filename = "output.o";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }

  legacy::PassManager pass;
  auto FileType = CGFT_ObjectFile;

  if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    errs() << "TheTargetMachine can't emit a file of this type";
    return 1;
  }

  pass.run(*TheModule);
  dest.flush();

  outs() << "Wrote " << Filename << "\n";

  return 0;
}