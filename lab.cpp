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
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number_int = -5,
  tok_number_double = -6,
  tok_char = -7,

  // function
  tok_return = -8,
  tok_if = -9,
  tok_else = -10,
  tok_while = -11,
  tok_for = -12,

  // user-defined
  tok_unary_def = -13,
  tok_binary_def = -14,
};

enum Types {
  type_int = 1,
  type_double = 2,
  type_char = 3,
};


static std::map<std::string, int> TypeValues; // Map typeString to int
static FILE *fip;
static std::string IdentifierStr;             // Filled in if tok_identifier
static int NumValI;                           // Filled in if tok_number_int
static double NumValD;                        // Filled in if tok_number_double
static int ValType;                           // Filled in if tok_def
static int CharVal;                           // Filled in if tok_char

static void InitializeTypeValue(){
  TypeValues["int"] = 1;
  TypeValues["double"] = 2;
  TypeValues["char"] = 3;
}

/// gettok - Return the next token from standard input.
static int gettok() {
  static int LastChar = ' ';
  std::map<std::string, int>::iterator iter;

  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = fgetc(fip);


  if (isalpha(LastChar)) {
    // Get next word.
    std::string word = "";
    word.push_back(LastChar);
    // Read any alpha and digit.
    LastChar = fgetc(fip);
    while (isalpha(LastChar) || isdigit(LastChar)) {
      word.push_back(LastChar);
      LastChar = fgetc(fip);
    }

    // int & double
    if (word == "int" || word == "double" || word == "char") {
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


  if (isdigit(LastChar)) {
    int integer = 0;
    double decimal = 0;
    int tok_type = tok_number_int;

    // Get next number.
    while (isdigit(LastChar) || LastChar == '.') {
      if (LastChar == '.') {
        if (tok_type == tok_number_int) 
          tok_type = tok_number_double;
        else if (tok_type == tok_number_double)
          // invalid input
          return 0;
      } else {
        if (tok_type == tok_number_int)
          integer = integer * 10 + (LastChar - '0');
        else if (tok_type == tok_number_double)
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

  if (LastChar == '#') {
    // Comment util end of line.
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
// you don't have to modify this part. (of course it is ok to add something if needed)
namespace {

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

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}

  Value *codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
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
  std::string FnName;
  int DeclType;
  std::vector<std::string> Decls;

public:
  DeclStmtAST(std::string FnName, int DeclType, std::vector<std::string> Decls)
    : FnName(FnName), DeclType(DeclType), Decls(Decls) {}

  Value *codegen();
};

/// SimpStmtAST - Statement class for simple statement "=".
class SimpStmtAST : public StmtAST {
  std::string Ident;
  std::unique_ptr<ExprAST> RHS;

public:
  SimpStmtAST(std::string Ident, std::unique_ptr<ExprAST> RHS)
    : Ident(Ident), RHS(std::move(RHS)) {}

  Value *codegen();  
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
  int FnType;

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args, std::vector<int> ArgTypes, int FnType)
      : Name(Name), Args(std::move(Args)), ArgTypes(std::move(ArgTypes)), FnType(FnType) {}

  Function *codegen();
  const std::string &getName() const { return Name; }
  const int getReturnType() {return FnType;}
  const std::vector<int> &getArgTypes() {return ArgTypes;}
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
static std::map<char, int> BinopPrecedence;


/// LogError* - These are little helper functions for error handling.
/// you can add additional function to help you log error. 
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

std::unique_ptr<BodyAST> LogErrorB(const char *Str) {
  LogError(Str);
  return nullptr;
}

std::unique_ptr<StmtAST> LogErrorS(const char *Str) {
  LogError(Str);
  return nullptr;
}


static std::unique_ptr<ExprAST> ParseExpression(int precedence);


/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr(int NumberType) {
  if (NumberType == type_double){
    auto Result = std::make_unique<NumberDoubleExprAST>(NumValD);
    getNextToken(); // consume the number
    return Result;
  } else {
    auto Result = std::make_unique<NumberIntExprAST>(NumValI);
    getNextToken(); // consume the number
    return Result;
  }
}


/// charexpr ::= char
static std::unique_ptr<ExprAST> ParseCharExpr() {
  auto Result = std::make_unique<CharExprAST>(CharVal);
  getNextToken();
  return Result;
}


/// identifierexpr
/// <ident> or <callee>
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string ident = IdentifierStr;
  getNextToken(); // eat identifier

  if (CurTok != '(')
    return std::make_unique<VariableExprAST>(ident);
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


/// expression
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
    return LogError("Invalid expression input");
    break;
  }

  while (CurTok == '+' || CurTok == '-' || CurTok == '*' || CurTok == '<') {
    char op = CurTok;
    int rp = BinopPrecedence[op];
    int lp = rp - 1;
    if (lp < precedence)
      break;
    getNextToken();
    auto Right = ParseExpression(rp);
    Result = std::make_unique<BinaryExprAST>(op, std::move(Result), std::move(Right));
  }

  return Result;
}

/// declaration statement
/// <decl>
static std::unique_ptr<StmtAST> ParseDeclStatement(std::string FnName) {
  int DeclType = ValType;
  getNextToken(); // eat <type>

  if (CurTok != tok_identifier)
    return LogErrorS("Expected variable name in declaration");
  std::vector<std::string> DeclNames;
  DeclNames.push_back(IdentifierStr);
  getNextToken(); // eat <ident>

  while (CurTok == ',') {
    getNextToken(); // eat ','
    if (CurTok != tok_identifier)
      return LogErrorS("Expected valid variable name in declaration");
    DeclNames.push_back(IdentifierStr);
    getNextToken(); // eat <ident>
  }

  if (CurTok != ';') {
    return LogErrorS("Expected ';' after a statement");
  }
  getNextToken(); // eat ';'

  return std::make_unique<DeclStmtAST>(FnName, DeclType, DeclNames);
}


/// simple statement
/// <simp>
static std::unique_ptr<StmtAST> ParseSimpStatement() {
  std::string ident = IdentifierStr;
  getNextToken(); // eat <ident>

  if (CurTok != '=') {
    return LogErrorS("Expected '=' in simple statement");
  }
  getNextToken(); // eat '='

  auto E = ParseExpression(0);

  if (CurTok != ';') {
    return LogErrorS("Expected ';' after a statement");
  }
  getNextToken(); // eat ';'

  return std::make_unique<SimpStmtAST>(ident, std::move(E));
}


/// return statement
/// <return>
static std::unique_ptr<StmtAST> ParseRetStatement() {
  getNextToken(); // eat "return"

  auto E = ParseExpression(0);

  if (CurTok != ';') {
    return LogErrorS("Expected ';' after a statement");
  }
  getNextToken(); // eat ';'

  return std::make_unique<RetStmtAST>(std::move(E));
}


/// statement 
/// <stmt>
static std::unique_ptr<StmtAST> ParseStatement(std::string FnName) {
  switch (CurTok)
  {
  case tok_def:
    return ParseDeclStatement(FnName);
  case tok_identifier:
    return ParseSimpStatement();
  case tok_return:
    return ParseRetStatement();
  default:
    return LogErrorS("Invalid statement input");
  }
}


/// prototype
/// <prototype>
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  int FnType = ValType;
  getNextToken(); // eat ValType
  if (CurTok != tok_identifier)
    return LogErrorP("Expected function name in prototype");
  
  std::string FnName = IdentifierStr;
  getNextToken(); // eat FnName

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");
  getNextToken(); // eat '('

  std::vector<std::string> ArgNames;
  std::vector<int> ArgTypes;
  if (CurTok == tok_def) {
    ArgTypes.push_back(ValType);
    getNextToken(); // eat <type>
    if (CurTok != tok_identifier)
      return LogErrorP("Expected valid arg name in paramlist");
    ArgNames.push_back(IdentifierStr);
    getNextToken(); // eat <ident>
    while (CurTok == ',') {
      getNextToken(); // eat ','
      if (CurTok != tok_def)
        return LogErrorP("Expected arg type in paramlist");
      ArgTypes.push_back(ValType);
      getNextToken(); // eat <type>
      if (CurTok != tok_identifier)
        return LogErrorP("Expected valid arg name in paramlist");
      ArgNames.push_back(IdentifierStr);
      getNextToken(); // eat <ident>
    }
  }
  
  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");

  // Success.
  getNextToken(); // eat ')'

  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), std::move(ArgTypes), FnType);
}


/// body
/// <body>
static std::unique_ptr<BodyAST> ParseBody(std::string FnName) {
  std::vector<std::unique_ptr<StmtAST>> Stmts;
  while (CurTok != tok_return)
  {
    auto S = ParseStatement(FnName);
    Stmts.push_back(std::move(S));
  }
  auto RetStmt = ParseRetStatement();
  Stmts.push_back(std::move(RetStmt));
  return std::make_unique<BodyAST>(std::move(Stmts));
}


/// definition ::= 'def' prototype expression
/// <function>
static std::unique_ptr<FunctionAST> ParseDefinition() {
  auto Proto = ParsePrototype();
  if (CurTok != '{')
    return LogErrorF("Expected '{' in function body");
  getNextToken(); // eat '{'
  auto Body = ParseBody(Proto->getName());
  if (CurTok != '}')
    return LogErrorF("Expected '}' in function body");
  getNextToken(); // eat '}'
  return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
}


/// external ::= 'extern' prototype
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
static std::map<std::string, AllocaInst*> NamedValues;
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


Value *NumberDoubleExprAST::codegen() {
  return ConstantFP::get(*TheContext, APFloat(Val));
}


Value *NumberIntExprAST::codegen() {
  return ConstantInt::get(*TheContext, APInt(32,Val));
}


Value *CharExprAST::codegen() {
  return ConstantInt::get(*TheContext, APInt(32,Val));
}


Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  Value *V = NamedValues[Name];
  if (!V)
    return LogErrorV("Unknown variable name");
  // Load the value.  
  return Builder->CreateLoad(V, Name.c_str());
}


Value *BinaryExprAST::codegen() {
  Value *L = LHS->codegen();
  if (!L)
    return nullptr;
  Value *R = RHS->codegen();
  if (!R)
    return nullptr;

  // Do value type conversion before binary operation.
  bool isIntOp;
  if (L->getType()->isDoubleTy()) {
    if (R->getType()->isIntegerTy()) 
      R = Builder->CreateUIToFP(R, Type::getDoubleTy(*TheContext), "tmp");
    else if (!R->getType()->isDoubleTy())
      return LogErrorV("Invalid operation value type");
    isIntOp = false;
  } else if (L->getType()->isIntegerTy()) {
    if (R->getType()->isDoubleTy()) {
      L = Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "tmp");
      isIntOp = false;
    } else if (R->getType()->isIntegerTy())
      isIntOp = true;
    else
      return LogErrorV("Invalid operation value type");
  } else {
    return LogErrorV("Invalid operation value type");
  }

  switch (Op) {
    case '+':
      if (isIntOp) 
        return Builder->CreateAdd(L, R, "addtmp");
      else
        return Builder->CreateFAdd(L, R, "addtmp");
    case '-':
      if (isIntOp)
        return Builder->CreateSub(L, R, "subtmp");
      else
        return Builder->CreateFSub(L, R, "subtmp");
    case '*':
      if (isIntOp)
        return Builder->CreateMul(L, R, "multmp");
      else
        return Builder->CreateFMul(L, R, "multmp");
    case '<':
      // Lack of bool type (i1), so use "double" type as cmp return type...
      if (isIntOp)
        return Builder->CreateUIToFP(Builder->CreateICmpULT(L, R, "cmptmp"), Type::getDoubleTy(*TheContext), "booltmp");
      else
        return Builder->CreateUIToFP(Builder->CreateFCmpULT(L, R, "cmptmp"), Type::getDoubleTy(*TheContext), "booltmp");
    default:
      return LogErrorV("Unknown binary opreation");
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
    ArgValues.push_back(Args[i]->codegen());
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
  }
  return Result;
}


Value *DeclStmtAST::codegen() {
  Function *TheFunction = getFunction(FnName);

  llvm::Type *ty;
  if (DeclType == type_int)
    ty = Type::getInt32Ty(*TheContext);
  else if (DeclType == type_double)
    ty = Type::getDoubleTy(*TheContext);
  else if (DeclType == type_char)
    ty = Type::getInt32Ty(*TheContext);

  for (auto &Decl : Decls) {
    // Create an alloca for this variable.
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Decl, ty);
    // Add arguments to variable symbol table.
    NamedValues[Decl] = Alloca;
  }

  return nullptr;
}


Value *SimpStmtAST::codegen() {
  Value *R = RHS->codegen();
  if (!R)
    return nullptr;

  // Look up the name.
  AllocaInst *Variable = NamedValues[Ident];
  if (!Variable)
    return LogErrorV("Unknown variable name");
  
  // Type casting.
  if (Variable->getAllocatedType()->isIntegerTy() && R->getType()->isDoubleTy())
    R = Builder->CreateFPToUI(R, Type::getInt32Ty(*TheContext), "tmp");
  if (Variable->getAllocatedType()->isDoubleTy() && R->getType()->isIntegerTy())
    R = Builder->CreateUIToFP(R, Type::getDoubleTy(*TheContext), "tmp");

  Builder->CreateStore(R, Variable);
  return nullptr;
}


Value *RetStmtAST::codegen() {
  return Ret->codegen();
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

  NamedValues.clear();
  for (auto &Arg : TheFunction->args()) {
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, std::string(Arg.getName()), Arg.getType());
    Builder->CreateStore(&Arg, Alloca);
    NamedValues[std::string(Arg.getName())] = Alloca;
  }

  if (Value *RetVal = Body->codegen()) {
    //****************
    // Correctly create the RetVal use Builder.
    //****************
    if (P.getReturnType() == type_double && RetVal->getType()->isIntegerTy())
      RetVal = Builder->CreateUIToFP(RetVal, Type::getDoubleTy(*TheContext), "tmp");
    if (P.getReturnType() == type_int && RetVal->getType()->isDoubleTy())
      RetVal = Builder->CreateFPToUI(RetVal, Type::getInt32Ty(*TheContext), "tmp");
    if (P.getReturnType() == type_char && RetVal->getType()->isDoubleTy())
      RetVal = Builder->CreateFPToUI(RetVal, Type::getInt32Ty(*TheContext), "tmp");
    Builder->CreateRet(RetVal);
    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);
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
      HandleDefinition();
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

  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.
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