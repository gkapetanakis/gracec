%{
  (* header *)
  open Ast
%}

%start <program> program
%type <func_def> func_def
%type <fpar_def> fpar_def
%type <data_type> data_type
%type <var_type> var_type
%type <ret_type> ret_type
%type <fpar_type> fpar_type
%type <local_def> local_def
%type <func_decl> func_decl
%type <var_def> var_def
%type <stmt> stmt
%type <block> block
%type <func_call> func_call
%type <l_value> l_value
%type <expr> expr
%type <cond> cond

%%

/* rules */
let program :=
  | ~ = func_def; "eof"; { MainFunction func_def }

let func_def :=
  | ~ = header; local_defs = list (local_def); ~ = block; { { header; local_defs; block } }

let header :=
  | "fun"; id = ID; "("; fpar_defs = separated_nonempty_list (";", fpar_def); ")"; ":"; ~ = ret_type; { { id; fpar_defs; ret_type } }

let fpar_def :=
  | ref = option("ref"); ids = separated_nonempty_list (",", ID); ":"; ~ = fpar_type; { { ref = Option.is_some ref; ids; fpar_type } }

let data_type :=
  | "int"; { Int }
  | "char"; { Char }

let var_type :=
  | ~ = data_type; dims = list (delimited ("[", INT_LIT, "]")); { if List.length dims = 0 
                                                                    then VarDType data_type 
                                                                    else Array (data_type, dims) }

let ret_type :=
  | ~ = data_type; { RetDType data_type }
  | "nothing"; { Nothing }

let fpar_type :=
  | ~ = data_type; first_dim = option (pair ("[", "]")); rest_dims = list (delimited ("[", INT_LIT, "]")); { if Option.is_none first_dim || List.length rest_dims = 0
                                                                                                               then FParDType data_type
                                                                                                               else Array (data_type, Option.is_some first_dim, rest_dims) }

let local_def :=
  | ~ = func_def; { FunctionDefinition func_def }
  | ~ = func_decl; { FunctionDeclaration func_decl }
  | ~ = var_def; { VariableDefinition var_def }

let func_decl :=
  | ~ = header; ";"; { header }

let var_def :=
  | "var"; ids = separated_nonempty_list (",", ID); ":"; ~ = var_type; ";"; { { ids; var_type } }

let stmt :=
  | ";"; { EmptyStatement } (* empty statement *)
  | ~ = l_value; "<-"; ~ = expr; ";"; { Assignment (l_value, expr) } (* assignment *)
  | ~ = block; { Block block }
  | ~ = func_call; ";"; { StatementFunctionCall func_call }
  | "if"; ~ = cond; "then";  ~ = stmt; { IfStatement (cond, stmt, None) }
  | "if"; ~ = cond; "then";  then_stmt = stmt; "else"; else_stmt = stmt; { IfStatement (cond, then_stmt, Some else_stmt) }
  | "while"; ~ = cond; "do"; ~ = stmt; { WhileLoop (cond, stmt) }
  | "return"; expr_opt = option (expr); ";"; { ReturnExpression expr_opt }

let block :=
  | "{"; stmts = list (stmt); "}"; { stmts }

let func_call :=
  | id = ID; "("; params = separated_nonempty_list (",", expr); ")"; { { id; params } }

let l_value :=
  | id = ID; { Ident id }
  | string_lit = STRING_LIT; { Str string_lit }
  | ~ = l_value; "["; ~ = expr; "]"; { ArrayIdx (l_value, expr) }

let expr :=
  | int_lit = INT_LIT; { IntLit int_lit }
  | char_lit = CHAR_LIT; { CharLit char_lit }
  | ~ = l_value; { LValue l_value }
  | "("; ~ = expr; ")"; { expr }
  | ~ = func_call; { ExpressionFunctionCall func_call }
  | "+"; ~ = expr; { UnaryArithmeticOp (Pos, expr) }
  | "-"; ~ = expr; { UnaryArithmeticOp (Neg, expr) }
  | lhs = expr; "+"; rhs = expr; { BinaryArithmeticOp (lhs, Add, rhs) }
  | lhs = expr; "-"; rhs = expr; { BinaryArithmeticOp (lhs, Sub, rhs) }
  | lhs = expr; "*"; rhs = expr; { BinaryArithmeticOp (lhs, Mul, rhs) }
  | lhs = expr; "div"; rhs = expr; { BinaryArithmeticOp (lhs, Div, rhs) }
  | lhs = expr; "mod"; rhs = expr; { BinaryArithmeticOp (lhs, Mod, rhs) }

let cond :=
  | "("; ~ = cond; ")"; { cond }
  | "not"; ~ = cond; { NotOp cond }
  | lhs = cond; "and"; rhs = cond; { BinaryLogicalOp (lhs, And, rhs) }
  | lhs = cond; "or"; rhs = cond; { BinaryLogicalOp (lhs, Or, rhs) }
  | lhs = expr; "="; rhs = expr; { ComparisonOp (lhs, Eq, rhs) }
  | lhs = expr; "#"; rhs = expr; { ComparisonOp (lhs, NEq, rhs) }
  | lhs = expr; ">"; rhs = expr; { ComparisonOp (lhs, Great, rhs) }
  | lhs = expr; "<"; rhs = expr; { ComparisonOp (lhs, Less, rhs) }
  | lhs = expr; ">="; rhs = expr; { ComparisonOp (lhs, GreatEq, rhs) }
  | lhs = expr; "<="; rhs = expr; { ComparisonOp (lhs, LessEq, rhs) }

%%

(* trailer *)
