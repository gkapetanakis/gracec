(* types *)
type data_type = Int | Char
type var_type = VarDType of data_type | VarArray of data_type * int list
type ret_type = RetDType of data_type | Nothing

type fpar_type =
  | FParDType of data_type
  | FParArray of data_type * bool * int list

(* operators *)
type bin_arit_op = Add | Sub | Mul | Div | Mod
type un_arit_op = Pos | Neg
type comp_op = Eq | NEq | Great | Less | GreatEq | LessEq
type logic_op = And | Or

(* definitions and declarations *)
type var_def = { ids : string list; var_type : var_type }
type fpar_def = { ref : bool; ids : string list; fpar_type : fpar_type }
type func_decl = { id : string; fpar_defs : fpar_def list; ret_type : ret_type }

(* l-values and expressions *)
type l_value = Ident of string | Str of string | ArrayIdx of l_value * expr

and expr =
  | IntLit of int
  | CharLit of char
  | LValue of l_value
  | ExpressionFunctionCall of func_call
  | UnaryArithmeticOp of un_arit_op * expr
  | BinaryArithmeticOp of expr * bin_arit_op * expr

and func_call = { id : string; params : expr list }

(* conditionals *)
type cond =
  | NotOp of cond
  | BinaryLogicalOp of cond * logic_op * cond
  | ComparisonOp of expr * comp_op * expr

(* statements and blocks *)
type stmt =
  | EmptyStatement
  | Assignment of l_value * expr
  | Block of block
  | StatementFunctionCall of func_call
  | IfStatement of cond * stmt * stmt option
  | WhileLoop of cond * stmt
  | ReturnExpression of expr option

and block = stmt list

type header = func_decl

type func_def = { header : header; local_defs : local_def list; block : block }

and local_def =
  | FunctionDeclaration of func_decl
  | FunctionDefinition of func_def
  | VariableDefinition of var_def

type program = MainFunction of func_def

(* constructors for record types *)
let fpar_def (ref, ids, fpar_type) = { ref; ids; fpar_type }
let var_def (ids, var_type) = { ids; var_type }
let func_call (id, params) = { id; params }
let func_decl (id, fpar_defs, ret_type) = { id; fpar_defs; ret_type }
let header = func_decl
let func_def (header, local_defs, block) = { header; local_defs; block }
