type scalar_type = Int | Char

type array_type =
  | Fixed of scalar_type * int list
  | Flexible of scalar_type * int list

type un_arit_op = Pos | Neg
type bin_arit_op = Add | Sub | Mul | Div | Mod
type un_logic_op = Not
type bin_logic_op = And | Or
type rel_op = Eql | NEql | Greater | Lesser | GreaterEql | LesserEql
type var_type = ScalarVar of scalar_type | AggregateVar of array_type
type param_type = ScalarParam of scalar_type | AggregateParam of array_type
type ret_type = ScalarRet of scalar_type | Nothing

type l_value = Ident of string | Str of string | ArrAccess of l_value * expr
and func_call = string * expr list

and expr =
  | IntLit of int
  | CharLit of char
  | LValue of l_value
  | ExprFuncCall of func_call
  | UnAritOp of un_arit_op * expr
  | BinAritOp of bin_arit_op * expr * expr

type cond =
  | UnLogicOp of un_logic_op * cond
  | BinLogicOp of bin_logic_op * cond * cond
  | RelOp of rel_op * expr * expr

type block = stmt list

and stmt =
  | EmptyStmt
  | Assignment of l_value * expr
  | Block of block
  | StmtFuncCall of func_call
  | IfThen of cond * stmt
  | IfThenElse of cond * stmt * stmt
  | WhileDo of cond * stmt
  | RetExpr of expr option

type eval_strat = Val | Ref
type var_def = string * var_type
type param_def = string * eval_strat * param_type
type header = string * param_def list * ret_type
type func_decl = header

type func_def = header * local_def list * block

and local_def =
  | FuncDecl of func_decl
  | FuncDef of func_def
  | VarDef of var_def

type program = MainFunc of func_def
