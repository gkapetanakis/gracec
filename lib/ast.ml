type bin_arit_op = Add | Subtract | Multiply | Divide | Modulo
type un_arit_op = Positive | Negative

type comp_op =
  | Equal
  | NotEqual
  | Lesser
  | LesserEqual
  | Greater
  | GreaterEqual

type logic_op = And | Or
type data_type = Int | Char
type var_type = VarDataType of data_type | Array of data_type * int list
type ret_type = RetDataType of data_type | Nothing
type fpar_type = Array of data_type * bool * int list
type fpar_def = { ref : bool; ids : string list; fpar_type : fpar_type }
type var_def = { ids : string list; var_type : var_type }

type l_value =
  | Identifier of string
  | String of string
  | ArrayIndex of l_value * expr

and expr =
  | IntLiteral of int
  | CharLiteral of char
  | LValue of l_value
  | ExpressionFunctionCall of func_call
  | UnaryArithmeticOperator of un_arit_op * expr
  | BinaryArithmeticOperator of expr * bin_arit_op * expr

and func_call = { id : string; params : expr list }

type cond =
  | NotOperator of cond
  | BinaryLogicalOperator of cond * logic_op * cond
  | ComparisonOperator of expr * comp_op * expr

type stmt =
  | EmptyStatement
  | Assignment of l_value * expr
  | Block of block
  | StatementFunctionCall of func_call
  | IfStatement of cond * stmt * stmt option
  | WhileLoop of cond * stmt
  | ReturnExpression of expr option

and block = stmt list

type header = { id : string; fpar_defs : fpar_def list; ret_type : ret_type }
type func_decl = header

type func_def = { header : header; local_defs : local_def list; block : block }

and local_def =
  | FunctionDefinition of func_def
  | FunctionDeclaration of func_decl
  | VariableDefinition of var_def

type program = MainFunction of func_def
