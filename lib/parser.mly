%{
  (* header *)
  open Syntax

  (*exception Parsing_error of string*)
%}

(* types of nonterminal symbols *)
%start <Syntax.program>        program
%type  <Syntax.scalar_type>    scalar_type
%type  <Syntax.array_type>     array_type
%type  <Syntax.un_arit_op>     un_arit_op
%type  <Syntax.bin_arit_op>    bin_arit_op
%type  <Syntax.un_logic_op>    un_logic_op
%type  <Syntax.bin_logic_op>   bin_logic_op
%type  <Syntax.rel_op>         rel_op
%type  <Syntax.var_type>       var_type
%type  <Syntax.param_type>     param_type
%type  <Syntax.ret_type>       ret_type
%type  <Syntax.l_value>        l_value
%type  <Syntax.func_call>      func_call
%type  <Syntax.expr>           expr
%type  <Syntax.cond>           cond
%type  <Syntax.block>          block
%type  <Syntax.stmt>           stmt
%type  <Syntax.eval_strat>     eval_strat
%type  <Syntax.var_def list>   var_def
%type  <Syntax.param_def list> param_def
%type  <Syntax.header>         header
%type  <Syntax.func_decl>      func_decl
%type  <Syntax.func_def>       func_def
%type  <Syntax.local_def list> local_def 

%%

(* rules *)

let scalar_type :=
| "int";  { Int }
| "char"; { Char }

let array_type :=
| ~ = scalar_type; ~ = delimited ("[", INT_LIT, "]") +; < Fixed >
| ~ = scalar_type; "["; "]"; ~ = delimited ("[", INT_LIT, "]") *; < Flexible >

let un_arit_op ==
| "+"; { Pos }
| "-"; { Neg }

let bin_arit_op ==
| "+"; { Add }
| "-"; { Sub }
| "*"; { Mul }
| "div"; { Div }
| "mod"; { Mod }

let un_logic_op ==
| "not"; { Not }

let bin_logic_op ==
| "and"; { And }
| "or"; { Or }

let rel_op ==
| "="; { Eql }
| "#"; { NEql }
| ">"; { Greater }
| "<"; { Lesser }
| ">="; { GreaterEql }
| "<="; { LesserEql }

let var_type :=
| ~ = scalar_type; < ScalarVar >
| ~ = array_type; < AggregateVar >

let param_type :=
| ~ = scalar_type; < ScalarParam >
| ~ = array_type; < AggregateParam >

let ret_type :=
| ~ = scalar_type; < ScalarRet >
| "nothing"; { Nothing }

let l_value :=
| ~ = ID; < Ident >
| ~ = STR_LIT; < Str >
| ~ = l_value; "["; ~ = expr; "]"; < ArrAccess >

let func_call :=
| ~ = ID; "("; ~ = separated_nonempty_list (",", expr); ")"; < >

let expr :=
| "("; ~ = expr; ")"; < >
| ~ = INT_LIT; < IntLit >
| ~ = CHAR_LIT; < CharLit >
| ~ = l_value; < LValue >
| ~ = func_call; < ExprFuncCall >
| ~ = un_arit_op; ~ = expr; < UnAritOp >
| lhs = expr; ~ = bin_arit_op; rhs = expr; { BinAritOp (bin_arit_op, lhs, rhs) }

let cond :=
| "("; ~ = cond; ")"; < >
| ~ = un_logic_op; ~ = cond; < UnLogicOp >
| lhs = cond; ~ = bin_logic_op; rhs = cond; { BinLogicOp (bin_logic_op, lhs, rhs) }
| lhs = expr; ~ = rel_op; rhs = expr; { RelOp (rel_op, lhs, rhs) }

let block :=
| "{"; ~ = stmt *; "}"; < >

let stmt :=
| ";"; { EmptyStmt }
| ~ = l_value; "<-"; ~ = expr; ";"; < Assignment >
| ~ = block; < Block >
| ~ = func_call; ";"; < StmtFuncCall >
| "if"; ~ = cond; "then"; ~ = stmt; < IfThen >
| "if"; ~ = cond; "then"; s1 = stmt; "else"; s2 = stmt; < IfThenElse >
| "while"; ~ = cond; "do"; ~ = stmt; < WhileDo >
| "return"; ~ = expr ?; ";"; < RetExpr >

let eval_strat :=
| { Val }
| "ref"; { Ref }

let var_def :=
| "var"; ids = separated_nonempty_list (",", ID); ":"; ~ = var_type; ";";
  { List.map (fun id -> (id, var_type)) ids }

let param_def :=
| ~ = eval_strat; ids = separated_nonempty_list (",", ID); ":"; ~ = param_type;
  { List.map (fun id -> (id, eval_strat, param_type)) ids }

let header :=
| "fun"; ~ = ID; "("; ~ = flatten (separated_list (";", param_def)); ")"; ":"; ~ = ret_type; < >

let func_decl :=
| ~ = header; ";"; < >

let func_def :=
| ~ = header; ~ = flatten (local_def *); ~ = block; < >

let local_def :=
| ~ = func_decl; { [FuncDecl func_decl] }
| ~ = func_def; { [FuncDef func_def] }
| ~ = var_def; { List.map (fun vd -> VarDef vd) var_def }

let program :=
  ~ = func_def; "eof";
  { MainFunc (func_def) }

%%

(* trailer *)
