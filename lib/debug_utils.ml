open Syntax

let print_with_offset ofs str = Printf.printf "%s%s" (String.make ofs ' ') str

let print_scalar_type _tab ofs = function
  | Int -> print_with_offset ofs "int"
  | Char -> print_with_offset ofs "char"

let rec print_array_type tab ofs = function
  | Fixed (scalar_type, [ h ]) ->
      print_with_offset ofs "Array:\n";
      print_with_offset (ofs + tab) (Printf.sprintf "Size: %d\n" h);
      print_with_offset (ofs + tab) "Of: ";
      print_scalar_type tab 0 scalar_type
  | Fixed (scalar_type, h :: tl) ->
      print_with_offset ofs "Array:\n";
      print_with_offset (ofs + tab) (Printf.sprintf "Size: %d\n" h);
      print_with_offset (ofs + tab) "Of:\n";
      print_array_type tab (ofs + tab + tab) (Fixed (scalar_type, tl))
  | Fixed (_, []) -> assert false
  | Flexible (scalar_type, []) ->
      print_with_offset ofs "Array:\n";
      print_with_offset (ofs + tab) "Size: flexible\n";
      print_with_offset (ofs + tab) "Of: ";
      print_scalar_type tab 0 scalar_type
  | Flexible (scalar_type, int_list) ->
      print_with_offset ofs "Array:\n";
      print_with_offset (ofs + tab) "Size: flexible\n";
      print_with_offset (ofs + tab) "Of:\n";
      print_array_type tab (ofs + tab + tab) (Fixed (scalar_type, int_list))

let print_un_arit_op _tab ofs = function
  | Pos -> print_with_offset ofs "+"
  | Neg -> print_with_offset ofs "-"

let print_bin_arit_op _tab ofs = function
  | Add -> print_with_offset ofs "+"
  | Sub -> print_with_offset ofs "-"
  | Mul -> print_with_offset ofs "*"
  | Div -> print_with_offset ofs "div"
  | Mod -> print_with_offset ofs "mod"

let print_un_logic_op _tab ofs = function Not -> print_with_offset ofs "not"

let print_bin_logic_op _tab ofs = function
  | And -> print_with_offset ofs "and"
  | Or -> print_with_offset ofs "or"

let print_rel_op _tab ofs = function
  | Eql -> print_with_offset ofs "="
  | NEql -> print_with_offset ofs "#"
  | Greater -> print_with_offset ofs ">"
  | Lesser -> print_with_offset ofs "<"
  | GreaterEql -> print_with_offset ofs ">="
  | LesserEql -> print_with_offset ofs "<="

let print_var_type tab ofs = function
  | ScalarVar scalar_type -> print_scalar_type tab ofs scalar_type
  | AggregateVar array_type -> print_array_type tab ofs array_type

let print_param_type tab ofs = function
  | ScalarParam scalar_type -> print_scalar_type tab ofs scalar_type
  | AggregateParam array_type -> print_array_type tab ofs array_type

let print_ret_type tab ofs = function
  | ScalarRet scalar_type -> print_scalar_type tab ofs scalar_type
  | Nothing -> print_with_offset ofs "nothing"

let rec print_l_value tab ofs = function
  | Ident id -> print_with_offset ofs (Printf.sprintf "Identifier: %s" id)
  | Str str -> print_with_offset ofs (Printf.sprintf "String: %s" str)
  | ArrAccess (l_value, expr) ->
      print_with_offset ofs "ArrAcces:\n";
      print_with_offset (ofs + tab) "LValue:\n";
      print_l_value tab (ofs + tab + tab) l_value;
      print_with_offset (ofs + tab) "Expr:\n";
      print_expr tab (ofs + tab + tab) expr

and print_func_call tab ofs = function
  | id, expr_list ->
      print_with_offset ofs (Printf.sprintf "Name: %s\n" id);
      print_with_offset ofs "Params:";
      if List.length expr_list = 0 then print_with_offset 0 " none"
      else print_with_offset 0 "\n";
      List.iter
        (fun e ->
          print_expr tab (ofs + tab) e;
          print_with_offset 0 "\n")
        expr_list

and print_expr tab ofs = function
  | IntLit i -> print_with_offset ofs (Printf.sprintf "IntLit: %d" i)
  | CharLit c -> print_with_offset ofs (Printf.sprintf "CharLit: %c" c)
  | LValue l_value ->
      print_with_offset ofs "LValue:\n";
      print_l_value tab (ofs + tab) l_value
  | ExprFuncCall func_call ->
      print_with_offset ofs "ExprFuncCall:\n";
      print_func_call tab (ofs + tab) func_call
  | UnAritOp (op, expr) ->
      print_with_offset ofs "UnAritOp:\n";
      print_with_offset (ofs + tab) "Operator: ";
      print_un_arit_op tab 0 op;
      print_with_offset (ofs + tab) "Operand:\n";
      print_expr tab (ofs + tab + tab) expr
  | BinAritOp (op, lhs, rhs) ->
      print_with_offset ofs "BinAritOp:\n";
      print_with_offset (ofs + tab) "Operator: ";
      print_bin_arit_op tab 0 op;
      print_with_offset (ofs + tab) "Operands:\n";
      print_expr tab (ofs + tab + tab) lhs;
      print_with_offset 0 "\n";
      print_expr tab (ofs + tab + tab) rhs

let rec print_cond tab ofs = function
  | UnLogicOp (op, cond) ->
      print_with_offset ofs "UnLogicOp:\n";
      print_with_offset (ofs + tab) "Operator: ";
      print_un_logic_op tab 0 op;
      print_with_offset 0 "\n";
      print_with_offset (ofs + tab) "Operand:\n";
      print_cond tab (ofs + tab + tab) cond
  | BinLogicOp (op, lhs, rhs) ->
      print_with_offset ofs "BinLogicOp:\n";
      print_with_offset (ofs + tab) "Operator: ";
      print_bin_logic_op tab 0 op;
      print_with_offset (ofs + tab) "Operands:\n";
      print_cond tab (ofs + tab + tab) lhs;
      print_with_offset 0 "\n";
      print_cond tab (ofs + tab + tab) rhs
  | RelOp (op, lhs, rhs) ->
      print_with_offset ofs "RelOp:\n";
      print_with_offset (ofs + tab) "Operator: ";
      print_rel_op tab 0 op;
      print_with_offset (ofs + tab) "Operands:\n";
      print_expr tab (ofs + tab + tab) lhs;
      print_with_offset 0 "\n";
      print_expr tab (ofs + tab + tab) rhs

let rec print_block tab ofs = function
  | stmt_list ->
      List.iter
        (fun s ->
          print_stmt tab ofs s;
          print_with_offset 0 "\n")
        stmt_list

and print_stmt tab ofs = function
  | EmptyStmt -> ()
  | Assignment (l_value, expr) ->
      print_with_offset ofs "Assignment:\n";
      print_with_offset (ofs + tab) "LValue:\n";
      print_l_value tab (ofs + tab + tab) l_value;
      print_with_offset 0 "\n";
      print_with_offset (ofs + tab) "Expr:\n";
      print_expr tab (ofs + tab + tab) expr
  | Block block ->
      print_with_offset ofs "Block:\n";
      print_block tab (ofs + tab) block
  | StmtFuncCall func_call ->
      print_with_offset ofs "StmtFuncCall:\n";
      print_func_call tab (ofs + tab) func_call
  | IfThen (cond, stmt) ->
      print_with_offset ofs "IfThen:\n";
      print_with_offset (ofs + tab) "Cond:\n";
      print_cond tab (ofs + tab + tab) cond;
      print_with_offset 0 "\n";
      print_with_offset (ofs + tab) "Stmt:\n";
      print_stmt tab (ofs + tab + tab) stmt
  | IfThenElse (cond, if_stmt, else_stmt) ->
      print_with_offset ofs "IfThenElse:\n";
      print_with_offset (ofs + tab) "Cond:\n";
      print_cond tab (ofs + tab + tab) cond;
      print_with_offset 0 "\n";
      print_with_offset (ofs + tab) "IfStmt:\n";
      print_stmt tab (ofs + tab + tab) if_stmt;
      print_with_offset 0 "\n";
      print_with_offset (ofs + tab) "ElseStmt:\n";
      print_stmt tab (ofs + tab + tab) else_stmt
  | WhileDo (cond, stmt) ->
      print_with_offset ofs "WhileDo:\n";
      print_with_offset (ofs + tab) "Cond:\n";
      print_cond tab (ofs + tab + tab) cond;
      print_with_offset 0 "\n";
      print_with_offset (ofs + tab) "Stmt:\n";
      print_stmt tab (ofs + tab + tab) stmt
  | RetExpr None -> print_with_offset ofs "RetExpr: none"
  | RetExpr (Some expr) ->
      print_with_offset ofs "RetExpr:\n";
      print_with_offset (ofs + tab) "Expr:\n";
      print_expr tab (ofs + tab + tab) expr

let print_eval_strat _tab ofs = function
  | Val -> print_with_offset ofs "val"
  | Ref -> print_with_offset ofs "ref"

let print_var_def tab ofs = function
  | id, var_type -> (
      print_with_offset ofs (Printf.sprintf "Name: %s\n" id);
      print_with_offset ofs "Type:";
      match var_type with
      | ScalarVar _ ->
          print_with_offset 0 " ";
          print_var_type tab 0 var_type
      | AggregateVar _ ->
          print_with_offset 0 "\n";
          print_var_type tab (ofs + tab) var_type)

let print_param_def tab ofs = function
  | id, eval_strat, param_type -> (
      print_with_offset ofs (Printf.sprintf "- Name: %s\n" id);
      print_with_offset (ofs + 2) "EvalStrat: ";
      print_eval_strat tab 0 eval_strat;
      print_with_offset 0 "\n";
      print_with_offset (ofs + 2) "Type: ";
      match param_type with
      | ScalarParam _ ->
          print_with_offset 0 " ";
          print_param_type tab 0 param_type
      | AggregateParam _ ->
          print_with_offset 0 "\n";
          print_param_type tab (ofs + 2 + tab) param_type)

let print_header tab ofs = function
  | id, param_def_list, ret_type ->
      print_with_offset ofs (Printf.sprintf "Name: %s\n" id);
      print_with_offset ofs "Params:";
      if List.length param_def_list = 0 then print_with_offset 0 " none\n"
      else print_with_offset 0 "\n";
      List.iter
        (fun s ->
          print_param_def tab (ofs + tab) s;
          print_with_offset 0 "\n")
        param_def_list;
      print_with_offset ofs "RetType: ";
      print_ret_type tab 0 ret_type

let print_func_decl tab ofs = function header -> print_header tab ofs header

let rec print_func_def tab ofs = function
  | header, local_def_list, block -> (
      print_with_offset ofs "Header:\n";
      print_header tab (ofs + tab) header;
      print_with_offset 0 "\n";
      print_with_offset ofs "LocalDefs:";
      if List.length local_def_list = 0 then print_with_offset 0 " none\n"
      else print_with_offset 0 "\n";
      List.iter
        (fun d ->
          print_local_def tab (ofs + tab) d;
          print_with_offset 0 "\n")
        local_def_list;
      print_with_offset ofs "Block:";
      match block with
      | stmt_list ->
          if List.length stmt_list = 0 then print_with_offset 0 " empty\n"
          else print_with_offset 0 "\n";
          print_block tab (ofs + tab) block)

and print_local_def tab ofs = function
  | FuncDecl func_decl ->
      print_with_offset ofs "FuncDecl:\n";
      print_func_decl tab (ofs + tab) func_decl
  | FuncDef func_def ->
      print_with_offset ofs "FuncDef:\n";
      print_func_def tab (ofs + tab) func_def
  | VarDef var_def ->
      print_with_offset ofs "VarDef:\n";
      print_var_def tab (ofs + tab) var_def

let print_program tab ofs = function
  | MainFunc func_def ->
      print_with_offset ofs "MainFunc:\n";
      print_func_def tab (ofs + tab) func_def
