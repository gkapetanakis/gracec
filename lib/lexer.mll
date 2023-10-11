{
  (* header *)
  open Lexing
  open Tokens

  exception Syntax_error of string

  let raise_unexpected_character_error lexbuf unrecognized_char =
    let line_num = lexbuf.Lexing.lex_curr_p.pos_lnum in
    let escaped_char = Char.escaped unrecognized_char in
    let error_msg = Printf.sprintf "Unexpected character at line %d: %s" line_num escaped_char in
    raise (Syntax_error error_msg)

  let raise_unclosed_comment_error lexbuf =
    let line_num = lexbuf.Lexing.lex_curr_p.pos_lnum in
    let error_msg = Printf.sprintf "Unclosed comment at line %d" line_num in
    raise (Syntax_error error_msg)

  let extract_char char_lit =
    if char_lit.[0] <> '\\' then
      (* char_lit is just a normal character *) 
      char_lit.[0]
    else
      match char_lit.[1] with
      (* char_lit is a hexadecimal number *)
      | 'x' -> Char.chr (int_of_string ("0x" ^ String.sub char_lit 2 2))
      (* char_lit is a non-numeric escaped character *)
      | 'n' -> '\n'
      | 't' -> '\t'
      | 'r' -> '\r'
      | '0' -> Char.chr 0
      | '\\' -> '\\'
      | '\'' -> '\''
      | '\"' -> '\"'
      | _ -> assert false

  let unescape_string string_lit =
    let list = List.of_seq (String.to_seq string_lit) in
    let rec aux = function
      | [], acc -> List.rev acc
      | '\\' :: 'n' :: rest, acc -> aux (rest, '\n' :: acc)
      | '\\' :: 't' :: rest, acc -> aux (rest, '\t' :: acc)
      | '\\' :: 'r' :: rest, acc -> aux (rest, '\r' :: acc)
      | '\\' :: '0' :: rest, acc -> aux (rest, Char.chr 0 :: acc)
      | '\\' :: '\\' :: rest, acc -> aux (rest, '\\' :: acc)
      | '\\' :: '\'' :: rest, acc -> aux (rest, '\'' :: acc)
      | '\\' :: '\"' :: rest, acc -> aux (rest, '\"' :: acc)
      | '\\' :: 'x' :: hex1 :: hex2 :: rest, acc ->
          aux
            ( rest,
              Char.chr
                (int_of_string ("0x" ^ String.make 1 hex1 ^ String.make 1 hex2))
              :: acc )
      | head :: rest, acc -> aux (rest, head :: acc)
    in
    String.of_seq (List.to_seq (aux (list, [])))

  let string_of_token token =
    match token with
    | CHAR -> "char"
    | INT -> "int"
    | NOTHING -> "nothing"
    | CHAR_LIT c -> Printf.sprintf "char_lit (%c)" c
    | INT_LIT i -> Printf.sprintf "int_lit (%d)" i
    | STRING_LIT s -> Printf.sprintf "string_lit (%s)" s
    | ID i -> Printf.sprintf "id (%s)" i
    | VAR -> "var"
    | FUN -> "fun"
    | REF -> "ref"
    | RETURN -> "return"
    | IF -> "if"
    | THEN -> "then"
    | ELSE -> "else"
    | WHILE -> "while"
    | DO -> "do"
    | EQUAL -> "equal"
    | NOT_EQUAL -> "not_equal"
    | GREATER -> "greater"
    | LESSER -> "lesser"
    | GREATER_EQUAL -> "greater_equal"
    | LESSER_EQUAL -> "lesser_equal"
    | AND -> "and"
    | OR -> "or"
    | NOT -> "not"
    | PLUS -> "plus"
    | MINUS -> "minus"
    | STAR -> "star"
    | DIV -> "div"
    | MOD -> "mod"
    | LPAREN -> "lparen"
    | RPAREN -> "rparen"
    | LBRACKET -> "lbracket"
    | RBRACKET -> "rbracket"
    | LBRACE -> "lbrace"
    | RBRACE -> "rbrace"
    | COMMA -> "comma"
    | COLON -> "colon"
    | SEMICOLON -> "semicolon"
    | LARROW -> "larrow"
    | EOF -> "eof"
}

(* regular expression declarations *)
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let escape_sequence = '\\' (['n' 't' 'r' '0' '\\' '\'' '\"'] | ('x' hex_digit hex_digit))
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let int_literal = digit+ as int_lit
let char_literal = '\'' (([^ '\n' '\'' '\\'] | escape_sequence) as char_lit) '\''
let string_literal = '\"' (([^ '\n' '\"' '\\'] | escape_sequence)+ as string_lit) '\"'
let identifier = letter (letter | digit | '_')*

(* rule declarations *)
rule token = parse
  (* whitespace and comments *)
  | whitespace { token lexbuf (* ignore whitespace *) }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | '$' { single_line_comment lexbuf (* ignore the whole line *) }
  | "$$" { multi_line_comment lexbuf (* ignore everything until next $$ *) }

  (* comparator operators *)
  | '=' { EQUAL }
  | '#' { NOT_EQUAL }
  | '>' { GREATER }
  | '<' { LESSER }
  | ">=" { GREATER_EQUAL }
  | "<=" { LESSER_EQUAL }

  (* logical operators *)
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }

  (* arithmetic operators *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | "div" { DIV }
  | "mod" { MOD }

  (* structural symbols *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | "<-" { LARROW }

  (* statement keywords *)
  | "var" { VAR }
  | "fun" { FUN }
  | "ref" { REF }
  | "return" { RETURN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }

  (* reserved type names *)
  | "char" { CHAR }
  | "int" { INT }
  | "nothing" { NOTHING }

  (* literals *)
  | int_literal { INT_LIT (int_of_string int_lit) }
  | char_literal { CHAR_LIT (extract_char char_lit) }
  | string_literal { STRING_LIT (unescape_string string_lit) }

  (* varible identifiers *)
  | identifier as id { ID (id) }

  (* end of file *)
  | eof { EOF }

  (* bad character *)
  | _ as c { raise_unexpected_character_error lexbuf c }

and single_line_comment = parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { single_line_comment lexbuf }

and multi_line_comment = parse
  | "$$" { token lexbuf }
  | newline { Lexing.new_line lexbuf; multi_line_comment lexbuf }
  | eof { raise_unclosed_comment_error lexbuf }
  | _ { multi_line_comment lexbuf }

{
  (* trailer *)
}
