let () =
  let dirname = "programs/" in
  let filenames = [ "program1.grc" ] in
  let test filename =
    let chan = open_in (dirname ^ filename) in
    let lexbuf = Lexing.from_channel chan in
    try
      let ast = Gracec_lib.Parser.program Gracec_lib.Lexer.token lexbuf in
      Gracec_lib.Debug_utils.print_program 2 0 ast
    with
    | Gracec_lib.Lexer.Lexing_error msg -> prerr_endline msg
    | Gracec_lib.Parser.Error ->
        prerr_endline
          (Printf.sprintf "Parse error at line %d:\n" lexbuf.lex_curr_p.pos_lnum)
  in
  List.iter test filenames
