let () =
  let dirname = "programs/" in
  let filenames = [ "program1.grc" ] in
  let test filename =
    let chan = open_in (dirname ^ filename) in
    let lexbuf = Lexing.from_channel chan in
    let rec tokenize_and_print () =
      try
        (* call the lexer's 'token' rule (tokenize input) *)
        let token = Gracec_lib.Lexer.token lexbuf in
        (* print found tokens *)
        match token with
        | Gracec_lib.Tokens.EOF -> ()
        | _ ->
            print_endline (Gracec_lib.Lexer.string_of_token token);
            tokenize_and_print ()
      with Gracec_lib.Lexer.Lexing_error msg -> print_endline msg
    in
    tokenize_and_print ()
  in
  List.iter test filenames

(*
  while true do
    print_endline "Give some input, or use Ctrl+C to quit:";
    let input_string = read_line () in
    let lexbuf = Lexing.from_string input_string in
  done
*)
