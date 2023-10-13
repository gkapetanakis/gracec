let () =
  while true do
    print_endline "Give some input, or use Ctrl+C to quit:";
    (* read a string from stdin *)
    let input_string = read_line () in
    (* create a buffer for the lexer from the input string *)
    let lexbuf = Lexing.from_string input_string in
    (* function to tokenize a string and print recognized tokens *)
    let rec tokenize_and_print () =
      try
        (* call the lexer's 'token' rule (tokenize input) *)
        let token = Gracec_lib.Lexer.token lexbuf in
        (* print found tokens *)
        match token with
        | Gracec_lib.Tokens.EOF -> ()
        | _ ->
            Printf.printf "Token: %s\n" (Gracec_lib.Lexer.string_of_token token);
            tokenize_and_print ()
      with Gracec_lib.Lexer.Lexing_error msg -> print_endline msg
    in
    tokenize_and_print ();
    print_endline ""
  done
