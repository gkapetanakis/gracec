(*open Lexing
  open Grace.Parser

  let parse_file file_name =
    let input_channel = open_in file_name in
    let lexbuf = Lexing.from_channel input_channel in
    let result = Parser.program Lexer.tokenize lexbuf in
    close_in input_channel;
    result

  let rec parse_files () =
    print_string "Enter the file name (or press Ctrl+C to exit): ";
    flush stdout;
    let file_name = read_line () in
    parse_file file_name;
    print_endline "Parsing successful!\n";
    parse_files ()

  let _ = parse_files ()*)
