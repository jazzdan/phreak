open Common;
open PHPSyntaxTree;
open PHPSyntaxTreePrinter;

let makeParseException e lxb => {
  let cpos = lxb.Lexing.lex_curr_p;
  ParseException
    e
    cpos.Lexing.pos_fname
    cpos.Lexing.pos_lnum
    (cpos.Lexing.pos_cnum - cpos.Lexing.pos_bol)
    (Lexing.lexeme lxb)
  [@implicit_arity]
};

let parse () => {
  let lexbuf = Lexing.from_channel stdin;
  try (Phpparser.single_php_source_file Phplexer.token lexbuf) {
  | PHPAnalException _ as e => raise (makeParseException e lexbuf)
  | Parsing.Parse_error as e => raise (makeParseException e lexbuf)
  | Failure _ as e => raise (makeParseException e lexbuf)
  | End_of_file as e => raise (makeParseException e lexbuf)
  }
};

let main () =>
  try {
    let result = parse ();
    print_endline (toString_PHPSourceFile result);
    print_newline ()
  } {
  | ParseException e fname lnum lpos tok [@implicit_arity] =>
    print_endline (
      "Parse error at " ^ fname ^ ":" ^ string_of_int lnum ^ ":" ^ string_of_int lpos ^ ": " ^ tok
    );
    print_endline (Printexc.to_string e)
  | e =>
    print_endline ("Toplevel exception: " ^ Printexc.to_string e);
    raise e
  };

main ();
