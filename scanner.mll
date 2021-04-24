(* Ocamllex scanner for BLAStoff *)

{ open Blastoffparser

(* http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual026.html#toc111 *)
let keyword_table = Hashtbl.create 97
let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "while", WHILE;
      "return", RETURN;
      "if", IF;
      "else", ELSE;
      "for", FOR;
      "def", FDECL;
      "T", TRANSP]
}

let digit = ['0'-'9']
let arrow = ['-']['>']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"     { single_line_comment lexbuf }
| '-'?digit* as lxm { INTLITERAL(int_of_string lxm) }
| ['-']?digit*['.']digit* as lxm { FLOATLITERAL(float_of_string lxm) }
| '|'      { VLINE }
| '['      { LBRACK }
| ']'      { RBRACK }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '\''[^'\'']*'\'' as str { STRINGLITERAL(String.sub str 1 ((String.length str) - 2)) }
| '@'      { ELMUL   }
| "@="     { ELMULASSIGN }
| '~'      { CONV }
| "~="     { CONVASSIGN }
| ':'      { CONCAT }
| ":="     { CONCATASSIGN }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| "+="     { PLUSASSIGN }
| '*'      { MATMUL }
| "*="     { MATMULASSIGN }
| '='      { ASSIGN }
| arrow     { EDGE }
| ['+']['%']     { PLUSREDUCE }
| ['*']['%']     { MULREDUCE }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| '^'      { RAISE }
| "^="     { RAISEASSIGN }
| '!'      { NOT }
| '#'      { SEMIRING }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm
  { (*print_endline "find lxm: ";
      print_endline lxm;*)
    try
      Hashtbl.find keyword_table lxm
    with Not_found ->
      ID(lxm)}
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
      "*/" { token lexbuf }
  | _    { comment lexbuf }
and single_line_comment = parse
  '\n' { token lexbuf }
  | _ { single_line_comment lexbuf }
