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
                     "let", VDECL; 
                     "I", IMAT;
                     "Zero", ZEROMAT;
                     "T", TRANSP]
}

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '|'      { VLINE }
| '['      { LBRACK }
| ']'      { RBRACK }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '#'      { SEMIRING }
| '@'      { ELMULT   }
| ':'      { CONCAT }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { MATMUL }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| digits as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm
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
