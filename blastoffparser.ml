type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | FDECL
  | VDECL
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | VOID
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "blastoffparser.mly"
open Ast
# 46 "blastoffparser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACK *);
  263 (* RBRACK *);
  264 (* COMMA *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* ASSIGN *);
  270 (* FDECL *);
  271 (* VDECL *);
  272 (* NOT *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* LT *);
  276 (* LEQ *);
  277 (* GT *);
  278 (* GEQ *);
  279 (* AND *);
  280 (* OR *);
  281 (* RETURN *);
  282 (* IF *);
  283 (* ELSE *);
  284 (* FOR *);
  285 (* WHILE *);
  286 (* INT *);
  287 (* BOOL *);
  288 (* FLOAT *);
  289 (* VOID *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  290 (* LITERAL *);
  291 (* BLIT *);
  292 (* ID *);
  293 (* FLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\003\000\005\000\
\005\000\008\000\008\000\007\000\007\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\011\000\011\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\013\000\013\000\014\000\014\000\
\015\000\012\000\012\000\016\000\016\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\002\000\002\000\000\000\
\001\000\001\000\003\000\000\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\000\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\003\000\004\000\003\000\003\000\001\000\003\000\001\000\003\000\
\001\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\000\000\000\000\001\000\003\000\
\004\000\000\000\007\000\000\000\010\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\006\000\000\000\000\000\012\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\025\000\000\000\024\000\013\000\000\000\000\000\000\000\
\049\000\000\000\045\000\000\000\039\000\040\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\043\000\016\000\000\000\044\000\000\000\015\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\046\000\048\000\000\000\000\000\000\000\
\042\000\000\000\000\000\000\000\020\000\000\000\000\000\000\000\
\018\000\000\000\000\000\019\000"

let yydgoto = "\002\000\
\003\000\004\000\008\000\009\000\014\000\020\000\022\000\015\000\
\037\000\038\000\048\000\077\000\042\000\043\000\044\000\078\000"

let yysindex = "\008\000\
\000\000\000\000\000\000\001\000\223\254\236\254\000\000\000\000\
\000\000\015\255\000\000\009\255\000\000\047\255\053\255\069\255\
\030\255\000\000\000\000\059\255\000\000\042\255\184\255\000\000\
\000\000\052\255\184\255\184\255\184\255\090\255\096\255\100\255\
\000\000\000\000\005\255\000\000\000\000\229\255\157\000\078\255\
\000\000\058\255\000\000\101\255\000\000\000\000\217\000\109\255\
\184\255\184\255\184\255\184\255\184\255\000\000\184\255\184\255\
\184\255\184\255\184\255\184\255\184\255\184\255\184\255\184\255\
\184\255\184\255\000\000\000\000\052\255\000\000\052\255\000\000\
\179\000\121\255\201\000\217\000\105\255\117\255\217\000\084\255\
\084\255\000\000\000\000\223\255\223\255\107\255\107\255\107\255\
\107\255\233\000\002\255\000\000\000\000\148\255\184\255\148\255\
\000\000\184\255\099\255\002\000\000\000\217\000\148\255\184\255\
\000\000\124\255\148\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\125\255\000\000\000\000\135\255\000\000\
\000\000\000\000\000\000\095\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\133\255\000\000\000\000\000\000\
\000\000\000\000\205\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\062\255\000\000\000\000\086\255\000\000\
\000\000\133\255\000\000\136\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\046\255\000\000\137\255\054\255\026\000\
\050\000\000\000\000\000\138\000\141\000\074\000\082\000\106\000\
\114\000\188\255\007\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\131\255\000\000\000\000\082\255\000\000\139\255\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\123\000\000\000\000\000\000\000\120\000\000\000\
\213\255\233\255\208\255\000\000\000\000\022\000\000\000\000\000"

let yytablesize = 511
let yytable = "\039\000\
\007\000\074\000\010\000\045\000\046\000\047\000\052\000\038\000\
\001\000\038\000\055\000\056\000\057\000\058\000\038\000\011\000\
\012\000\053\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\073\000\047\000\075\000\076\000\079\000\038\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\090\000\091\000\023\000\013\000\024\000\025\000\026\000\
\052\000\016\000\099\000\027\000\101\000\052\000\041\000\106\000\
\041\000\028\000\069\000\105\000\017\000\041\000\047\000\108\000\
\070\000\019\000\029\000\030\000\047\000\031\000\032\000\100\000\
\018\000\006\000\102\000\033\000\034\000\035\000\036\000\023\000\
\047\000\024\000\068\000\026\000\053\000\041\000\022\000\027\000\
\022\000\053\000\092\000\049\000\093\000\028\000\057\000\058\000\
\012\000\050\000\012\000\012\000\012\000\051\000\029\000\030\000\
\012\000\031\000\032\000\097\000\071\000\072\000\012\000\033\000\
\034\000\035\000\036\000\055\000\056\000\057\000\058\000\012\000\
\012\000\095\000\012\000\012\000\098\000\103\000\107\000\008\000\
\012\000\012\000\012\000\012\000\017\000\021\000\017\000\017\000\
\017\000\009\000\050\000\051\000\017\000\021\000\021\000\040\000\
\000\000\000\000\017\000\000\000\000\000\023\000\000\000\024\000\
\000\000\026\000\000\000\017\000\017\000\027\000\017\000\017\000\
\000\000\000\000\000\000\028\000\017\000\017\000\017\000\017\000\
\000\000\000\000\000\000\000\000\029\000\030\000\000\000\031\000\
\032\000\000\000\000\000\000\000\000\000\033\000\034\000\035\000\
\036\000\023\000\000\000\000\000\037\000\026\000\037\000\000\000\
\000\000\027\000\000\000\037\000\000\000\000\000\000\000\028\000\
\000\000\000\000\000\000\000\000\000\000\026\000\000\000\026\000\
\000\000\000\000\037\000\037\000\026\000\026\000\026\000\026\000\
\026\000\033\000\034\000\035\000\036\000\026\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\054\000\000\000\055\000\
\056\000\057\000\058\000\000\000\000\000\055\000\056\000\057\000\
\058\000\061\000\062\000\063\000\064\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\000\000\000\000\000\000\
\000\000\000\000\104\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\055\000\056\000\057\000\058\000\005\000\006\000\
\000\000\000\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\027\000\000\000\027\000\000\000\000\000\000\000\
\000\000\027\000\027\000\027\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\028\000\000\000\028\000\000\000\000\000\000\000\
\000\000\028\000\028\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\033\000\000\000\033\000\000\000\000\000\000\000\
\000\000\033\000\034\000\000\000\034\000\000\000\000\000\000\000\
\000\000\034\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\035\000\000\000\035\000\000\000\000\000\000\000\
\000\000\035\000\036\000\000\000\036\000\000\000\000\000\000\000\
\000\000\036\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\031\000\000\000\031\000\032\000\000\000\032\000\
\000\000\031\000\000\000\000\000\032\000\000\000\000\000\000\000\
\000\000\000\000\031\000\031\000\000\000\032\000\032\000\067\000\
\031\000\031\000\000\000\032\000\032\000\055\000\056\000\057\000\
\058\000\000\000\000\000\000\000\000\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\094\000\000\000\000\000\
\000\000\000\000\000\000\055\000\056\000\057\000\058\000\000\000\
\000\000\000\000\000\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\096\000\000\000\000\000\000\000\000\000\
\000\000\055\000\056\000\057\000\058\000\000\000\000\000\000\000\
\000\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\055\000\056\000\057\000\058\000\000\000\000\000\000\000\
\000\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\055\000\056\000\057\000\058\000\000\000\000\000\000\000\
\000\000\059\000\060\000\061\000\062\000\063\000\064\000"

let yycheck = "\023\000\
\000\000\050\000\036\001\027\000\028\000\029\000\002\001\001\001\
\001\000\003\001\009\001\010\001\011\001\012\001\008\001\036\001\
\002\001\013\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\049\000\050\000\051\000\052\000\053\000\024\001\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\002\001\036\001\004\001\005\001\006\001\
\003\001\003\001\094\000\010\001\096\000\008\001\001\001\104\000\
\003\001\016\001\001\001\103\000\008\001\008\001\001\001\107\000\
\007\001\036\001\025\001\026\001\007\001\028\001\029\001\095\000\
\004\001\015\001\098\000\034\001\035\001\036\001\037\001\002\001\
\104\000\004\001\005\001\006\001\003\001\034\001\001\001\010\001\
\003\001\008\001\069\000\002\001\071\000\016\001\011\001\012\001\
\002\001\002\001\004\001\005\001\006\001\002\001\025\001\026\001\
\010\001\028\001\029\001\003\001\008\001\001\001\016\001\034\001\
\035\001\036\001\037\001\009\001\010\001\011\001\012\001\025\001\
\026\001\001\001\028\001\029\001\008\001\027\001\003\001\003\001\
\034\001\035\001\036\001\037\001\002\001\001\001\004\001\005\001\
\006\001\003\001\003\001\003\001\010\001\003\001\020\000\024\000\
\255\255\255\255\016\001\255\255\255\255\002\001\255\255\004\001\
\255\255\006\001\255\255\025\001\026\001\010\001\028\001\029\001\
\255\255\255\255\255\255\016\001\034\001\035\001\036\001\037\001\
\255\255\255\255\255\255\255\255\025\001\026\001\255\255\028\001\
\029\001\255\255\255\255\255\255\255\255\034\001\035\001\036\001\
\037\001\002\001\255\255\255\255\001\001\006\001\003\001\255\255\
\255\255\010\001\255\255\008\001\255\255\255\255\255\255\016\001\
\255\255\255\255\255\255\255\255\255\255\001\001\255\255\003\001\
\255\255\255\255\023\001\024\001\008\001\009\001\010\001\011\001\
\012\001\034\001\035\001\036\001\037\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\001\001\255\255\009\001\
\010\001\011\001\012\001\255\255\255\255\009\001\010\001\011\001\
\012\001\019\001\020\001\021\001\022\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\255\255\255\255\255\255\
\255\255\255\255\001\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\009\001\010\001\011\001\012\001\014\001\015\001\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\001\001\255\255\003\001\255\255\255\255\255\255\
\255\255\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\001\001\255\255\003\001\255\255\255\255\255\255\
\255\255\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\001\001\255\255\003\001\255\255\255\255\255\255\
\255\255\008\001\001\001\255\255\003\001\255\255\255\255\255\255\
\255\255\008\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\001\001\255\255\003\001\255\255\255\255\255\255\
\255\255\008\001\001\001\255\255\003\001\255\255\255\255\255\255\
\255\255\008\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\001\001\255\255\003\001\001\001\255\255\003\001\
\255\255\008\001\255\255\255\255\008\001\255\255\255\255\255\255\
\255\255\255\255\017\001\018\001\255\255\017\001\018\001\003\001\
\023\001\024\001\255\255\023\001\024\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\003\001\255\255\255\255\
\255\255\255\255\255\255\009\001\010\001\011\001\012\001\255\255\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\003\001\255\255\255\255\255\255\255\255\
\255\255\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  FDECL\000\
  VDECL\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  VOID\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "blastoffparser.mly"
            ( _1 )
# 349 "blastoffparser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "blastoffparser.mly"
                 ( ([], [])               )
# 355 "blastoffparser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "blastoffparser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 363 "blastoffparser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "blastoffparser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 371 "blastoffparser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 41 "blastoffparser.mly"
     ( { fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7
	 body = List.rev _8 } )
# 384 "blastoffparser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 48 "blastoffparser.mly"
                    (_2 :: _1)
# 392 "blastoffparser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "blastoffparser.mly"
             (_2)
# 399 "blastoffparser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "blastoffparser.mly"
                  ( [] )
# 405 "blastoffparser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 55 "blastoffparser.mly"
                  ( _1 )
# 412 "blastoffparser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "blastoffparser.mly"
                         ( [_1]     )
# 419 "blastoffparser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "blastoffparser.mly"
                         ( _3 :: _1 )
# 427 "blastoffparser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "blastoffparser.mly"
                   ( [] )
# 433 "blastoffparser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 63 "blastoffparser.mly"
                   ( _2 :: _1 )
# 441 "blastoffparser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "blastoffparser.mly"
                                            ( Expr _1               )
# 448 "blastoffparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 67 "blastoffparser.mly"
                                            ( Return _2             )
# 455 "blastoffparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 68 "blastoffparser.mly"
                                            ( Block(List.rev _2)    )
# 462 "blastoffparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "blastoffparser.mly"
                                            ( If(_3, _5, Block([])) )
# 470 "blastoffparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "blastoffparser.mly"
                                            ( If(_3, _5, _7)        )
# 479 "blastoffparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 72 "blastoffparser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 489 "blastoffparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 73 "blastoffparser.mly"
                                            ( While(_3, _5)         )
# 497 "blastoffparser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "blastoffparser.mly"
                  ( Noexpr )
# 503 "blastoffparser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "blastoffparser.mly"
                  ( _1 )
# 510 "blastoffparser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 80 "blastoffparser.mly"
                     ( Literal(_1)            )
# 517 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "blastoffparser.mly"
              ( Fliteral(_1)           )
# 524 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 82 "blastoffparser.mly"
                     ( BoolLit(_1)            )
# 531 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "blastoffparser.mly"
                     ( Id(_1)                 )
# 538 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "blastoffparser.mly"
                     ( Binop(_1, Add,   _3)   )
# 546 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "blastoffparser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 554 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "blastoffparser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 562 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "blastoffparser.mly"
                     ( Binop(_1, Div,   _3)   )
# 570 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "blastoffparser.mly"
                     ( Binop(_1, Equal, _3)   )
# 578 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "blastoffparser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 586 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "blastoffparser.mly"
                     ( Binop(_1, Less,  _3)   )
# 594 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "blastoffparser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 602 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "blastoffparser.mly"
                     ( Binop(_1, Greater, _3) )
# 610 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "blastoffparser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 618 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "blastoffparser.mly"
                     ( Binop(_1, And,   _3)   )
# 626 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "blastoffparser.mly"
                     ( Binop(_1, Or,    _3)   )
# 634 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "blastoffparser.mly"
                         ( Unop(Neg, _2)      )
# 641 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "blastoffparser.mly"
                     ( Unop(Not, _2)          )
# 648 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "blastoffparser.mly"
                     ( Assign(_1, _3)         )
# 656 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 99 "blastoffparser.mly"
                              ( Call(_1, _3)  )
# 664 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "blastoffparser.mly"
                       ( _2                   )
# 671 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mat_content) in
    Obj.repr(
# 101 "blastoffparser.mly"
                              ( _2            )
# 678 "blastoffparser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mat_row) in
    Obj.repr(
# 104 "blastoffparser.mly"
            ( _1 )
# 685 "blastoffparser.ml"
               : 'mat_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mat_content) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mat_row) in
    Obj.repr(
# 105 "blastoffparser.mly"
                             ( [_1; _3] )
# 693 "blastoffparser.ml"
               : 'mat_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'el) in
    Obj.repr(
# 108 "blastoffparser.mly"
       ( [_1] )
# 700 "blastoffparser.ml"
               : 'mat_row))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'el) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mat_row) in
    Obj.repr(
# 109 "blastoffparser.mly"
                     (_1 : _3 )
# 708 "blastoffparser.ml"
               : 'mat_row))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "blastoffparser.mly"
            ( _1 )
# 715 "blastoffparser.ml"
               : 'el))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "blastoffparser.mly"
                  ( [] )
# 721 "blastoffparser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 116 "blastoffparser.mly"
               ( List.rev _1 )
# 728 "blastoffparser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "blastoffparser.mly"
                            ( [_1] )
# 735 "blastoffparser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "blastoffparser.mly"
                         ( _3 :: _1 )
# 743 "blastoffparser.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
