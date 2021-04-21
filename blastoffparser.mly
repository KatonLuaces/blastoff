/* Ocamlyacc parser for BLAStoff */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA SEMIRING EDGE
%token MATMUL ELMUL ASSIGN FDECL RANGEMAT CONV PLUS MINUS RAISE PLUSREDUCE MULREDUCE
%token NOT EQ NEQ LT LEQ GT GEQ AND OR IMAT ELMAT TRANSP VLINE SEMIRING CONCAT ZEROMAT
%token RETURN IF ELSE FOR WHILE INT BOOL FLOAT VOID
%token <int> INTLITERAL
%token <float> FLOATLITERAL
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%right LBRACK RBRACK
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left MATMUL ELMUL
%left CONCAT CONV
%left RAISE
%left EDGE
%right PLUSREDUCE MULREDUCE
%left TRANSP
%right NOT
%%

program:
  units EOF { $1 }

units:
    /* empty */ { ([], []) }
    | units fdecl { ($2 :: fst $1 , snd $1)  }
    | units stmt { (fst $1, $2 :: snd $1) }

fdecl:
   FDECL ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
   { { fname = $2;
       formals = $4;
       body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    ID                   { [$1]     }
  | formal_list COMMA ID { $3 :: $1 }

expr_list:
    expr                   { [$1]     }
  | expr_list COMMA expr { $3 :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | SEMIRING ID SEMI                           { Semiring $2           }
  | RETURN ret_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

ret_opt:
      /* nothing */ { UnkMatLit([[]]) }
  | expr          { $1 }


lit:
    INTLITERAL { IntLit($1) }
  | FLOATLITERAL { FloatLit($1) }

expr:
      lit          { UnkMatLit([[$1]])            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr MATMUL  expr { Binop($1, Matmul,  $3)   }
  | expr ELMUL  expr { Binop($1, Elmul,  $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr CONV   expr { Binop($1, Conv,  $3)   }
  | expr CONCAT   expr { Binop($1, Concat,  $3)}
  | expr RAISE expr  { Binop($1, Exponent, $3) }
  | expr RAISE TRANSP { Unop(Transp, $1)      }
  | NOT  expr        { Unop(Neg, $2)   }
  | PLUSREDUCE expr  { Unop(Plusreduce, $2)   }
  | MULREDUCE expr   { Unop(Mulreduce, $2)    }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | expr LBRACK expr_list RBRACK   { Selection($1, $3)}
  | expr ASSIGN expr   { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | VLINE expr VLINE   { Unop(Size, $2)       }
  | LBRACK mat_content RBRACK { UnkMatLit($2) }
  | LBRACK graph_content RBRACK { GraphLit($2) }

mat_content:
    mat_row { [$1] }
  | mat_content SEMI mat_row {$3 :: $1}

mat_row:
    lit { [$1] }
  | mat_row COMMA lit {$3 :: $1 }
  | /* nothing */ {[]}

graph_content:
    edge { [$1] }
  | graph_content SEMI edge {$3 :: $1}

edge:
      INTLITERAL EDGE INTLITERAL { ($1, $3) }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
