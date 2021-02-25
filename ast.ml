(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Matmul | Elmul | Conv | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or  | Concat | Exponent 

type uop = Neg | Size | Transp | Plusreduce | Mulreduce

type typ = Int | Float

type expr =
    Literal of int
  | Matlit of int list list
  | Imat of int 
  | Zeromat of int * int
  | Rangemat of int
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Fdecl of string * string list * stmt list

type program = stmt list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Matmul -> "*"
  | Elmul -> "@"
  | Conv -> "~"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Exponent -> "^"
  | Concat -> ":"


let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_e_with_uop e o
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")" 
  | Matlit(m) -> let string_of_row row = List.fold_left (fun acc el-> acc ^ (string_of_int el) ^ ",") "" row in
                        (List.fold_left (fun str row -> str ^ string_of_row row ^ ";\n") "[" m) ^ "]"
  | Imat(s) -> "I( " ^ string_of_int s ^ ")"
  | Rangemat(s) -> "Range( " ^ string_of_int s ^ ")"
  | Zeromat(n,m) -> "Zero( " ^ string_of_int n ^ ", " ^ string_of_int m ^ ")"
  | Noexpr -> ""
  and string_of_e_with_uop e = let str_expr = string_of_expr e in function
    Neg -> "-" ^ str_expr
  | Size -> "|" ^ str_expr ^ "|"
  | Transp -> str_expr ^ "^T"
  | Plusreduce -> "+%" ^ str_expr
  | Mulreduce -> "*%" ^ str_expr


let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Fdecl(name, formals, stmts) ->
      "def " ^ name ^ "(" ^ String.concat ", " formals ^ "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"

let string_of_program (stmts) =
  String.concat "" (List.map string_of_stmt stmts)
