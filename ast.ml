(* Abstract Syntax Tree and functions for printing it *)

type op =
  | Add
  | Sub
  | Matmul
  | Elmul
  | Conv
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | Concat
  | Exponent

type uop =
  | Neg
  | Size
  | Transp
  | Plusreduce
  | Mulreduce

type lit =
  | IntLit of int
  | FloatLit of float

type expr =
  | Literal of lit (* TODO(Katon): This should be unnecessary eventually and thus should be removed*)
  | UnkMatLit of lit list list
  | IntMatLit of int list list
  | FloatMatLit of float list list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
  | Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt

type func_decl =
  { fname : string
  ; formals : string list
  ; body : stmt list
  }

type program = func_decl list * stmt list

(* Pretty-printing functions *)

let string_of_op = function
  | Add -> "+"
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

let string_of_mat print_lit m = 
   let string_of_row row =
      List.fold_left (fun acc lit -> acc ^ print_lit lit ^ ",") "" row
    in
    List.fold_left (fun str row -> str ^ string_of_row row ^ ";\n") "[" m ^ "]"

let rec string_of_expr = function
  | Literal l -> (match l with IntLit ilit -> string_of_int ilit 
                            | FloatLit flit -> string_of_float flit)
  | Id s -> s
  | Binop (e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop (o, e) -> string_of_e_with_uop e o
  | Assign (v, e) -> v ^ " = " ^ string_of_expr e
  | Call (f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | UnkMatLit m -> string_of_mat (fun lit -> match lit with 
        IntLit ilit -> string_of_int ilit| FloatLit flit -> string_of_float flit) m
  | IntMatLit m -> string_of_mat string_of_int m
  | FloatMatLit m -> string_of_mat string_of_float m
  | Noexpr -> ""

and string_of_e_with_uop e =
  let str_expr = string_of_expr e in
  function
  | Neg -> "-" ^ str_expr
  | Size -> "|" ^ str_expr ^ "|"
  | Transp -> str_expr ^ "^T"
  | Plusreduce -> "+%" ^ str_expr
  | Mulreduce -> "*%" ^ str_expr

let rec string_of_stmt = function
  | Block stmts -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr expr -> string_of_expr expr ^ ";\n"
  | Return expr -> "return " ^ string_of_expr expr ^ ";\n"
  | If (e, s, Block []) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If (e, s1, s2) ->
    "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While (e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_func func =
  "def "
  ^ func.fname
  ^ "("
  ^ String.concat ", " func.formals
  ^ ")"
  ^ "{\n"
  ^ String.concat "" (List.map string_of_stmt func.body)
  ^ "}\n"

let string_of_program (funcs, stmts) =
  String.concat "" (List.map string_of_func funcs)
  ^ "\n"
  ^ String.concat "" (List.map string_of_stmt stmts)
