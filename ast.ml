(* Abstract Syntax Tree and functions for printing it *)

type op =
  | Add
  | Matmul
  | Elmul
  | Conv
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | Concat
  | Exponent

type uop =
  | Neg
  | Transp
  | Plusreduce
  | Mulreduce
  | Size

type lit =
  | IntLit of int
  | FloatLit of float

type expr =
  | GraphLit of (int * int) list
  | UnkMatLit of lit list list
  | IntMatLit of int list list
  | FloatMatLit of float list list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | IdAssign of string * expr
  | SelectAssign of string * expr list * expr
  | Selection of expr * expr list
  | Call of string * expr list
  | StringLit of string

type stmt =
  | Semiring of string
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
  | Matmul -> "*"
  | Elmul -> "@"
  | Conv -> "~"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | Exponent -> "^"
  | Concat -> ":"

let string_of_mat lit_to_string m =
   let string_of_row row =
     String.concat "," (List.fold_left (fun acc lit ->  (lit_to_string lit) :: acc) [] row)
    in
    "[" ^ (String.concat ";" (List.fold_left (fun acc row -> (string_of_row row) :: acc) []  m)) ^ "]"

let string_of_graph g =
   let string_of_edge (v1,v2) = (string_of_int v1) ^ "->" ^ (string_of_int v2) in
   "[" ^ (String.concat ";" (List.map string_of_edge g)) ^ "]"

let rec string_of_expr = function
  | Id s -> s
  | Binop (e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop (o, e) -> string_of_e_with_uop e o
  | Assign (e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | IdAssign (s, e) -> s ^ " = " ^ string_of_expr e
  | Call (f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | UnkMatLit m -> string_of_mat (fun lit -> match lit with
        IntLit ilit -> string_of_int ilit| FloatLit flit -> string_of_float flit) m
  | IntMatLit m -> string_of_mat string_of_int m
  | GraphLit g -> string_of_graph g
  | StringLit s -> "\"" ^ s ^ "\""
  | FloatMatLit m -> string_of_mat string_of_float m
  | Selection (e, args) -> (string_of_expr e) ^ "[" ^ String.concat ", " (List.map string_of_expr args) ^ "]"
  | SelectAssign (s, args, e) -> s ^ "[" ^ String.concat ", " (List.map string_of_expr args) ^ "]" ^ " = " ^ string_of_expr e
and string_of_e_with_uop e =
  let str_expr = string_of_expr e in
  function
  | Neg -> "!" ^ str_expr
  | Size -> "|" ^ str_expr ^ "|"
  | Transp -> str_expr ^ "^T"
  | Plusreduce -> "+%" ^ str_expr
  | Mulreduce -> "*%" ^ str_expr

let rec string_of_stmt = function
  | Semiring ring -> "#" ^ ring ^ "\n"
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
