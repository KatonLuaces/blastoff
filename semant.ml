(* Semantic checking for the BLAStoff compiler *)

open Ast
module StringMap = Map.Make (String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check (funcs, stmts) =
  let check_vars loc stmt_lst =
    let add_decl lst = function
      | Expr e ->
        (match e with
        | Id var -> var :: lst
        | _ -> lst)
      | _ -> lst
    in
    let decls = List.fold_left add_decl [] stmt_lst in
    let rec check_dups = function
      | [] -> ()
      | n1 :: n2 :: _ when n1 = n2 -> raise (Failure ("duplicate " ^ n1 ^ " in " ^ loc))
      | _ :: tl -> check_dups tl
    in
    check_dups (List.sort compare decls)
  in
  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, args) =
      StringMap.add name { fname = name; formals = args; body = [] } map
    in
    List.fold_left add_bind StringMap.empty Definitions.functions
  in
  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *) in
    match fd with
    (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
  in
  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls funcs in
  let find_func fname =
    try StringMap.find fname function_decls with
    | Not_found -> raise (Failure ("Undeclared function " ^ fname))
  in
  let is_float = function
    | IntLit _ -> false
    | FloatLit _ -> true
  in
  let contains_float m = List.exists (fun lst -> List.exists is_float lst) m in
  let get_char_codes s =
    (* Takes string, returns backwards list of character codes *)
    let rec exp i l = if i < 0 then l else exp (i - 1) (Char.code s.[i] :: l) in
    exp (String.length s - 1) []
  in
  let rec check_expr = function
    | Call (fname, args) as call ->
      let fd = find_func fname in
      let num_formals = List.length fd.formals in
      if List.length args != num_formals
      then
        raise
          (Failure
             ("Expecting "
             ^ string_of_int num_formals
             ^ " arguments in "
             ^ string_of_expr call))
      else Call (fname, List.map check_expr args)
    | StringLit s ->
      let chars = List.rev (get_char_codes s) in
      IntMatLit (List.map (fun c -> [ c ]) chars)
    | UnkMatLit m ->
      let has_float = contains_float m in
      (match has_float with
      | true -> 
        FloatMatLit 
          (List.map
             (fun row ->
               List.map
                 (function
                   | IntLit lit -> float_of_int lit
                   | FloatLit lit -> lit)
                 row
             ) m)
      | false ->
        IntMatLit
          (List.map
             (fun row ->
               List.map
                 (function
                   | IntLit lit -> lit
                   | FloatLit _ -> raise (Failure "Expected Integers in Matrix"))
                 row)
             m))
    | Id n -> Id n
    | Binop (e1, op, e2) -> Binop (check_expr e1, op, check_expr e2)
    | Unop (op, e) -> Unop (op, check_expr e)
    | FloatMatLit _ -> raise (Failure "Unexpected float matrix in semant checking")
    | IntMatLit _ -> raise (Failure "Unexpected float matrix in semant checking")
    | GraphLit g -> GraphLit g
    | Selection (e, args) -> Selection (check_expr e, List.map check_expr args)
    | IdAssign (n, e) -> IdAssign (n, check_expr e)
    | SelectAssign (n, args, e) -> SelectAssign (n, List.map check_expr args, check_expr e)
    | Assign (e1, e2) ->
      let fix_assign = function
        | Id i, e -> check_expr (IdAssign (i, e))
        | Selection (Id n, args), e -> check_expr (SelectAssign (n, args, e))
        | _ -> raise (Failure "Bad left side of assignment, expected ID or ID[...]")
      in
      fix_assign (e1, e2)
  in
  let rec check_stmt = function
    | Expr e -> Expr (check_expr e)
    | Semiring ring ->
      (match List.mem_assoc ring Definitions.rings with
      | true -> Semiring ring
      | false -> raise (Failure ("Unknown semiring " ^ ring)))
    | Block bl -> Block (check_stmt_list bl)
    | If (p, b1, b2) -> If (check_expr p, check_stmt b1, check_stmt b2)
    | While (p, s) -> While (check_expr p, check_stmt s)
    | Return e -> Return (check_expr e)
  and check_stmt_list = function
    | [ (Return _ as s) ] -> [ check_stmt s ]
    | Return _ :: _ -> raise (Failure "Unreachable statments after return")
    | Block sl :: ss -> check_stmt_list (sl @ ss)
    | s :: ss -> check_stmt s :: check_stmt_list ss
    | [] -> []
  in
  let add_return body =
    match List.rev body with
    | Return _ :: _ -> body
    | _ as l -> List.rev (Return (UnkMatLit [ [] ]) :: l)
  in
  let check_function func =
    let _ = check_vars "body" func.body in
    let checked_body = check_stmt_list (add_return func.body) in
    { fname = func.fname; formals = func.formals; body = checked_body }
  in
  List.map check_function funcs, List.map check_stmt stmts
;;
