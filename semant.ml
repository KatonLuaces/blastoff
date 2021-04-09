(* Semantic checking for the BLAStoff compiler *)

open Ast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check (funcs, stmts) =
  
  (*TODO: Check vars (in funcs and in top level)*)
  let rec check_vars loc stmt_lst = 
    let add_decl lst = function
      Expr e -> match e with Id var -> var :: lst | _ -> lst
    in
    let decls = List.fold_left add_decl [] stmt_lst
    in
    let rec check_dups = function
      [] -> ()
    | n1 :: n2 :: _ when n1 = n2 -> raise (Failure ("duplicate " ^ n1 ^ " in " ^ loc))
    | _ :: tl -> check_dups tl
    in check_dups (List.sort compare decls)
  in
  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map name = StringMap.add name {
      fname = name; 
      formals = ["n"];
      body = [] } map
    in List.fold_left add_bind StringMap.empty [ "Range";
			                         "I";
			                         "Zeros"; ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls funcs
  in 
  let find_func fname = 
    try StringMap.find fname function_decls
    with Not_found -> raise (Failure ("Undeclared function " ^ fname ))
  in
  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    let _ = check_vars "body" func.body in
    func
in
let rec check_expr = function
    Call(fname, args) as call ->
        let fd = find_func fname in
        let num_formals = List.length fd.formals in
        if List.length args != num_formals then
        raise (Failure ("Expecting " ^ (string_of_int num_formals) ^ " arguments in " ^ string_of_expr call))
        else call
    | e -> e
in
let rec check_stmt = function 
      Expr e -> Expr (check_expr e)
    | stmt -> stmt
  in (List.map check_function funcs, List.map check_stmt stmts)
