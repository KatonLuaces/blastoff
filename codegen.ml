module L = Llvm
module A = Ast
open Ast
open Definitions
module StringMap = Map.Make (String)

let translate (functions, statements) =
  let main_fdecl = { fname = "main"; formals = []; body = List.rev statements } in

  let function_decls : (L.llvalue * func_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.fname
      and formal_types = Array.of_list (List.map (fun (_) -> matrix_t) fdecl.formals)
      in
      let ftype = L.function_type matrix_t formal_types in
      StringMap.add name (L.define_function name ftype blastoff_module, fdecl) m
    in
    let decls = List.fold_left function_decl StringMap.empty functions in
    StringMap.add main_fdecl.fname (
      L.define_function main_fdecl.fname (L.function_type i32_t (Array.of_list [])) blastoff_module, main_fdecl
    ) decls
  in

  let build_function_body fdecl is_main =
    let func, _ = try StringMap.find fdecl.fname function_decls with Not_found -> raise (Failure ("Unknown function, " ^ fdecl.fname)) in
    let builder = L.builder_at_end context (L.entry_block func) in
    let local_vars =
      let add_formal m n p =
        L.set_value_name n p;
        let local = L.build_alloca matrix_t n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m
      in
      let add_local m n =
        let local_var = L.build_alloca matrix_t n builder in
        StringMap.add n local_var m
      in
      let formals =
        List.fold_left2
          add_formal
          StringMap.empty
          fdecl.formals
          (Array.to_list (L.params func))
      in
      let add_assignment lst = function Expr e -> (match e with IdAssign (v, _) -> (match v with id -> id :: lst | _ -> lst )
                                                              | _ -> lst) | _ -> lst in
      let locals = List.fold_left add_assignment [] fdecl.body in
      List.fold_left add_local formals locals
    in
    let lookup n = try StringMap.find n local_vars with Not_found -> raise (Failure ("Undeclared variable " ^ n)) in
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in
    let build_graph_matrix m  =
      let max3 a b c =  if a >= b && a >= c then a else if b >= c && b >= a then b else c in
      let dim = 1 + List.fold_left (fun acc elem -> max3 acc (fst elem) (snd elem)) 0 m in
      let mat = L.build_call matrix_create_f [| 
        L.const_int i32_t dim ; 
        L.const_int i32_t dim 
      |] "matrix_create" builder in
        List.iter (
          fun elem -> (
            ignore(L.build_call matrix_setelem_f [| mat;
                                                    L.const_int i32_t 1;
                                                    L.const_int i32_t (fst elem);
                                                    L.const_int i32_t (snd elem) |] "matrix_setelem" builder)
          ) 
        ) m ; mat
    in
    let build_int_matrix m  =
      let mat = L.build_call matrix_create_f [|
        L.const_int i32_t (List.length m) ;
        L.const_int i32_t (List.length (List.hd m))
      |] "matrix_create" builder
        in
        List.iteri (
          fun i row -> (
            List.iteri (
              fun j elem ->
                ignore(L.build_call matrix_setelem_f [| mat;
                                                        L.const_int i32_t elem;
                                                        L.const_int i32_t i;
                                                        L.const_int i32_t j |] "matrix_setelem" builder)
            )
          ) (List.rev row)
        ) (List.rev m) ; mat
    in
    let rec fill_select_args args =
      let zero = L.build_call matrix_create_f [|L.const_int i32_t 1 ; L.const_int i32_t 1|] "matrix_create" builder in
      let base = L.build_call matrix_create_f [|L.const_int i32_t 1 ; L.const_int i32_t 1|] "matrix_create" builder in
      let one = L.build_call matrix_setelem_f [|base; L.const_int i32_t 1 ; L.const_int i32_t 0 ; L.const_int i32_t 0|] "matrix_setelem" builder ; base in
      match args with
      | [_;_;_;_] as l -> (l)
      | [_;_;_] as l -> fill_select_args (one::l)
      | [_;_] as l -> fill_select_args (one::l)
      | [_] as l -> fill_select_args (zero::l)
      | _ -> raise (Failure "Too many/few arguments to selection")
    in
    let rec build_expr builder e = match e with
      | IntMatLit m -> build_int_matrix m
      | GraphLit m -> build_graph_matrix m
      | FloatMatLit _ -> raise (Failure "Float Matrix Literal")
      | IdAssign (v , e) -> let comp_e = build_expr builder e in
        (match v with s -> ignore(L.build_store comp_e (lookup s) builder)); comp_e
      | Call(fname, exprs) ->
        (match fname with
           "print" -> (match exprs with
              [e] -> L.build_call matrix_print_f [| (build_expr builder e) |] "matrix_print" builder
             | _ -> raise (Failure "Invalid list of expressions passed to print"))
         | "toString" -> (match exprs with
              [e] -> L.build_call matrix_tostring_f [| (build_expr builder e) |] "matrix_tostring" builder
             | _ -> raise (Failure "Invalid list of expressions passed to toString"))
         | "I" -> (match exprs with
              [e] -> L.build_call matrix_identity_f [| (build_expr builder e) |] "matrix_identity" builder
             | _ -> raise (Failure "Invalid list of expressions passed to I"))
         | "Zero" -> (match exprs with
              [e] -> L.build_call matrix_zero_f [| (build_expr builder e) |] "matrix_zero" builder
             | _ -> raise (Failure "Invalid list of expressions passed to Zero"))
         | "range" -> (match exprs with
              [e] -> L.build_call matrix_range_f [| (build_expr builder e) |] "matrix_range" builder
             | _ -> raise (Failure "Invalid list of expressions passed to range"))
         | f -> let (fdef, fdecl) = (try StringMap.find f function_decls with Not_found -> raise (Failure ("Undeclared function, " ^ f ^ ", found in code generation"))) in
           let args = List.map (build_expr builder) (List.rev exprs) in
           L.build_call fdef (Array.of_list args) (fdecl.fname ^ "_result") builder)
      | Binop (e1, op, e2) ->
          let e1' = build_expr builder e1
          and e2' = build_expr builder e2 in (
            match op with
              A.Matmul -> L.build_call matrix_mul_f [| e1'; e2'|] "matrix_mul" builder
            | A.Conv   -> L.build_call matrix_conv_f [| e1'; e2'|] "matrix_conv" builder
            | A.Elmul  -> L.build_call matrix_elmul_f [| e1'; e2'|] "matrix_elmul" builder
            | A.Add    -> L.build_call matrix_eladd_f [| e1'; e2'|] "matrix_eladd" builder
            | A.Neq  -> L.build_fcmp L.Fcmp.One e1' e2' "fneq" builder
          )
      | UnkMatLit _ -> raise (Failure "Type of matrix is unknown")
      | Unop (op, e1) -> let _ = build_expr builder e1 in (match op with A.Size -> raise (Failure "Unop call made"))
      | Id v -> L.build_load (lookup v) v builder
      | Selection (e, args) ->
        let partialargs' = List.map (build_expr builder) args in
        let filledargs' = fill_select_args partialargs' in
        let revfilledargs' = List.rev filledargs' in
        let e' = build_expr builder e in
        let args' = e'::revfilledargs' in
          L.build_call matrix_extract_f (Array.of_list args') "matrix_extract" builder
      | SelectAssign (v, args, e) ->
        let partialargs' = List.map (build_expr builder) args in
        let filledargs' = fill_select_args partialargs' in
        let revfilledargs' = List.rev filledargs' in
        let e' = build_expr builder e in
        let v' = L.build_load (lookup v) v builder in
        let args' = v'::e'::revfilledargs' in
          L.build_call matrix_insert_f (Array.of_list args') "matrix_insert" builder
    in
    let rec build_stmt builder = function
      | Block sl -> List.fold_left build_stmt builder sl
      | Semiring ring -> ignore (L.build_call change_ring_f [| L.const_int i32_t (List.assoc ring Definitions.rings) |] "ring_change" builder); builder
      | Expr e ->
        ignore (build_expr builder e);
        builder
      | Return e ->
        ignore (L.build_ret (build_expr builder e) builder);
        builder
      | If (pred, thn, els) ->
        let bool_val_uncast = L.build_call matrix_bool_f [| (build_expr builder pred) |] "matrix_bool" builder in
        let bool_val = L.build_icmp L.Icmp.Eq bool_val_uncast (L.const_int i32_t 1) "i1_t" builder in
        let merge_bb = L.append_block context "merge" func in
        let build_br_merge = L.build_br merge_bb in
        let then_bb = L.append_block context "then" func in
          add_terminal (build_stmt (L.builder_at_end context then_bb) thn) build_br_merge;
        let else_bb = L.append_block context "else" func in
          add_terminal (build_stmt (L.builder_at_end context else_bb) els) build_br_merge;
        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb
      | While (pred, body) ->
        let pred_bb = L.append_block context "while" func in
        let body_bb = L.append_block context "while_body" func in
        let pred_builder = L.builder_at_end context pred_bb in
          let bool_val_uncast = L.build_call matrix_bool_f [| (build_expr pred_builder pred) |] "matrix_bool" builder in
            let bool_val = L.build_icmp L.Icmp.Eq bool_val_uncast (L.const_int i32_t 1) "i1_t" builder in
            let merge_bb = L.append_block context "merge" func in
            let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
            add_terminal (build_stmt (L.builder_at_end context body_bb) body) (L.build_br pred_bb);
            L.builder_at_end context merge_bb
      (*| For (e1, e2, e3, body) ->
        build_stmt builder ( Block [Expr e1 ; While (e2, Block [body ; Expr e3])] ) *)
    in
    let builder = build_stmt builder (Block fdecl.body) in
    add_terminal builder (L.build_ret (L.const_int (if is_main then i32_t else matrix_t) 0))
  in
  build_function_body main_fdecl true;
  List.iter2  build_function_body  functions  (List.map (fun (_) -> false) functions);
  blastoff_module
;;
