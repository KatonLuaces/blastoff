module A = Ast
open Ast
open Definitions
module StringMap = Map.Make (String)

let translate (functions, statements) =
  let main_fdecl = { fname = "main"; formals = []; body = List.rev statements } in
  let function_decls : (L.llvalue * func_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.fname
      and formal_types = Array.of_list (List.map (fun _ -> matrix_t) fdecl.formals) in
      let ftype = L.function_type matrix_t formal_types in
      StringMap.add name (L.define_function name ftype blastoff_module, fdecl) m
    in
    let decls = List.fold_left function_decl StringMap.empty functions in
    StringMap.add
      main_fdecl.fname
      ( L.define_function
          main_fdecl.fname
          (L.function_type i32_t (Array.of_list []))
          blastoff_module
      , main_fdecl )
      decls
  in
  let build_function_body fdecl is_main =
    let func, _ =
      try StringMap.find fdecl.fname function_decls with
      | Not_found -> raise (Failure ("Unknown function, " ^ fdecl.fname))
    in
    let builder = L.builder_at_end context (L.entry_block func) in
    let local_vars =
      let add_formal m n p =
        L.set_value_name n p;
        let local = L.build_alloca matrix_t n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m
      in
      let add_local m n =
        if StringMap.mem n m
        then m
        else (
          let local_var = L.build_alloca matrix_t n builder in
          StringMap.add n local_var m)
      in
      let formals =
        List.fold_left2
          add_formal
          StringMap.empty
          fdecl.formals
          (Array.to_list (L.params func))
      in
      let rec add_assignment lst = function
        | Expr e ->
          (match e with
          | IdAssign (id, _) -> id :: lst
          | _ -> lst)
        | Block stmts -> List.fold_left add_assignment lst stmts
        | If (_, s1, s2) -> add_assignment (add_assignment lst s1) s2
        | While (_, s) -> add_assignment lst s
        | _ -> lst
      in
      let locals = List.fold_left add_assignment [] fdecl.body in
      List.fold_left add_local formals locals
    in
    let lookup n =
      try StringMap.find n local_vars with
      | Not_found -> raise (Failure ("Undeclared variable " ^ n))
    in
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in
    let build_graph_matrix jasons_builder m =
      let max3 a b c =
        if a >= b && a >= c then a else if b >= c && b >= a then b else c
      in
      let dim = 1 + List.fold_left (fun acc elem -> max3 acc (fst elem) (snd elem)) 0 m in
      let mat =
        L.build_call
          matrix_create_f
          [| L.const_int i32_t dim; L.const_int i32_t dim |]
          "matrix_create"
          jasons_builder
      in
      List.iter
        (fun elem ->
          ignore
            (L.build_call
               matrix_setelem_f
               [| mat
                ; L.const_int i32_t 1
                ; L.const_int i32_t (fst elem)
                ; L.const_int i32_t (snd elem)
               |]
               "matrix_setelem"
               jasons_builder))
        m;
      mat
    in
    let build_matrix typ jasons_builder m =
      let mat =
        L.build_call
          matrix_create_f
          [| L.const_int i32_t (List.length m)
           ; L.const_int i32_t (List.length (List.hd m))
          |]
          "matrix_create"
          jasons_builder
      in
      List.iteri
        (fun i row ->
          (List.iteri (fun j elem ->
               ignore
                 (L.build_call
                    matrix_setelem_f
                    [| mat
                     ; typ elem
                     ; L.const_int i32_t i
                     ; L.const_int i32_t j
                    |]
                    "matrix_setelem"
                    jasons_builder)))
            (List.rev row))
        (List.rev m);
      mat
    in
    let rec fill_select_args builder args =
      let zero =
        L.build_call
          matrix_create_f
          [| L.const_int i32_t 1; L.const_int i32_t 1 |]
          "matrix_create"
          builder
      in
      let base =
        L.build_call
          matrix_create_f
          [| L.const_int i32_t 1; L.const_int i32_t 1 |]
          "matrix_create"
          builder
      in
      let one =
        ignore
          (L.build_call
             matrix_setelem_f
             [| base; L.const_int i32_t 1; L.const_int i32_t 0; L.const_int i32_t 0 |]
             "matrix_setelem"
             builder);
        base
      in
      match args with
      | [ _; _; _; _ ] as l -> l
      | [ _; _; _ ] as l -> fill_select_args builder (one :: l)
      | [ _; _ ] as l -> fill_select_args builder (one :: l)
      | [ _ ] as l -> fill_select_args builder (zero :: l)
      | _ -> raise (Failure "Too many/few arguments to selection")
    in
    let rec build_expr builder e =
      match e with
      | IntMatLit m -> build_matrix (fun el -> L.const_int i32_t el) builder m
      | GraphLit m -> build_graph_matrix builder m
      | FloatMatLit m -> build_matrix (fun el -> L.const_float float_t el) builder m
      | IdAssign (v, e) ->
        let comp_e = build_expr builder e in
        (match v with
        | s -> ignore (L.build_store comp_e (lookup s) builder));
        comp_e
      | Call (fname, exprs) ->
        (match fname with
        | "print" ->
          (match exprs with
          | [ e ] ->
              build_call "matrix_print" [| build_expr builder e |] builder
          | _ -> raise (Failure "Invalid list of expressions passed to print"))
        | "toString" ->
          (match exprs with
          | [ e ] ->
            build_call
              "matrix_tostring"
              [| build_expr builder e |]
              builder
          | _ -> raise (Failure "Invalid list of expressions passed to toString"))
        | "I" ->
          (match exprs with
          | [ e ] ->
            build_call
              "matrix_create_identity"
              [| build_expr builder e |]
              builder
          | _ -> raise (Failure "Invalid list of expressions passed to I"))
        | "Zero" ->
          (match exprs with
          | [ e ] ->
            build_call "matrix_create_zero" [| build_expr builder e |] builder
          | _ -> raise (Failure "Invalid list of expressions passed to Zero"))
        | "range" ->
          (match exprs with
          | [ e ] ->
            build_call "matrix_create_range" [| build_expr builder e |] builder
          | _ -> raise (Failure "Invalid list of expressions passed to range"))
        | "__ring_push" ->
          (match exprs with
          | [] -> L.build_call ring_push_f [||] "__ring_push" builder
          | _ -> raise (Failure "Invalid list of expressions passed to __ring_push"))
        | "__ring_pop" ->
          (match exprs with
          | [] -> L.build_call ring_pop_f [||] "__ring_pop" builder
          | _ -> raise (Failure "Invalid list of expressions passed to __ring_pop"))
        | f ->
          let fdef, fdecl =
            try StringMap.find f function_decls with
            | Not_found ->
              raise (Failure ("Undeclared function, " ^ f ^ ", found in code generation"))
          in
          let args = List.map (build_expr builder) (List.rev exprs) in
          L.build_call fdef (Array.of_list args) (fdecl.fname ^ "_result") builder)
      | Binop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
        | A.Matmul -> build_call "matrix_mul" [| e1'; e2' |] builder
        | A.Exponent -> L.build_call matrix_exp_f [| e1'; e2' |] "matrix_mul" builder
        | A.Conv -> build_call "matrix_conv" [| e1'; e2' |] builder
        | A.Elmul -> build_call "matrix_elmul" [| e1'; e2' |] builder
        | A.Add -> build_call "matrix_eladd" [| e1'; e2' |]  builder
        | A.Concat -> build_call "matrix_concat" [| e1'; e2' |] builder
        | A.Equal -> build_call "matrix_eq" [| e1'; e2' |] builder
        | A.Neq -> build_call "matrix_neq" [| e1'; e2' |] builder
        | A.Leq -> build_call "matrix_leq" [| e1'; e2' |] builder
        | A.Less -> build_call "matrix_less" [| e1'; e2' |] builder
        | A.Geq -> build_call "matrix_geq" [| e1'; e2' |] builder
        | A.Greater ->
          build_call "matrix_greater" [| e1'; e2' |] builder)
      | UnkMatLit _ -> raise (Failure "Type of matrix is unknown")
      | Assign _ -> raise (Failure "Assign in codegen")
      | StringLit _ -> raise (Failure "StringLit in codegen")
      | Unop (op, e) ->
        let e' = build_expr builder e in
        (match op with
        | A.Size -> build_call "matrix_size"  [| e' |] builder
        | A.Transp -> build_call "matrix_transpose" [| e' |] builder
        | A.Plusreduce ->
          build_call
            "matrix_reduce"
            [| e'; L.const_int i32_t 0 |]
            builder
        | A.Mulreduce ->
          build_call
            "matrix_reduce"
            [| e'; L.const_int i32_t 1 |]
            builder
        | A.Neg -> build_call "matrix_negate" [| e' |] builder)
      | Id v -> L.build_load (lookup v) v builder
      | Selection (e, args) ->
        let partialargs' = List.map (build_expr builder) args in
        let filledargs' = fill_select_args builder partialargs' in
        let revfilledargs' = List.rev filledargs' in
        let e' = build_expr builder e in
        let args' = e' :: revfilledargs' in
        L.build_call matrix_extract_f (Array.of_list args') "matrix_extract" builder
      | SelectAssign (v, args, e) ->
        let partialargs' = List.map (build_expr builder) args in
        let filledargs' = fill_select_args builder partialargs' in
        let revfilledargs' = List.rev filledargs' in
        let e' = build_expr builder e in
        let v' = L.build_load (lookup v) v builder in
        let args' = v' :: e' :: revfilledargs' in
        build_call "matrix_insert" (Array.of_list args') builder
    in
    let rec build_stmt builder = function
      | Block sl -> List.fold_left build_stmt builder sl
      | Semiring ring ->
        ignore
          (L.build_call
             ring_change_f
             [| L.const_int i32_t (List.assoc ring Definitions.rings) |]
             "ring_change"
             builder);
        builder
      | Expr e ->
        ignore (build_expr builder e);
        builder
      | Return e ->
        ignore (build_expr builder (Call ("__ring_pop", [])));
        ignore (L.build_ret (build_expr builder e) builder);
        builder
      | If (pred, thn, els) ->
        let pred_expr = build_expr builder pred in
        let mat_truthiness =
          L.build_call matrix_truthy_f [| pred_expr |] "matrix_truthy" builder
        in
        let bool_val =
          L.build_icmp L.Icmp.Eq mat_truthiness (L.const_int i32_t 1) "i1_t" builder
        in
        let merge_bb = L.append_block context "merge_if" func in
        let build_br_merge = L.build_br merge_bb in
        let then_bb = L.append_block context "then" func in
        add_terminal (build_stmt (L.builder_at_end context then_bb) thn) build_br_merge;
        let else_bb = L.append_block context "else" func in
        add_terminal (build_stmt (L.builder_at_end context else_bb) els) build_br_merge;
        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb
      | While (pred, body) ->
        let pred_bb = L.append_block context "while" func in
        let pred_builder = L.builder_at_end context pred_bb in
        let pred_expr = build_expr pred_builder pred in
        let mat_truthiness =
          L.build_call matrix_truthy_f [| pred_expr |] "matrix_truthy" pred_builder
        in
        let bool_val =
          L.build_icmp L.Icmp.Eq mat_truthiness (L.const_int i32_t 1) "i1_t" pred_builder
        in
        ignore (L.build_br pred_bb builder) (* builds branch to while from entry point *);
        let body_bb = L.append_block context "while_body" func in
        let body_builder = build_stmt (L.builder_at_end context body_bb) body in
        add_terminal body_builder (L.build_br pred_bb);
        let merge_bb = L.append_block context "merge" func in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb
    in
    let body = Expr (Call ("__ring_push", [])) :: fdecl.body in
    let builder = build_stmt builder (Block body) in
    add_terminal
      builder
      (L.build_ret (L.const_int (if is_main then i32_t else matrix_t) 0))
  in
  build_function_body main_fdecl true;
  List.iter2 build_function_body functions (List.map (fun _ -> false) functions);
  blastoff_module
;;
