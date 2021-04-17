module L = Llvm
module A = Ast
open Ast
module StringMap = Map.Make (String)

let translate (functions, statements) =
  let context = L.global_context () in
  let llmem = L.MemoryBuffer.of_file "graphblas.bc" in
  let llm = Llvm_bitreader.parse_bitcode context llmem in
  let blastoff_module = L.create_module context "BLAStoff" in

  let i32_t = L.i32_type context
  and matrix_t =
    L.pointer_type
      (match L.type_by_name llm "struct.matrix" with
      | None -> raise (Failure "matrix type implementation not found")
      | Some t -> t)
  in

  let matrix_create_t = L.function_type matrix_t [| i32_t; i32_t |] in
  let matrix_create_f = L.declare_function "matrix_create" matrix_create_t blastoff_module in
  let matrix_print_t = L.function_type i32_t [| matrix_t |] in
  let change_ring_t = L.function_type i32_t [| i32_t |] in
  let change_ring_f = L.declare_function "change_ring" change_ring_t blastoff_module in
  let matrix_print_f = L.declare_function "matrix_print" matrix_print_t blastoff_module in
  let matrix_setelem_t = L.function_type i32_t [| matrix_t; i32_t; i32_t; i32_t |] in
  let matrix_setelem_f = L.declare_function "matrix_setelem" matrix_setelem_t blastoff_module in
  let matrix_mul_t = L.function_type matrix_t [| matrix_t; matrix_t |] in
  let matrix_mul_f = L.declare_function "matrix_mul" matrix_mul_t blastoff_module in

  let main_fdecl = { fname = "main"; formals = []; body = statements } in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
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

  (* Fill in the body of the given function *)
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
      let add_assignment lst = function Expr e -> (match e with Assign (v, _) -> (match v with Id id -> id :: lst | _ -> lst )
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
    let rec build_expr builder e = match e with
      | IntMatLit m -> build_int_matrix m
      | GraphLit _ -> raise (Failure "Graph Literal")
      | FloatMatLit _ -> raise (Failure "Float Matrix Literal")
      | Assign (v , e) -> let comp_e = build_expr builder e in
        (match v with Id s-> ignore(L.build_store comp_e (lookup s) builder)); comp_e 
      | Call(fname, exprs) ->
        (match fname with 
          "print" -> (match exprs with 
              [e] -> L.build_call matrix_print_f [| (build_expr builder e) |] "matrix_print" builder 
             | _ -> raise (Failure "Invalid list of expressions passed to print"))
         | f -> let (fdef, fdecl) = (try StringMap.find f function_decls with Not_found -> raise (Failure ("Undeclared function, " ^ f ^ ", found in code generation"))) in
           let args = List.map (build_expr builder) (List.rev exprs) in
           L.build_call fdef (Array.of_list args) (fdecl.fname ^ "_result") builder)
      | Binop (e1, op, e2) ->
          let e1' = build_expr builder e1
          and e2' = build_expr builder e2 in ( 
            match op with
            A.Matmul -> L.build_call matrix_mul_f [| e1'; e2'|] "matrix_mul" builder
          )
      | UnkMatLit _ -> raise (Failure "Type of matrix is unknown")
      | Literal _ -> raise (Failure "Naked literal")
      | Noexpr -> raise (Failure "No expression in codegen")
      | Unop (op, e1) -> let _ = build_expr builder e1 in (match op with A.Size -> raise (Failure "Unop call made"))
      | Id v -> L.build_load (lookup v) v builder
      | Selection _ -> raise (Failure "Selection not implemented")
    in
    let rec build_stmt builder = function
      | Block sl -> List.fold_left build_stmt builder sl
      | Semiring ring -> ignore (L.build_call change_ring_f [| L.const_int i32_t (List.assoc ring Constants.rings) |] "ring_change" builder); builder
      | Expr e ->
        ignore (build_expr builder e);
        builder
      | Return e ->
        ignore (L.build_ret (build_expr builder e) builder);
        builder
      | If (pred, thn, els) ->
        let bool_val = build_expr builder pred in
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
          ignore(L.build_br pred_bb builder);
        let body_bb = L.append_block context "while_body" func in
          add_terminal(build_stmt (L.builder_at_end context body_bb) body) (L.build_br pred_bb);
        let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = build_expr pred_builder pred in
            let merge_bb = L.append_block context "merge" func in
            ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
            L.builder_at_end context merge_bb
    in
    let builder = build_stmt builder (Block fdecl.body) in
    add_terminal builder (L.build_ret (L.const_int (if is_main then i32_t else matrix_t) 0))
  in
  build_function_body main_fdecl true;
  List.iter2  build_function_body  functions  (List.map (fun (_) -> false) functions);
  blastoff_module
;;
