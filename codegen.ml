module L = Llvm
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
  let matrix_print_f = L.declare_function "matrix_print" matrix_print_t blastoff_module in
  let matrix_setelem_t = L.function_type i32_t [| matrix_t; i32_t; i32_t; i32_t |] in
  let matrix_setelem_f = L.declare_function "matrix_setelem" matrix_setelem_t blastoff_module in

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
    let func, _ = StringMap.find fdecl.fname function_decls in
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
      List.fold_left add_local formals []
      (*TODO: decide in what form locals are necessary*)
    in
    let lookup n = StringMap.find n local_vars in
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in
    let rec build_expr builder e = match e with
      | Matlit m -> let mat = L.build_call matrix_create_f [| 
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
      | Call("print", [e]) ->
          L.build_call matrix_print_f [| (build_expr builder e) |] "matrix_print" builder
      (* | Binop (e1, op, e2) -> ()
      | Unop (e1, op) -> ()
      | Assign (v, e1) -> ()
      | Call (f, exprs) -> () *)
    in
    let rec build_stmt builder = function
      | Block sl -> List.fold_left build_stmt builder sl
      | Expr e ->
        ignore (build_expr builder e);
        builder
      | Return e ->
        ignore (L.build_ret (build_expr builder e) builder);
        builder
      (*
      | If (pred, thn, els) ->
        ();
        builder
      | For (e1, e2, e3, body) ->
        ();
        builder
      | While (pred, body) ->
        ();
        builder
        *)
    in
    let builder = build_stmt builder (Block fdecl.body) in
    add_terminal builder (L.build_ret (L.const_int (if is_main then i32_t else matrix_t) 0))
  in
  build_function_body main_fdecl true;
  List.iter2  build_function_body  functions  (List.map (fun (_) -> false) functions);
  blastoff_module
;;
