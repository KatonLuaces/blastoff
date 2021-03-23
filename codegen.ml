module L = Llvm
open Ast
module StringMap = Map.Make (String)

let translate (functions, statements) =
  let context = L.global_context () in
  let llmem = L.MemoryBuffer.of_file "graphblas.bc" in
  let llm = Llvm_bitreader.parse_bitcode context llmem in
  let blastoff_module = L.create_module context "BLAStoff" in
  let matrix_t =
    L.pointer_type
      (match L.type_by_name llm "struct.matrix" with
      | None -> raise (Failure "matrix type implementation not found")
      | Some t -> t)
  in
  let matmul = L.declare_function "matmul" matrix_t blastoff_module in
  (* 

  let function_decls : (L.llvalue * func_decl) StringMap.t =
    let function_decl m fdecl = 
      let name = fdecl.fname in
      let ftype = matrix_t in
      StringMap.add name (L.define_function name ftype blastoff_module, fdecl) m in
    List.fold_left function_decl StringMap.empy functions in
  let build_function_body fdecl = 
    let (func, _) = StringMap.find fdecl.name function_decls in
    let builder = L.builder_at_end context (L.entry_block func) in

    let local_vars = 
      let add_formal m n p = 
        L.set_value_name n p;
        let local = L.build_alloca matrix_t n builder in
        ignore (L.build_store p local builder);
        Stringmap.add n local m
      in
      let add_local m n =
        let local_var = L.build_alloca matrix_t n builder
        in StringMap.add local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty 
          fdecl.formals (Array.to_list (L.params func)) 
      in
      List.fold_left add_local formals fdecl fdecl.locals
    in
    let lookup n = StringMap.find n local_vars in
    let rec build_expr builder = function
        Matlit  m ->  L.build_call matrix_from_matrix_lit [| m |]
      | Binop (e1, op, e2) -> ()
      | Unop (e1, op) -> ()
      | Assign (v , e1) -> ()
      | Call (f, exprs) -> ()
    in

    let rec build_stmt builder = function
        Block sl -> List.fold_left stmt builder
      | Expr e -> ignore(build_expr builder e); builder
      | Return e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | If (pred, thn, els) -> (); builder
      | For  (e1, e2, e3, body) -> (); builder
      | While (pred, body) -> (); builder
    in
    let builder = stmt builder (Block fdecl.body) in
    let add_terminal builder instr = 
      match L.block_terminator (L.insertion_block builder) 
      with Some _ -> ()
         | None -> ignore (instr builder) 
    in
    List.iter build_function_body functions;
    *)
  blastoff_module
;;
