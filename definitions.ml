module L = Llvm

let context = L.global_context ()
let llmem = L.MemoryBuffer.of_file "graphblas.bc"
let llm = Llvm_bitreader.parse_bitcode context llmem
let blastoff_module = L.create_module context "BLAStoff"

let rings  = [("arith", 0) ; ("logic", 1); ("maxmin", 2)]
let functions = [("I", ["n"]); ("Zero", ["d"]); ("range", ["n"]); ("print", ["e"]); ("toString", ["e"])]

type built_in = {name: string; ret: L.lltype; args: L.lltype list}

let i32_t = L.i32_type context
let matrix_t =
  L.pointer_type
    (match L.type_by_name llm "struct.matrix" with
    | None-> raise (Failure "matrix type implementation not found")
    | Some t -> t)

let built_in_defs: built_in list = [
  {name="matrix_create"; ret=matrix_t; args=[matrix_t]};
  {name="matrix_create_identity"; ret=matrix_t; args=[matrix_t]};
  {name="matrix_create_zero"; ret=matrix_t; args=[matrix_t]};
  {name="matrix_create_range"; ret=matrix_t; args=[matrix_t]};
  {name="matrix_print"; ret=i32_t; args=[matrix_t]};
  {name="matrix_tostring"; ret=matrix_t; args=[matrix_t]};
  {name="change_ring"; ret=i32_t; args=[i32_t]};
  {name="matrix_setelem"; ret=i32_t; args=[matrix_t; i32_t; i32_t; i32_t]};
  {name="matrix_mul"; ret=matrix_t; args=[matrix_t; matrix_t]};
  {name="matrix_conv"; ret=matrix_t; args=[matrix_t; matrix_t]};
  {name="matrix_elmul"; ret=matrix_t; args=[matrix_t; matrix_t]};
  {name="matrix_eladd"; ret=matrix_t; args=[matrix_t; matrix_t]};
  {name="matrix_extract"; ret=matrix_t; args=[matrix_t; matrix_t; matrix_t; matrix_t; matrix_t;]};
  {name="matrix_insert"; ret=matrix_t; args=[matrix_t; matrix_t; matrix_t; matrix_t; matrix_t; matrix_t;]};
  {name="matrix_bool"; ret=i32_t; args=[matrix_t]}
]

let create_fun_type fdef = L.function_type fdef.ret (Array.of_list fdef.args)
let built_ins = List.map (fun fdef -> (fdef.name, create_fun_type fdef)) built_in_defs 
