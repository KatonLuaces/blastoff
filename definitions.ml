module L = Llvm

let context = L.global_context ()
let llmem = L.MemoryBuffer.of_file "graphblas.bc"
let llm = Llvm_bitreader.parse_bitcode context llmem
let blastoff_module = L.create_module context "BLAStoff"

let rings  = [("_", 0) ; ("arithmetic", 1) ; ("logical", 2); ("maxmin", 3)]
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

let matrix_create_t = L.function_type matrix_t [| i32_t; i32_t |]
let matrix_create_f = L.declare_function "matrix_create" matrix_create_t blastoff_module

let matrix_identity_t = L.function_type matrix_t [| matrix_t |]
let matrix_identity_f = L.declare_function "matrix_create_identity" matrix_identity_t blastoff_module

let matrix_zero_t = L.function_type matrix_t [| matrix_t |]
let matrix_zero_f = L.declare_function "matrix_create_zero" matrix_zero_t blastoff_module

let matrix_range_t = L.function_type matrix_t [| matrix_t |]
let matrix_range_f = L.declare_function "matrix_create_range" matrix_range_t blastoff_module

let matrix_print_t = L.function_type i32_t [| matrix_t |]
let matrix_print_f = L.declare_function "matrix_print" matrix_print_t blastoff_module

let matrix_tostring_t = L.function_type matrix_t [| matrix_t |]
let matrix_tostring_f = L.declare_function "matrix_tostring" matrix_tostring_t blastoff_module

let ring_push_t = L.function_type i32_t [| |]
let ring_push_f = L.declare_function "ring_push" ring_push_t blastoff_module

let ring_pop_t = L.function_type i32_t [| |]
let ring_pop_f = L.declare_function "ring_pop" ring_pop_t blastoff_module

let ring_change_t = L.function_type i32_t [| i32_t |]
let ring_change_f = L.declare_function "ring_change" ring_change_t blastoff_module

let matrix_setelem_t = L.function_type i32_t [| matrix_t; i32_t; i32_t; i32_t |]
let matrix_setelem_f = L.declare_function "matrix_setelem" matrix_setelem_t blastoff_module

let matrix_mul_t = L.function_type matrix_t [| matrix_t; matrix_t |]
let matrix_mul_f = L.declare_function "matrix_mul" matrix_mul_t blastoff_module

let matrix_conv_t = L.function_type matrix_t [| matrix_t; matrix_t |]
let matrix_conv_f = L.declare_function "matrix_conv" matrix_conv_t blastoff_module

let matrix_elmul_t = L.function_type matrix_t [| matrix_t; matrix_t |]
let matrix_elmul_f = L.declare_function "matrix_elmul" matrix_elmul_t blastoff_module

let matrix_eladd_t = L.function_type matrix_t [| matrix_t; matrix_t |]
let matrix_eladd_f = L.declare_function "matrix_eladd" matrix_eladd_t blastoff_module

let matrix_extract_t = L.function_type matrix_t [| matrix_t; matrix_t; matrix_t; matrix_t; matrix_t |]
let matrix_extract_f = L.declare_function "matrix_extract" matrix_extract_t blastoff_module

let matrix_insert_t = L.function_type matrix_t [| matrix_t; matrix_t; matrix_t; matrix_t; matrix_t; matrix_t |]
let matrix_insert_f = L.declare_function "matrix_insert" matrix_insert_t blastoff_module

let matrix_size_t = L.function_type matrix_t [| matrix_t |]
let matrix_size_f = L.declare_function "matrix_size" matrix_size_t blastoff_module

let matrix_transpose_t = L.function_type matrix_t [| matrix_t |]
let matrix_transpose_f = L.declare_function "matrix_transpose" matrix_transpose_t blastoff_module

let matrix_negate_t = L.function_type matrix_t [| matrix_t |]
let matrix_negate_f = L.declare_function "matrix_negate" matrix_negate_t blastoff_module

let matrix_reduce_t = L.function_type matrix_t [| matrix_t ; i32_t|]
let matrix_reduce_f = L.declare_function "matrix_reduce" matrix_reduce_t blastoff_module

let matrix_concat_t = L.function_type matrix_t [| matrix_t; matrix_t |]
let matrix_concat_f = L.declare_function "matrix_concat" matrix_concat_t blastoff_module

(* Comparison operators *)

let matrix_truthy_t = L.function_type i32_t [| matrix_t |]
let matrix_truthy_f = L.declare_function "matrix_truthy" matrix_truthy_t blastoff_module

(* Comparison operators return a matrix. matrix_truthy returns an i32 so we can
evaluate the bool value in the If statement. *)
let matrix_comp_t = L.function_type matrix_t [| matrix_t; matrix_t |]
let matrix_equal_f = L.declare_function "matrix_equal" matrix_comp_t blastoff_module
let matrix_neq_f = L.declare_function "matrix_neq" matrix_comp_t blastoff_module
let matrix_leq_f = L.declare_function "matrix_leq" matrix_comp_t blastoff_module
let matrix_less_f = L.declare_function "matrix_less" matrix_comp_t blastoff_module
let matrix_geq_f = L.declare_function "matrix_geq" matrix_comp_t blastoff_module
let matrix_greater_f = L.declare_function "matrix_greater" matrix_comp_t blastoff_module
