module L = Llvm

let context = L.global_context ()
let llmem = L.MemoryBuffer.of_file "graphblas.bc"
let llm = Llvm_bitreader.parse_bitcode context llmem
let blastoff_module = L.create_module context "BLAStoff"

let rings  = [("_", 0) ; ("arithmetic", 1) ; ("logical", 2); ("maxmin", 3)]
let functions = [("I", ["n"]); ("Zero", ["d"]); ("range", ["n"]); ("print", ["e"]); ("toString", ["e"])]

let i32_t = L.i32_type context
let matrix_t =
  L.pointer_type
    (match L.type_by_name llm "struct.matrix" with
    | None-> raise (Failure "matrix type implementation not found")
    | Some t -> t)

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

let matrix_bool_t = L.function_type i32_t [| matrix_t |]
let matrix_bool_f = L.declare_function "matrix_bool" matrix_bool_t blastoff_module
