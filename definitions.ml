module L = Llvm

let context = L.global_context () 
let llmem = L.MemoryBuffer.of_file "graphblas.bc"
let llm = Llvm_bitreader.parse_bitcode context llmem
let blastoff_module = L.create_module context "BLAStoff"

let rings  = [("arith", 0) ; ("logic", 1); ("maxmin", 2)]
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

let change_ring_t = L.function_type i32_t [| i32_t |]
let change_ring_f = L.declare_function "change_ring" change_ring_t blastoff_module

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

