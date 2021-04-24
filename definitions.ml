module L = Llvm

let context = L.global_context ()
let llmem = L.MemoryBuffer.of_file "graphblas.bc"
let llm = Llvm_bitreader.parse_bitcode context llmem
let blastoff_module = L.create_module context "BLAStoff"
let rings = [ "_", 0; "arithmetic", 1; "logical", 2; "maxmin", 3 ]

let functions =
  [ "I", [ "n" ]
  ; "Zero", [ "d" ]
  ; "range", [ "n" ]
  ; "print", [ "e" ]
  ; "toString", [ "e" ]
  ]
;;

type built_in =
  { name : string
  ; ret : L.lltype
  ; args : L.lltype list
  }

let i32_t = L.i32_type context
let float_t = L.double_type context

let matrix_t =
  L.pointer_type
    (match L.type_by_name llm "struct.matrix" with
    | None -> raise (Failure "matrix type implementation not found")
    | Some t -> t)
;;

let built_in_defs : built_in list =
  [ { name = "matrix_create"; ret = matrix_t; args = [ matrix_t ] }
  ; { name = "matrix_create_identity"; ret = matrix_t; args = [ matrix_t ] }
  ; { name = "matrix_create_zero"; ret = matrix_t; args = [ matrix_t ] }
  ; { name = "matrix_create_range"; ret = matrix_t; args = [ matrix_t ] }
  ; { name = "matrix_print"; ret = matrix_t; args = [ matrix_t ] }
  ; { name = "matrix_tostring"; ret = matrix_t; args = [ matrix_t ] }
  ; { name = "change_ring"; ret = i32_t; args = [ i32_t ] }
  ; { name = "matrix_setelem"; ret = i32_t; args = [ matrix_t; i32_t; i32_t; i32_t ] }
  ; { name = "matrix_mul"; ret = matrix_t; args = [ matrix_t; matrix_t ] }
  ; { name = "matrix_conv"; ret = matrix_t; args = [ matrix_t; matrix_t ] }
  ; { name = "matrix_elmul"; ret = matrix_t; args = [ matrix_t; matrix_t ] }
  ; { name = "matrix_eladd"; ret = matrix_t; args = [ matrix_t; matrix_t ] }
  ; { name = "matrix_extract"
    ; ret = matrix_t
    ; args = [ matrix_t; matrix_t; matrix_t; matrix_t; matrix_t ]
    }
  ; { name = "matrix_insert"
    ; ret = matrix_t
    ; args = [ matrix_t; matrix_t; matrix_t; matrix_t; matrix_t; matrix_t ]
    }
  ;{name = "matrix_eq"; ret = matrix_t; args = [ matrix_t; matrix_t ]}
  ;{name = "matrix_neq"; ret = matrix_t; args = [ matrix_t; matrix_t ]}
  ;{name = "matrix_leq"; ret = matrix_t; args = [ matrix_t; matrix_t ]}
  ;{name = "matrix_less"; ret = matrix_t; args = [ matrix_t; matrix_t ]}
  ;{name = "matrix_geq"; ret = matrix_t; args = [ matrix_t; matrix_t ]}
  ;{name = "matrix_greater"; ret = matrix_t; args = [ matrix_t; matrix_t ]}
  ;{name = "matrix_concat"; ret = matrix_t; args = [ matrix_t; matrix_t ]}
  ;{ name = "matrix_bool"; ret = i32_t; args = [ matrix_t ] }
  ;{ name = "matrix_negate"; ret = matrix_t; args = [ matrix_t ] }
  ;{ name = "matrix_reduce"; ret = matrix_t; args = [ matrix_t ; i32_t] }
  ;{ name = "matrix_insert"; ret = matrix_t; args = [ matrix_t; matrix_t; matrix_t; matrix_t; matrix_t; matrix_t] }
  ;{ name = "matrix_reduce"; ret = matrix_t; args = [ matrix_t ; i32_t] }
  ;{ name = "matrix_size"; ret = matrix_t; args = [ matrix_t ] }
  ;{ name = "matrix_transpose"; ret = matrix_t; args = [ matrix_t ] }
  ;{ name = "matrix_truthy"; ret = matrix_t; args = [ matrix_t ] }
  ]
;;

let matrix_truthy_t = L.function_type i32_t [| matrix_t |]
let matrix_truthy_f = L.declare_function "matrix_truthy" matrix_truthy_t blastoff_module


let matrix_exp_t = L.function_type matrix_t [| matrix_t; matrix_t |]
let matrix_exp_f = L.declare_function "matrix_exp" matrix_exp_t blastoff_module

let create_fun_type fdef = L.function_type fdef.ret (Array.of_list fdef.args)
let declare_fun fname ftype = L.declare_function fname ftype blastoff_module
let built_ins = List.map (fun fdef -> fdef.name, declare_fun fdef.name (create_fun_type fdef)) built_in_defs
let build_call fname args builder = L.build_call (List.assoc fname built_ins) args fname builder

let matrix_create_t = L.function_type matrix_t [| i32_t; i32_t |]
let matrix_create_f = L.declare_function "matrix_create" matrix_create_t blastoff_module
let matrix_identity_t = L.function_type matrix_t [| matrix_t |]
let matrix_identity_f =
  L.declare_function "matrix_create_identity" matrix_identity_t blastoff_module
let ring_push_t = L.function_type i32_t [||]
let ring_push_f = L.declare_function "ring_push" ring_push_t blastoff_module
let ring_pop_t = L.function_type i32_t [||]
let ring_pop_f = L.declare_function "ring_pop" ring_pop_t blastoff_module
let ring_change_t = L.function_type i32_t [| i32_t |]
let ring_change_f = L.declare_function "ring_change" ring_change_t blastoff_module
let matrix_setelem_t = L.function_type i32_t [| matrix_t; i32_t; i32_t; i32_t |]
let matrix_setelem_f =
  L.declare_function "matrix_setelem" matrix_setelem_t blastoff_module
let matrix_extract_t =
  L.function_type matrix_t [| matrix_t; matrix_t; matrix_t; matrix_t; matrix_t |]
let matrix_extract_f =
  L.declare_function "matrix_extract" matrix_extract_t blastoff_module
