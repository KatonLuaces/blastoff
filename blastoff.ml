(* Top-level of the BLAStoff compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast | Sast | LLVM_IR | Compile

let () =
  try
    let action = ref Compile in
    let set_action a () = action := a in
    let speclist = [
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-s", Arg.Unit (set_action Sast), "Print the SAST");
      ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
      ("-c", Arg.Unit (set_action Compile),
        "Check and print the generated LLVM IR (default)");
    ] in
    let usage_msg = "usage: ./blastoff.native [-a|-s|-l|-c] [file.blst]" in
    let channel = ref stdin in
    Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

    let lexbuf = Lexing.from_channel !channel in
    let ast = Blastoffparser.program Scanner.token lexbuf in
      match !action with
      | Ast     -> print_string (Ast.string_of_program ast)
      | Sast    -> ()
      | LLVM_IR -> ()
      | Compile -> ()
  with
  (*| Not_found -> Printf.printf "NotFoundError: unknown error\n"; from_console map past run*)
  | Parsing.Parse_error -> print_endline "SyntaxError: invalid syntax";
  | Failure explanation -> print_endline explanation;
  (*| Runtime explanation -> Printf.printf "%s\n" explanation; flush stdout; from_console map past run*)
