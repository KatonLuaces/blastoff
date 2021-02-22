open scanner

let scanner_test input_file =
  let lexbuf = Lexing.from_channel input_file in
  let rec loop =
     let the_token = token lexbuf in
        print_token token;
        flush stdout;
        match the_token with
           | (*WHATEVER WE DECIDE EOF TOKEN TO BE*) -> ()
           | _ -> loop
  in loop

let _ =
  if Array.length Sys.argv <> 2 then
    Printf.fprintf stderr "Usage: %s input_filename\n" Sys.argv.(0)
  else
    let input_file = open_in Sys.argv.(1) in
    scanner_test input_file;
      close_in input_file