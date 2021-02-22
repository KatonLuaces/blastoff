open Ast

let _ =
  if Array.length Sys.argv <> 2 then
      Printf.fprintf stderr "Usage: %s input_filename\n" Sys.argv.(0)
  else
      let input_file = open_in Sys.argv.(1) in
        try
            ast_test infile
        with (Failure f) ->
            Printf.fprintf stderr "\nERROR: %s\n" f;
        close_in infile
