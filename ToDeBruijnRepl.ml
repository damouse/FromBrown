open Format
  
(* Read lambda terms in \x.x style from stdin, one per line.
   Run them through Lambda.to_debruijn, and print the result.

   This program is intended to make it easy to test Lambda.to_debruijn.
*)
let _ =
  try
    while true do
      let line = input_line stdin
      in
        try
          let lam = IO.lam_of_string line
          in (print_string "  "; IO.print_db (Lambda.to_debruijn lam))
        with | Lambda.Empty -> ()
        | Parsing.Parse_error ->
            (print_string "Parse error."; print_newline ())
      done
  with | End_of_file -> exit 0
  

