(* Read a single DeBruijn term, in \1 style, from stdin.
   Reduce it with DeBruijn.beta_lor, and print the result.

   This program is part of the DeBruijn.beta_lor's automated tester.
*)
open Format
  
let _ =
  let db = IO.db_of_channel stdin in
  let output = DeBruijn.beta_lor db
  in
    match output with
    | Some out -> IO.print_db out
    | None -> (print_string "No redex."; print_newline ())
  

