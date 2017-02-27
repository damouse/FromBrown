open Format;;
(* Read DeBruijn terms, in \1 style from stdin, one per line. Reduce them with
   DeBruijn.beta_lor, and print the result of each beta reduction.

   This program is intended to make it easy to test DeBruijn.beta_lor.
*)

let rec reduce n e =
  Printf.printf "Hello";
  print_string "  "; IO.print_db e;
  if n <= 0
  then print_string "Lots of reductions, giving up."
  else match (DeBruijn.beta_lor e) with
  | Some out -> reduce (n-1) out
  | None -> () 
in
      
try
  while true do
    let line = input_line stdin in
    try
      let db = IO.db_of_string line in
      reduce 25 db
    with 
        DeBruijn.Empty -> ()
      | Parsing.Parse_error -> print_string "Parse error."; print_newline()
        
  done
with End_of_file -> exit 0
;;
