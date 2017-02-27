(* Read a single debruijn term, in \1 style, from stdin.
   Reduce it with DeBruijn.beta_lor, and print the result.

   This program is part of the DeBruijn.beta_lor's automated tester.
*)
open Format;;

(* let teststr = "(\\1\\1)\\\\1";; *)
(* let teststr = "(\\\\2 1)(\\1)";; *)
let teststr = "\\(\\\\2 1) (\\2)";;

let db = IO.db_of_string teststr;;
print_string "  Pre: "; IO.print_db db;;

let res = match (DeBruijn.beta_lor db) with
  | Some out -> IO.print_db out;
  | None -> (print_string "No redex."; print_newline ())
;;

