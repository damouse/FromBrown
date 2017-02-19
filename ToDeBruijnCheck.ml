(* Read a single Lambda term, in \x.x style, from stdin.
   Convert it with Lambda.to_debruijn, and print the result.

   This program is part of Lambda.to_debruijn's automated tester.
*)

open Format;;

let lam = IO.lam_of_channel stdin in
IO.print_lam lam;
let db = Lambda.to_debruijn lam in
IO.print_db db
;;