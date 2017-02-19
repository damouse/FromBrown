(* The IO module provides IO functions for Lambda.expr and DeBruijn.expr *)
(* Printing functions. These print their input expressions to stdout. *)
val print_lam : Lambda.expr -> unit
  
val print_db : DeBruijn.expr -> unit
  
(* Parse a string as an expression; return that expression. *)
val lam_of_string : string -> Lambda.expr
  
val db_of_string : string -> DeBruijn.expr
  
(* Parse a channel (a file handle) as an expression; return that expression. *)
val lam_of_channel : in_channel -> Lambda.expr
  
val db_of_channel : in_channel -> DeBruijn.expr
  

