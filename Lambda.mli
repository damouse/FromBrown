(* Lambda.mli - Interface file for the Lambda module.

   The Lambda module contains the AST for Lambda terms, and a function
   to convert them into De Bruijn-indexed terms.
*)
(* expr: The AST for traditional lambda terms. *)
type expr = | Var of char | Lambda of char * expr | Apply of expr * expr

(* Empty: parser exception for empty terms. *)
exception Empty
  
(* to_debruijn: converts Lambda.expr terms into DeBruijn.expr terms. *)
val to_debruijn : expr -> DeBruijn.expr
  

