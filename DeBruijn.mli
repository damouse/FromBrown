(* DeBruijn.mli - Interface to the DeBruijn module.
   
   The DeBruijn module contains the AST for De Bruijn-indexed terms,
   and a function to beta-reduce them. *)

(* expr: The AST for De Bruijn-indexed terms. *)
type expr =
    Var of int
  | Lambda of expr
  | Apply of expr * expr

(* Empty: Parser exception for empty inputs. *)
exception Empty

(* beta_lor: returns None if its input has no beta-redexes, and
   Some(e') if reducing the leftmost-outermost redex of the input gives e'. *)
val beta_lor: expr -> expr option