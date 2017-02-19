(* Lambda.ml - Implementation of the Lambda module.

   The Lambda module contains the AST for Lambda terms, and a function
   to convert them into De Bruijn-indexed terms.
*)

(* The AST for traditional lambda terms. *)
type expr =
    Var of char 
  | Lambda of char * expr
  | Apply of expr * expr

(* Parser exception for empty terms. Leave this here. *)
exception Empty

(* Print function, callable from to_debruijn. *)
open Format;;
type direction = Down | Left | Right
let rec print e = 
  let rec print_part = 
    ( function
      | (Var c, _, _) -> print_char c
      | (Lambda(c, e), Left, _) -> l_paren(); print_lambda c e; r_paren()
      | (Lambda(c, e), Right, Left) -> l_paren(); print_lambda c e; r_paren()
      | (Lambda(c, e), _, _) -> print_lambda c e;
      | (Apply(e1, e2), Right, _) -> l_paren(); print_apply e1 e2 Right; r_paren()
      | (Apply(e1, e2), d, _) -> print_apply e1 e2 d
     )
  and print_lambda c e =
    print_char '\\'; print_char c; print_char '.'; print_cut();
    open_box 0; print_part (e, Down, Down); close_box()
  and print_apply e1 e2 d =
    open_box 0; print_part (e1, Left, d); close_box(); print_cut();
    open_box 0; print_part (e2, Right, d); close_box()
  and l_paren _ = open_box 0; print_char '('
  and r_paren _ = print_char ')'; close_box ()

  in open_box 0; print_part (e,Down,Down); close_box(); print_newline()

(* to_debruijn: converts Lambda.expr terms into equivalent DeBruijn.expr terms.

   Remember: variables free in e should be free in the result, as if
   they were pointing to a lambda binding just outside the expression.

   Replace 'DeBruijn.Var 1' with your implementation.
*)
let to_debruijn e = DeBruijn.Var 1
