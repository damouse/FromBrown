(* Lambda.ml - Implementation of the Lambda module.

   The Lambda module contains the AST for Lambda terms, and a function
   to convert them into De Bruijn-indexed terms.
*)
(* The AST for traditional lambda terms. *)
type expr = 
    | Var of char 
    | Lambda of char * expr 
    | Apply of expr * expr

(* Parser exception for empty terms. Leave this here. *)
exception Empty
  
(* Print function, callable from to_debruijn. *)
open Format
  
type direction = | Down | Left | Right

let rec print e =
    Printf.printf "New Print Loop\n";

    let rec print_part =
        function
        | (Var c, _, _) ->  print_char c; Printf.printf "Char\n";
        | (Lambda (c, e), Left, _) -> (l_paren (); print_lambda c e; r_paren ()); Printf.printf "Lambda: Left\n";
        | (Lambda (c, e), Right, Left) -> (l_paren (); print_lambda c e; r_paren ()); Printf.printf "Lambda: Left/Right\n";
        | (Lambda (c, e), _, _) -> print_lambda c e; Printf.printf "Lambda: None\n";
        | (Apply (e1, e2), Right, _) -> (l_paren (); print_apply e1 e2 Right; r_paren ()); Printf.printf "Apply: Right\n";
        | (Apply (e1, e2), d, _) -> print_apply e1 e2 d; Printf.printf "Apply: D\n";
    and print_lambda c e =
        Printf.printf "print_lambda: "; print_char c; print_newline ();

        (print_char '\\';
        print_char c;
        print_char '.';
        print_cut ();
        open_box 0;
        print_part (e, Down, Down);
        close_box ())
    and print_apply e1 e2 d =
        (open_box 0;
        print_part (e1, Left, d);
        close_box ();
        print_cut ();
        open_box 0;
        print_part (e2, Right, d);
        close_box ())
    and l_paren _ = (open_box 0; print_char '(')
    and r_paren _ = (print_char ')'; close_box ())
    in (open_box 0; print_part (e, Down, Down); close_box (); print_newline ())
  
(* to_debruijn: converts Lambda.expr terms into equivalent DeBruijn.expr terms.

   Remember: variables free in e should be free in the result, as if
   they were pointing to a lambda binding just outside the expression.

   Replace 'DeBruijn.Var 1' with your implementation.
*)

(*
Sample case for the first task: 
    (\x.x(\y.y)) (\a.\b.b)
    (\\1)\1
*)

(* Double semi ends the current context, including operations inside functions.  *)

let to_debruijn e = 
    print e;
    (* Printf.printf "Passed: %c\n" e.Var; *)
    DeBruijn.Var 1
  

