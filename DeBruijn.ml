(* DeBruijn.ml - Implementation of the Lambda module.
   
   The DeBruijn module contains the AST for De Bruijn-indexed terms,
   and a function to beta-reduce them. *)
(* The AST for De Bruijn-indexed terms. *)
type expr = 
    | Var of int 
    | Lambda of expr 
    | Apply of expr * expr

(* Empty: Parser exception for empty inputs. Leave this here. *)
exception Empty
  
(* Print function, callable from cd beta_lor *)
open Format
  
type direction = | Down | Left | Right

let rec print e =
  let rec print_part =
    function
    | (Var i, _, _) ->
        (if !need_space then print_space () else ();
         print_int i;
         need_space := true)
    | (Lambda e, Left, _) -> (l_paren (); print_lambda e; r_paren ())
    | (Lambda e, Right, Left) -> (l_paren (); print_lambda e; r_paren ())
    | (Lambda e, _, _) -> print_lambda e
    | (Apply (e1, e2), Right, _) -> (l_paren (); print_apply e1 e2 Right; r_paren ())
    | (Apply (e1, e2), d, _) -> print_apply e1 e2 d
  and need_space = ref false
  and l_paren _ = (open_box 0; print_char '('; need_space := false)
  and r_paren _ = (print_char ')'; need_space := false; close_box ())
  and print_lambda e =
    (print_string "\\";
     need_space := false;
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
  in (open_box 0; print_part (e, Down, Down); close_box (); print_newline ())
  
(* beta_lor: Perform the leftmost-outermost beta reduction on e.
   Return None if e has no redexes, and Some e' if e beta-reduces to e'. 

   Replace the 'None' here with your implementation.
*)
let beta_lor e = None
  

