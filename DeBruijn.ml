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

(* 
Working Example foo1
    lam: (\x.x(\y.y)) (\a.\b.b)
    db: (\1\1)\\1

    Beta reduction: (\\1)\1
*)

(* Lifting in expression e at new depth *)
let lift e depth = 
    match e with 
    | Var v -> Printf.printf "Var \n"
    | Lambda (e) -> Printf.printf "Lambda \n"
    | Apply (l, r) -> Printf.printf "Apply \n"

(*
Substitute expression e into expression l for vars that match the given depth d.

Called the first time with lambda, expression, 1. Every Var encountered at 1 is replaced with expression.
Every Lambda encountered recursively calls this function, increasing the depth. 

Note that sub should already be lifted to the initial depth of the substitution. 
*)
let rec substitute target exp depth = 
    match target with 
    (* If depth matches, correct lambda switch. Return the expression *)
    | Var v ->  Printf.printf "Value\n"; if v = depth then exp else target

    (* Increment the value *)
    | Lambda (e) -> Printf.printf "Lambda\n"; substitute e exp (depth + 1)

    | Apply (l, r) -> Printf.printf "Apply\n"; Apply ((substitute l exp depth), (substitute l exp depth))

(* 
Find a reduction, perform the reduction, and then return instead of continuing onwards. 

Every step of the match should return e, the current expression, except the substitution step. 
*)
let rec find_reduce e = 
    match e with 
        | Lambda (l) -> find_reduce l

        (* Found the redux. Perform it and return *)
        | Apply (Lambda (l), r) -> Printf.printf "Found it! \n"; substitute l r 1 

        (* Fall through on everything else *)
        | _ -> print e; e

let beta_lor e = 
    (* lift e; *)
    Some (find_reduce e)

    (* None *)
  
