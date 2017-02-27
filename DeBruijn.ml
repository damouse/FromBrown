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

(* Lifting term by depth with cutoff

Call the function with the current depth and a cutoff of 0.
*)
let rec lift term depth cutoff = 
    match term with 
    | Var v -> 
        Printf.printf "Lifting var %d depth: %d cutoff: %d\n" v depth cutoff; 
        if v > cutoff then Var (v + depth -1) else term

    | Lambda (e) -> 
        Printf.printf "Lifting lambda: "; print term;
        let ret = Lambda (lift e depth (cutoff + 1)) in
        Printf.printf "Lift returning lambda: "; print ret;
        ret

    | Apply (l, r) -> Printf.printf "Lift apply \n";
        Apply ((lift l depth cutoff), (lift r depth cutoff))

(* Substitute "term" into "into" at the current depth *)
let rec substitute term into depth = 
    match into with 
    | Var v ->  
        Printf.printf "Sub Value: %d, depth: %d\n" v depth; 
        if v = depth then Printf.printf "Sub depth match\n" else Printf.printf "Sub depth nomatch\n";
        if v = depth then (lift term depth 0) else into
    | Lambda (e) -> 
        Printf.printf "Sub Lambda "; print into; 
        let ret = Lambda (substitute term e (depth + 1)) in 
        Printf.printf "Sub returning lambda: "; print ret;
        ret

    | Apply (l, r) -> 
        Printf.printf "Sub Apply "; print into; 
        Apply ((substitute term l depth), (substitute term r depth))

(* 
Find a beta reduction and carry it out.

Every step of the match should return e, the current expression, except the substitution step. 
*)
let rec find_reduce e = 
    match e with 
        | Var v -> 
            e
        | Lambda (l) -> 
            Printf.printf "Find lambda "; print e;
            Lambda (find_reduce l)
        | Apply (Lambda (l), r) -> 
            Printf.printf "Find matching apply "; print e; 
            substitute r l 1
        | Apply (l, r) -> 
            "Find non-matching apply "; print e; 
            Apply ((find_reduce l), (find_reduce r))

let rec has_redux e = 
    match e with 
        | Var v -> false
        | Lambda (l) -> false || (has_redux l)
        | Apply (Lambda (l), r) -> true
        | Apply (l, r) -> (has_redux l) || (has_redux r)

let beta_lor e = 
  (* Some (find_reduce e) *)
  if (has_redux e) then Some (find_reduce e) else None
