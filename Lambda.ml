(* Lambda.ml - Implementation of the Lambda module.

   The Lambda module contains the AST for Lambda terms, and a function
   to conv them into De Bruijn-indexed terms.
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
  let rec print_part =
    function
    | (Var c, _, _) -> print_char c
    | (Lambda (c, e), Left, _) -> (l_paren (); print_lambda c e; r_paren ())
    | (Lambda (c, e), Right, Left) -> (l_paren (); print_lambda c e; r_paren ())
    | (Lambda (c, e), _, _) -> print_lambda c e
    | (Apply (e1, e2), Right, _) -> (l_paren (); print_apply e1 e2 Right; r_paren ())
    | (Apply (e1, e2), d, _) -> print_apply e1 e2 d
  and print_lambda c e =
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

(* A very simple implementation of a map that maps strings to ints and can increment them 
all at once. As hilariously inefficient as it is cool. Returns a 1 if a key is not found to 
account for unbound variables wrt the base scope.
*)
module Dict =
  struct
    let make () = fun _ -> 1
    let put d k v = fun k' -> if k = k' then v else d k'
    let increment d = fun k -> d k + 1 
  end    

(* <<<<<<< HEAD
let rec convert e dict = 
    match e with 
    | Var c ->  DeBruijn.Var (dict c)
    | Lambda (c, e) -> DeBruijn.Lambda (convert e (Dictionary.increment (Dictionary.put dict c 0)))
    | Apply (e1, e2) -> DeBruijn.Apply ((convert e1 dict), (convert e2 dict))

let to_debruijn e = 
    (* Really thought this was gold, but wiffing all the tests badly. 
    I suspect this is a bug in the testing code, honestly. *)
    convert e (Dictionary.make ())
    
======= *)
let to_debruijn e = 
    let rec conv e dict = match e with 
        | Var c ->  DeBruijn.Var (dict c)
        | Lambda (c, e) -> DeBruijn.Lambda (conv e (Dict.increment (Dict.put dict c 0)))
        | Apply (e1, e2) -> DeBruijn.Apply ((conv e1 dict), (conv e2 dict))

    in conv e (Dict.make ())
  
(* >>>>>>> 37b36be2db30f43be9f08776adac951ca7ca6892 *)

