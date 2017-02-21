(* The IO module.

   Unless you're just satisfying your curiosity, you only need to
   refer to IO.mli to see how to use this module. On the other hand,
   I'm actually pretty pleased with logic behind the printing
   functions that gives them minimal parenthesization, so feel free to
   admire them. -ME *)
open Format
  
module L = Lambda
  
module D = DeBruijn
  
(*** Print standard Lambdas ***)
type direction = | Down | Left | Right

(* expr: expression to print
   direction: Left or Right if the parent of expr is the left or right child
   of an Apply, Down otherwise.
   second direction: Left or Right if our parent is an Apply, and that parent 
   is a left or right child of an Apply. Down otherwise.
   direction and bool track the information we need
   to neatly parenthesize elements.

   Seriously, I should document this better. It's pretty complicated.
 *)
let rec print_lam e =
  let rec print_part =
    function
    | (L.Var c, _, _) -> print_char c
    | (L.Lambda (c, e), Left, _) -> (l_paren (); print_lambda c e; r_paren ())
    | (L.Lambda (c, e), Right, Left) -> (l_paren (); print_lambda c e; r_paren ())
    | (L.Lambda (c, e), _, _) -> print_lambda c e
    | (L.Apply (e1, e2), Right, _) -> (l_paren (); print_apply e1 e2 Right; r_paren ())
    | (L.Apply (e1, e2), d, _) -> print_apply e1 e2 d
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
  
(* print_db is very similar to print_lam above.  The only significant
  difference is that we must further keep track of soft spaces around
  numerals, so that we always have spaces between adjacent integers in
  our output. That's exactly what need_space handles.*)
let rec print_db e =
  let rec print_part =
    function
    | (D.Var i, _, _) ->
        (if !need_space then print_space () else ();
         print_int i;
         need_space := true)
    | (D.Lambda e, Left, _) -> (l_paren (); print_lambda e; r_paren ())
    | (D.Lambda e, Right, Left) -> (l_paren (); print_lambda e; r_paren ())
    | (D.Lambda e, _, _) -> print_lambda e
    | (D.Apply (e1, e2), Right, _) -> (l_paren (); print_apply e1 e2 Right; r_paren ())
    | (D.Apply (e1, e2), d, _) -> print_apply e1 e2 d
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
  
(* Reader functions. Most of the actual implementation of these lives in 
   LambdaParse.mly, LambdaLex.mll, DeBruijnParse.mly, and DeBruijnParse.mll.
   These are just convenient handles to the parsers those generate. *)
let lam_of_string (s : string) =
  let lexbuf = Lexing.from_string s
  in LambdaParse.term LambdaLex.tokenize lexbuf
  
let db_of_string (s : string) =
  let lexbuf = Lexing.from_string s
  in DeBruijnParse.term DeBruijnLex.tokenize lexbuf
  
let lam_of_channel (chan : in_channel) =
  let lexbuf = Lexing.from_channel chan
  in LambdaParse.term LambdaLex.tokenize lexbuf
  
let db_of_channel (chan : in_channel) =
  let lexbuf = Lexing.from_channel chan
  in DeBruijnParse.term DeBruijnLex.tokenize lexbuf
  

