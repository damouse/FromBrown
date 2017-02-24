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

(* Return a variable number of tabs. Just here for utility and excercise *)
let rec variable_tabs = function
    0 -> ""
    | _ as l -> variable_tabs (l - 1) ^ "  ";;

let print_tabbed s l = 
    Printf.printf "%s%s" (variable_tabs l) s

let tabs l = 
    Printf.printf "%s" (variable_tabs l)


let rec print_internal e  =
    let rec print_part =
        function
        | (Var c, _, _, l) ->  
            (* print_char c; *)
            tabs l; Printf.printf "Char: %c" c; 
            print_newline ();

        | (Lambda (c, e), Left, _, l) -> 
            print_tabbed "Lambda- Left: " l; 
            (l_paren (); print_lambda c e (l+1); r_paren ()); 
            print_newline ();
            
        | (Lambda (c, e), Right, Left, l) -> 
            print_tabbed "Lambda- Left/Right:" l; 
            (l_paren (); print_lambda c e (l+1); r_paren ());
            print_newline ();
            
        | (Lambda (c, e), _, _, l) -> 
            print_tabbed "Lambda- None: " l;
            print_lambda c e (l + 1);
            (* print_newline (); *)

        | (Apply (e1, e2), Right, _, l) -> 
            print_tabbed "Apply- Right: " l;
            (l_paren (); print_apply e1 e2 Right (l+1); r_paren ());
            (* print_newline (); *)

        | (Apply (e1, e2), d, _, l) -> 
            print_tabbed "Apply- Left: " l; 
            print_newline ();
            (* tabs l; Printf.printf "Apply: ";  *)
            print_apply e1 e2 d (l+1); 
            (* print_newline (); *)
            

    and print_lambda c e l =
        (* print_tabbed "print_lambda" l; *)

        (print_char '\\';
        print_char c;
        print_char '.';
        print_cut ();
        open_box 0;
        print_newline ();
        print_part (e, Down, Down, l + 1);
        close_box ())

    and print_apply e1 e2 d l =
        (* print_tabbed "print_apply" l; *)

        (open_box 0;
        print_part (e1, Left, d, l + 1);
        print_newline ();
        close_box ();
        print_cut ();
        open_box 0;
        print_part (e2, Right, d, l + 1);
        (* print_newline (); *)
        close_box ())

    and l_paren _ = (open_box 0; print_char '(')
    and r_paren _ = (print_char ')'; close_box ())

    in (open_box 0; print_part (e, Down, Down, 0); close_box ();)
  
(*
Sample case for the first task: 
    (\x.x(\y.y)) (\a.\b.b)
    (\\1)\1

I understand how to construct the expressions and how to parse them based on the work above. 

Need to do a little thinking on how to construct the values, especially as the depth of bindings increases.

In a sense the structure doesn't really change. The only thing that changes is the value of the int based 
on the nesting level, which needs to be counted outside in.

Actually, isn't that what I'm counting above with the tabs? Consider:
    
    \x.x

On the first level of indentation we know our depth is 1 and therefor the var 1 should take that same value. 
How do you know which variable is currently being tracked? The "Bound" list?

-- Dict
Pass a dict down. Every lambda (or binding) add the seen var to the dict and continue downwards. On each 
step down, increment field of dict based on depth. When a var is seen, replace it with its value.

If lambda: add the var to the dict
On each (lambdas only, right?) step, increment value and pass dict copy down

Lambda('x',             [x: 1]
  Lambda('y',           [x: 2, y: 1]
    Apply( 
      Apply(
        Var 'x',        Resolve x to 2
        Var 'y'         resolve x to 1
      ),
      Var 'x')          Resolve x to 2
    ))

Seems simple. The information being passed along is only what we need: the depth of the last bindings

-- Parent
Could also build an intermediate data structure based on the lambda tree as you move down. When a var needs 
a number, stop building the debruijin terms and hunt back upwards until its binding is discovered. Less efficient, 
but more logical.

Will likely need a more "object" like structure that can hold a reference to its parent. The refs are built up as 
we descend down the tree
*)


(* A very simple implementation of a map that maps strings to ints and can increment them 
all at once. As hilariously inefficient as it is cool. Returns a 1 if a key is not found to 
account for unbound variables wrt the base scope.
*)
module Dictionary =
  struct
    let make () = fun _ -> 1
    let put d k v = fun k' -> if k = k' then v else d k'
    let increment d = fun k -> d k + 1 
  end    

let rec convert e dict = 
    match e with 
    | Var c ->  DeBruijn.Var (dict c)
    | Lambda (c, e) -> DeBruijn.Lambda (convert e (Dictionary.increment (Dictionary.put dict c 0)))
    | Apply (e1, e2) -> DeBruijn.Apply ((convert e1 dict), (convert e2 dict))

let to_debruijn e = 
    (* Really thought this was gold, but wiffing all the tests badly. 
    I suspect this is a bug in the testing code, honestly. *)
    convert e (Dictionary.make ())
    

