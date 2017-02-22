(* Notes
    - Every function call must be passed arguments, unless it takes none. Then pass unit
    - Invocations do not need parentheses unless its arguments are a statement


Questions
    - Does in just define variables in the following scope?
*)
let four = 2 + 2

let greeting = "Hello, World!"

(* The parenthesis can be omitted in both the signature and the application. It most closely means "None",
its worth checking what the convention is for functions that take no arguments.
*)

let hello () = 
    (* The double semicolon prevents a value from escaping from the function? Without it you have to write
        let () = hello ()
    which... assigns unit to nothing? Adding the semicolon makes the preceeding statement procedural instead of functional.
    *)
    print_endline greeting;;

(* hello ();; *)

let add a b =
    a + b

(* Pattern matching *)
let is_hello s = 
    match s with 
    | "Hello" | "hello" -> true
    | _ -> false;;

(* Yeah, the double semi is needed when issuing statements in a non-functional context 
Note also the parenthesis on the arguments to printf*)
(* Printf.printf "%b\n" (is_hello "hello");; *)
(* Printf.printf "%b\n" (is_hello "hell");; *)


(* LISTS *)


(* Lists - immutable singly-linked lists.
   They're the default data structure for storing collections of items.
   These are not suitable for random access.
   A list is either the empty list denoted `[]`
   or a pair of the first element called the head and the rest of the list
   called the tail, e.g. `element :: other_elements`

   Lists are seperated by semicolons, tuples by commas
*)
let list0 = 1 :: (2 :: (3 :: []))
let list1 = 1 :: 2 :: 3 :: []     (* same as list0 *)
let list2 = [1; 2; 3];;             (* same as list1 *)

(* You can access individual list items with the List.nth function. *)
List.nth list1 1 ;;

(* There are higher-order functions for lists such as map and filter. *)
List.map (fun x -> x * 2) [1; 2; 3] ;;
List.filter (fun x -> x mod 2 = 0) [1; 2; 3; 4] ;;

(* Arrays are enclosed in [| |] *)
let my_array = [| 1; 2; 3 |] ;;

(* You can access array items like this: *)
my_array.(0) ;;

(* Recursive functions require the `rec` keyword (but type definitions are
   implicitly recursive). We don't need to write recursive functions
   too often in "enterprise" code but this is how all iterators
   over lists are defined, and sometimes it's better to write our own.
   The following is the same as the standard `List.filter`.
*)
let rec filter predicate list =
  match list with
  | [] -> []
  | head :: tail ->
      if predicate head then
        head :: filter predicate tail
      else
        filter predicate tail


(* Exceptions
   Exceptions are of the type `exn` which is a special variant type
   than can be extended with new cases.
 *)
exception Fishsticks of string

let found =
  try
    Some (List.find (fun x -> x < 0) [1;2;3])
  with Not_found ->
    None;;

(* Since OCaml is a functional language, it lacks "procedures".
   Every function must return something. So functions that
   do not really return anything and are called solely for their
   side effects, like print_endline, return value of "unit" type. *)

(* Definitions can be chained with "let ... in" construct.
   This is roughly the same to assigning values to multiple
   variables before using them in expressions in imperative
   languages. *)
let x = 10 in
let y = 20 in
x + y ;;

(* Anonymous functions use the following syntax: *)
let my_lambda = fun x -> x * x ;;

(* Alternatively, you can use the "function" keyword. *)
let is_one = 
    function
    | 1 -> true
    | _ -> false;;

(* Printf.printf "Is one: %b\n" (is_one 1);; *)
(* Printf.printf "Is one: %b\n" (is_one 0);; *)

(* Matching predicates, aka "guarded pattern matching". *)
let abs x =
    match x with
    | x when x < 0 -> -x
    | _ -> x
;;

(*
The "expr" types shown in the top of the lambda file is a *variant*.
    The capitalized names are its "tags," and what follow are the union of types
    that construct that tag of the type
*)
(* type color =
  | Basic of basic_color * weight (* basic colors, regular and bold *)
  | RGB   of int * int * int       (* 6x6x6 color cube *)
  | Gray  of int                   (* 24 grayscale levels *)
 *)

(* I think the fastest way forward on frombrown is a simple map.*)

(* A very simple implementation of a map that maps strings to ints and can increment them 
all at once. As hilariously inefficient as it is cool.
*)
module Dictionary =
  struct
    exception KeyNotFound
    let make () = fun _ -> raise KeyNotFound
    (* let get d k = d k *)
    let put d k v = fun k' -> if k = k' then v else d k'
    let increment d = fun k -> d k + 1 
  end    

let dict = Dictionary.make ();;
let dict = Dictionary.put dict "a" 1;;
let dict = Dictionary.put dict "b" 2;;

(* let leek =;; *)

Printf.printf "Lookup: %d\n"  (dict "a");;
Printf.printf "Lookup: %d\n"  (dict "b");;

let dict = Dictionary.increment dict;;
let dict = Dictionary.increment dict;;

Printf.printf "Lookup: %d\n"  (dict "a");;
Printf.printf "Lookup: %d\n"  (dict "b");;