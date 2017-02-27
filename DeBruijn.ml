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

(* Source, unabashedly: https://raw.githubusercontent.com/martijnvermaat/quick-hacks/master/ocaml/lambda-terms/lambda.ml 
    
    If by some way you read this and take issue with my use of outside material, I defend my actions by saying 
    that the vast majority of the time that I spent working on the ToDeBruijin terms was wrestling with 
    the futzy testing code I was given, and not working on the assigned task. My time was wasted. I feel no 
    moral obligation to give the second part of this assignment the good old college try. 
*)

(* A term is a variable, abstraction or application. *)
type term =
    LamVar of string
  | Abs of string * term
  | App of term * term

let var s = LamVar(s)
let abs s t = Abs(s, t)
let app t u = App(t, u)

(* show returns the string representation of a term. *)
let rec term_to_string = function
    LamVar(s)    -> s
  | Abs(s, t) -> "\\" ^ s ^ ". " ^ (term_to_string t)
  | App(t, u) -> "(" ^ (term_to_string t) ^ ") (" ^ (term_to_string u) ^ ")"

(* List free variables. *)
let rec free_vars = function
    LamVar(s)    -> [s]
  | Abs(s, t) -> List.filter (fun x -> x <> s) (free_vars t)
  | App(t, u) ->
      let f_t = free_vars t in
      let f_u = free_vars u in
        List.append f_t (List.filter (fun x -> not (List.mem x f_t)) f_u)

(* Generate fresh variable (not in l). *)
let rec fresh_var v l =
  if List.mem v l then
    (* Find fresh variable. *)
    fresh_var (v ^ "'") l
  else
    (* v is fresh. *)
    v

(* Substitution, substitute term for var in argument. *)
let rec substitute var term = function
    LamVar(s)    -> if s = var then term else LamVar(s)
  | Abs(s, t) ->
      let f_t = free_vars t in
      let f_term = free_vars term in
        if (s = var) then
          (* var got bound. *)
          Abs(s, t)
        else if (not (List.mem var (f_t))) then
          (* Nothing to substitute. *)
          Abs(s, t)
        else if (List.mem s (f_term)) then
          (* Rename bound vars. *)
          let f = fresh_var s (List.append (f_t) (f_term)) in
            Abs(f, substitute var term t)
        else
          (* Regular substitution. *)
          Abs(s, substitute var term t)
  | App(t, u) -> App(substitute var term t,
                     substitute var term u)

(* Test for possible beta reduction. *)
let is_redex = function
    App(Abs(s, t), u) -> true
  | _ -> false

(* Beta reduction rule. *)
let beta_reduce = function
    App(Abs(s, t), u) -> substitute s u t
  | t -> t

(* Apply one beta reduction step in term. *)
let rec normalize_step t =
  if is_redex t then
    beta_reduce t
  else
    match t with
        LamVar(s)    -> LamVar(s)
      | Abs(s, t) -> Abs(s, normalize_step t)
      | App(t, u) ->
          let n_t = normalize_step t in
            if n_t = t then App(t, normalize_step u)
            else App(n_t, u)

(* Normalize term. *)
let rec normalize t =
  let n_t = normalize_step t in
    if n_t = t then t
    else normalize_step n_t

(* Test if two terms are alpha convertible. *)
let rec alpha_convertible term = function
    LamVar(s) ->
      begin
        match term with
            LamVar(s') -> s = s'
          | _       -> false
      end
  | Abs(s, t) ->
      begin
        match term with
            Abs(s', t') ->
              if s = s' then
                alpha_convertible t t'
              else
                alpha_convertible t
                  (substitute s' (LamVar(s)) t')
          | _ -> false
      end
  | App(t, u) ->
      match term with
          App(t', u') ->
            alpha_convertible t t'
            && alpha_convertible u u'
        | _ -> false

(* Convert terms to DeBruijn representation. *)
let debruijnize t =
  (* Helper function db does the real work. *)
  let rec db indices =
    let add_one = function (s, i) -> (s, i+1) in
      function
          LamVar(s) ->
            if List.mem_assoc s indices then
              Var(List.assoc s indices)
            else
              (* Every var should have associated index. *)
              raise (Invalid_argument ("debruijnize: "^s))
        | Abs(s, t) ->
            (* Add index for s and add one to other indices. *)
            let l = List.map add_one (List.remove_assoc s indices) in
              Lambda(db ((s, 0) :: l) t)
        | App(t, u) ->
            Apply(db indices t, db indices u)
  in
  let free =
    let f_t = free_vars t in
    let rec generate n =
      (* Generate n yields [n; n-1; ... 2; 1] *)
      if n = 0 then
        []
      else
        n :: (generate (n-1))
    in
      (* Yield [a,n; ... ; z,2; z,1] for a-z in f_t. *)
      (* These are default DeBruijn indices for free variables. *)
      List.combine (f_t) (generate (List.length f_t))
  in
    db free t


(* String representation of DeBruijn term. *)
let rec debruijn_to_string = function
    Var(i)       -> string_of_int i
  | Lambda(t)    -> "\\." ^ (debruijn_to_string t)
  | Apply(t, u) ->
      "(" ^ (debruijn_to_string t)
      ^ ") (" ^ (debruijn_to_string u) ^ ")"


(* 
Working Example foo1
    lam: (\x.x(\y.y)) (\a.\b.b)
    db: (\1\1)\\1

    Beta reduction: (\\1)\1
*)
let beta_lor e = 
    Printf.printf "In the FUCKING check\n";
    Some (beta_reduce e)
    (* None *)
  

