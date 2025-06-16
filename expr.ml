(* 
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  let open SS in
  match exp with
  | Var x -> singleton x
  | Unop (_op, e) -> free_vars e
  | App (e1, e2)
  | Binop (_, e1, e2) -> union (free_vars e1) (free_vars e2)
  | Conditional (cond, thn, els) -> union (free_vars cond) (free_vars thn) |>
                                    union (free_vars els)
  | Fun (x, e) -> free_vars e |> remove x
  | Let (x, def, body) -> free_vars body |> remove x |> union (free_vars def)
  | Letrec (x, def, body) -> union (free_vars def) (free_vars body) |>
                             remove x
  | _ -> empty ;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no other variable names
   use the prefix "var". (Otherwise, they might accidentally be the
   same as a generated variable name.) *)
let new_varname =
  let suffix = ref 1 in
  fun () : varid -> let new_var = "var" ^ string_of_int !suffix in
                    incr suffix; 
                    new_var ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  match exp with
  | Var x -> if x = var_name then repl else exp
  | Unop (op, e) -> Unop (op, subst var_name repl e)
  | Binop (op, e1, e2) -> Binop (op, subst var_name repl e1,
                                     subst var_name repl e2)
  | Conditional (e1, e2, e3) -> Conditional (subst var_name repl e1,
                                             subst var_name repl e2,
                                             subst var_name repl e3)
  | Fun (x, e) -> if x = var_name then exp
                  else if not (SS.mem x (free_vars repl)) then
                    Fun (x, subst var_name repl e)
                  else let z = new_varname () in Fun (z, subst var_name repl 
                                                         (subst x (Var(z)) e))
  | Let (x, def, body) -> if x = var_name then Let (x, subst var_name repl def,
                                                       body)
                          else if not (SS.mem x (free_vars repl)) then
                            Let (x, subst var_name repl def,
                                 subst var_name repl body)
                          else let z = new_varname () in
                          Let (z, subst var_name repl def,
                                  subst var_name repl (subst x (Var(z)) body))
  | Letrec (x, def, body) -> if x = var_name then exp
                             else if not (SS.mem x (free_vars repl)) then
                               Letrec (x, subst var_name repl def,
                                          subst var_name repl body)
                             else let z = new_varname () in
                             Let (z, subst var_name repl (subst x (Var(z)) def),
                                     subst var_name repl (subst x (Var(z))
                                                          body))
  | App (f, arg) -> App (subst var_name repl f, subst var_name repl arg)
  | _ -> exp ;;
     
(*......................................................................
  String representations of expressions
 *)

(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  let binop_to_concrete_string (op : binop) : string =
    match op with
    | Plus -> " + "
    | Minus -> " - "
    | Times -> " * "
    | Equals -> " = "
    | LessThan -> " < " in
  match exp with
  | Var x -> x                       
  | Num n -> string_of_int n                          
  | Bool b -> Bool.to_string b                         
  | Unop (_op, e) -> "(~-" ^ exp_to_concrete_string e ^ ")"                 
  | Binop (op, e1, e2) -> "(" ^ exp_to_concrete_string e1 ^
                          binop_to_concrete_string op ^
                          exp_to_concrete_string e2 ^ ")"         
  | Conditional (cond, thn, els) -> "if " ^ exp_to_concrete_string cond ^
                                    " then " ^ exp_to_concrete_string thn ^
                                    " else " ^ exp_to_concrete_string els     
  | Fun (x, e) -> "fun " ^ x ^ " -> " ^ exp_to_concrete_string e                  
  | Let (x, def, body) -> "let " ^ x ^ " = " ^
                          exp_to_concrete_string def ^ " in " ^
                          exp_to_concrete_string body             
  | Letrec (x, def, body) -> "let rec " ^ x ^ " = " ^
                             exp_to_concrete_string def ^ " in " ^
                             exp_to_concrete_string body        
  | Raise -> "raise"                                
  | Unassigned -> "Unassigned"                           
  | App (f, arg) -> exp_to_concrete_string f ^ " " ^ exp_to_concrete_string arg
  ;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  let binop_to_abstract_string (op : binop) : string =
    match op with
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Times -> "Times"
    | Equals -> "Equals"
    | LessThan -> "LessThan" in
  match exp with
  | Var x -> "Var(" ^ x ^ ")"                         
  | Num n -> "Num(" ^ string_of_int n ^ ")"                          
  | Bool b -> "Bool(" ^ Bool.to_string b ^ ")"                         
  | Unop (_op, e) -> "Unop(Negate, " ^ exp_to_abstract_string e ^ ")"                 
  | Binop (op, e1, e2) -> "Binop(" ^ binop_to_abstract_string op ^
                          ", " ^ exp_to_abstract_string e1 ^
                          ", " ^ exp_to_abstract_string e2 ^ ")"         
  | Conditional (cond, thn, els) -> "Conditional(" ^
                                    exp_to_abstract_string cond ^ ", " ^
                                    exp_to_abstract_string thn ^ ", " ^
                                    exp_to_abstract_string els ^ ")"     
  | Fun (x, e) -> "Fun(" ^ x ^ ", " ^
                  exp_to_abstract_string e ^ ")"                  
  | Let (x, def, body) -> "Let(" ^ x ^ ", " ^
                          exp_to_abstract_string def ^ ", " ^
                          exp_to_abstract_string body ^ ")"             
  | Letrec (x, def, body) -> "Letrec(" ^ x ^ ", " ^
                             exp_to_abstract_string def ^ ", " ^
                             exp_to_abstract_string body ^ ")"        
  | Raise -> "raise"                                
  | Unassigned -> "Unassigned"                           
  | App (f, arg) -> "App(" ^ exp_to_abstract_string f ^ ", " ^
                    exp_to_abstract_string arg ^ ")" ;;
