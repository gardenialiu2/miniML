(* 
                         CS 51 Final Project
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
  | FPlus
  | FMinus
  | FTimes
  | Equals
  | LessThan
  | GreaterThan (* new *)
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float (* new *)               
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
  match exp with
  | Var v -> SS.singleton v 
  | Num _ | Bool _ | Float _ | Raise | Unassigned -> SS.empty 
  | Unop (_unop, expr) -> free_vars expr                
  | Binop (_binop, expr1, expr2) -> SS.union (free_vars expr1) (free_vars expr2)
  | Conditional (expr1, expr2, expr3) -> SS.union (SS.union (free_vars expr1) 
    (free_vars expr2)) (free_vars expr3)
  | Fun (v, expr) -> SS.remove v (free_vars expr)
  | Let (v, expr1, expr2) -> SS.union (SS.remove v (free_vars expr2)) 
    (free_vars expr1)
  | Letrec (v, expr1, expr2) -> SS.remove v (SS.union (free_vars expr1) 
    (free_vars expr2))
  | App (expr1, expr2) -> SS.union (free_vars expr1) (free_vars expr2)
  
(* new_varname () -- Returns a freshly minted `varid` with prefix
   "var" and a running counter a la `gensym`. Assumes no other
   variable names use the prefix "var". (Otherwise, they might
   accidentally be the same as a generated variable name.) *)

let new_varname : unit -> varid =
  let suffix = ref 0 in
  fun () -> 
    suffix := !suffix + 1;
    "v" ^ string_of_int !suffix ;;

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
  let subbed = subst var_name repl in
  match exp with
  | Var v -> if v = var_name then repl else exp
  | Num _ | Float _ | Bool _ | Raise | Unassigned -> exp
  | Unop (unop, expr1) -> Unop (unop, subbed expr1)                
  | Binop (binop, left_expr, right_expr) -> Binop (binop, subbed left_expr, 
    subbed right_expr)     
  | Conditional (if_expr, then_expr, else_expr) -> 
    Conditional (subbed if_expr, subbed then_expr, subbed else_expr)
  | Fun (v, expr1) ->
    if v = var_name then exp
    else if not (SS.mem v (free_vars repl)) then Fun (v, subbed expr1)
    else  
      let z = new_varname () in Fun (z, subst v (Var z) expr1) 
  | Let (v, e1, e2) -> 
    if v = var_name then Let (v, subbed e1, e2)
    else if not (SS.mem v (free_vars repl)) then Let (v, subbed e1, subbed e2)
    else  
      let z = new_varname () in Let(z, subbed e2, subst v (Var z) e1) 
  | Letrec (v, def_expr, body_expr) -> 
      if v = var_name then exp
      else if SS.mem v (free_vars repl) then 
        let z = new_varname () in Letrec (z, subbed (subst v (Var z) def_expr),
      (subbed (subst v (Var z) body_expr)))
      else Letrec (v, subbed def_expr, subbed body_expr)
  | App (expr1, expr2) -> App (subbed expr1, subbed expr2)
 
(*......................................................................
  String representations of expressions
 *)
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> v                     
  | Num n -> string_of_int n                  
  | Bool b -> string_of_bool b    
  | Float f -> string_of_float f 
  | Unop (_unop, expr) -> "~-(" ^ (exp_to_concrete_string expr) ^ ")"              
  | Binop (binop, expr1, expr2) -> let b_string = 
      (match binop with
      | Plus -> " + "
      | Minus -> " - "
      | Times -> " * "
      | Equals -> " = "
      | FPlus -> " +. "
      | FMinus -> " -. "
      | FTimes -> " *. "
      | LessThan -> " < "
      | GreaterThan -> " > "
      ) in
    (exp_to_concrete_string expr1) ^ b_string ^ (exp_to_concrete_string expr2)       
  | Conditional (expr1, expr2, expr3) -> "if " ^ (exp_to_concrete_string expr1) 
    ^ " then " ^ (exp_to_concrete_string expr2) ^ " else " ^ 
    (exp_to_concrete_string expr3) 
  | Fun (v, expr) -> "(fun " ^ v ^ " -> " ^ (exp_to_concrete_string expr) ^ 
      ")"            
  | Let (v, expr1, expr2) -> "let " ^ v ^ " = " ^ 
    (exp_to_concrete_string expr1) ^ " in (" ^ (exp_to_concrete_string expr2) ^ 
      ")"
  | Letrec (v, expr1, expr2) -> "let rec " ^ v ^ " = " ^ 
    (exp_to_concrete_string expr1) ^ " in (" ^ (exp_to_concrete_string expr2) ^ 
      ")"
  | Raise -> "Raise"                 
  | Unassigned -> "Unassigned"      
  | App (expr1, expr2) -> (exp_to_concrete_string expr1) ^ " (" ^ 
    (exp_to_concrete_string expr2) ^ ")"       
;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string = 
  match exp with
  | Var v -> "Var(" ^ v ^ ")"                    
  | Num n -> "Num(" ^ string_of_int n ^ ")"                 
  | Bool b -> "Bool(" ^ (if b then "true" else "false") ^ ")"     
  | Float f -> "Float(" ^ string_of_float f ^ ")"           
  | Unop (_unop, expr) -> "Unop(Neg, " ^ (exp_to_abstract_string expr) ^ ")"                
  | Binop (binop, expr1, expr2) -> let b_string = 
      (match binop with
      | Plus -> "Plus"
      | Minus -> "Minus"
      | Times -> "Times"
      | FPlus -> "FPlus"
      | FMinus -> "FMinus"
      | FTimes -> "FTimes"
      | Equals -> "Equals"
      | LessThan -> "LessThan"
      | GreaterThan -> "GreaterThan"
      ) in
    "Binop(" ^ b_string ^ ", " ^ (exp_to_abstract_string expr1) ^ ", " ^ 
    (exp_to_abstract_string expr2) ^ ")"     
  | Conditional (expr1, expr2, expr3) -> "Conditional(" ^ 
    (exp_to_abstract_string expr1) ^ ", " ^ (exp_to_abstract_string expr2) ^ 
    ", " ^ (exp_to_abstract_string expr3) ^ ")"
  | Fun (v, expr) -> "Fun(" ^ v ^ ", " ^ (exp_to_abstract_string expr) ^ ")"           
  | Let (v, expr1, expr2) -> "Let(" ^ v ^ ", " ^ (exp_to_abstract_string expr1) 
    ^ ", " ^ (exp_to_abstract_string expr2) ^ ")"
  | Letrec (v, expr1, expr2) -> "Letrec(" ^ v ^ ", " ^ 
    (exp_to_abstract_string expr1) ^ ", " ^ (exp_to_abstract_string expr2) ^ 
    ")"
  | Raise -> "Raise"                   
  | Unassigned -> "Unassigned"                      
  | App (expr1, expr2) -> "App(" ^ (exp_to_abstract_string expr1) ^ ", " ^ 
    (exp_to_abstract_string expr2) ^ ")"   
;;
