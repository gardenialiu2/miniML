(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error in
   the interpreter *)
exception EvalError of string ;;
  
(* Exception for evaluator runtime, generated by an explicit `raise`
   construct in the object language *)
exception EvalException ;;

(*......................................................................
  Environments and values 
 *)

module type ENV = sig
    (* the type of environments *)
    type env
           
    (* the type of values (including closures) stored in
       environments *)
    type value =
      | Val of expr
      | Closure of (expr * env)
   
    (* empty () -- Returns an empty environment *)
    val empty : unit -> env

    (* close expr env -- Returns a closure for `expr` and its `env` *)
    val close : expr -> env -> value

    (* lookup env varid -- Returns the value in the `env` for the
       `varid`, raising an `Eval_error` if not found *)
    val lookup : env -> varid -> value

    (* extend env varid loc -- Returns a new environment just like
       `env` except that it maps the variable `varid` to the `value`
       stored at `loc`. This allows later changing the value, an
       ability used in the evaluation of `letrec`. To make good on
       this, extending an environment needs to preserve the previous
       bindings in a physical, not just structural, way. *)
    val extend : env -> varid -> value ref -> env

    (* env_to_string env -- Returns a printable string representation
       of environment `env` *)
    val env_to_string : env -> string
                                 
    (* value_to_string ?printenvp value -- Returns a printable string
       representation of a value; the optional flag `printenvp`
       (default: `true`) determines whether to include the environment
       in the string representation when called on a closure *)
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : ENV =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    let empty () : env = []

    let close (exp : expr) (env : env) : value =
      Closure (exp, env)

    let lookup (env : env) (varname : varid) : value =
      failwith "lookup not implemented"

    let extend (env : env) (varname : varid) (loc : value ref) : env =
      failwith "extend not implemented"

    let value_to_string ?(printenvp : bool = true) (v : value) : string =
      failwith "value_to_string not implemented"

    let env_to_string (env : env) : string =
      match env with
      | [] -> "[]"
      | (v, r) :: tl -> 
        v ^ " -> " ^ value_to_string !r ^ ", " ^ env_to_string tl
  end
;;


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below evaluates an expression `exp`
  in an environment `env` returning a result of type `value`. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a `value` and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type `expr -> Env.env -> Env.value` for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as `eval_e` below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
open Env ;;

let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

  (* extract TODO: Env.val (expr) => expr.
     Env.val can be __??? *)
let extract (value : Env.value) : expr =
  match value with
  | Env.Val x -> x
(* The SUBSTITUTION MODEL evaluator -- to be completed *)

let binopeval (b : binop) (left_expr : expr) (right_expr : expr) : expr = 
  match b, left_expr, right_expr with
  | Plus, Num x1, Num x2 -> Num (x1 + x2)
  | Minus, Num x1, Num x2 -> Num (x1 - x2)
  | Times, Num x1, Num x2 -> Num (x1 * x2)
  | Equals, Num x1, Num x2 -> Bool (x1 = x2) (* add case for bools!!!! *)
  | Lessthan, Num x1, Num x2 -> Num (x1 + x2)
  | _ -> raise (EvalError "Invalid binop")

let conditioneval (exp : Env.value) : bool = 
  match exp with
  | Closure (_, _) -> raise (Invalid_arg "Invalid closure")
  | Val v -> match v with
            | Bool b -> b 
            | _ -> raise (Invalid_arg "Not boolean")

let rec eval_s (exp : expr) (env : Env.env) : Env.value =
  match exp with
  | Var _ -> raise (EvalError "Unbound variable") (* TO ASK!! TODO *)
  | Num _ | Bool _ | Fun _ | Raise | Unassigned -> Env.Val exp
  | Unop (unop, expr1) -> 
    let Env.Val expr2 = eval_s expr1 env in 
    match unop, expr2 with
    | Negate, Num x -> Env.Val (Num (~-x))
    | _, _ -> raise (EvalError "Can't negate non-integers")
  | Binop (binop, left_expr, right_expr) -> 
    let Env.Val left = (eval_s left_expr env) in
    let Env.Val right = (eval_s right_expr env) in
    Env.Val (binopeval binop left right) 
  | Conditional (if_expr, then_expr, else_expr) -> 
    if conditioneval if_expr then eval_s then_expr env else eval_s else_expr env
  | Let (v, def_expr, body_expr) -> 
    let Env.Val val_d = eval_s def_expr env in
    eval_s (subst v val_d body_expr) env
  | Letrec (v, expr1, expr2) -> (* TODOOOO !!!! *)
    let Env.Val val_d = eval_s def_expr env in
    eval_s (subst v val_d body_expr) env ???
  | App (expr1, expr2) -> 
    let def, body = match expr1 with
    | Fun (def_expr, body_expr) -> def_expr, body_expr
    | _ -> raise (EvalError "Can't apply non function") in
    let Env.Val val_q = eval_s expr2 in
    eval_s (subst def val_q body) env (* TODOOOO *)
   
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
   
let rec eval_d (exp : expr) (env : Env.env) : Env.value =
  match exp with 
  | Var v -> Env.Val exp
  | Num n -> n
  | Bool b -> b
  | Unop (unop, expr1) -> 
    let Val expr2 = eval_d expr1 env in 
    match unop, expr2 with
    | Negate, Num x -> Env.Val (Num (-x))
    | _, _ -> raise (EvalError "Can't negate non-integers")
  | Binop (binop, left_expr, right_expr) -> 
    let Env.Val left = eval_d left_expr env in
    let Env.Val right = eval_d right_expr env in
    Env.Val (binopeval binop left right) 
  | Conditional (if_expr, then_expr, else_expr) -> 
    if conditioneval if_expr then eval_d then_expr env else eval_d else_expr env
  | Fun (_v, _expr) -> Env.Val exp
  | Let (v, def_expr, body_expr) -> (* TODOOOO!!!! *)
    let Env.Val left = eval_d left_expr env in
    let Env.Val right = eval_d right_expr env in
    Env.Val (binopeval binop left right) 
  | Letrec (v, def_expr, body_expr) -> ??
  | App (expr1, expr2) ->
    let def, body = match expr1 with
    | Fun (v, body_expr) -> v, body_expr
    | _ -> raise (EvalError "Can't apply non function") in
    let Env.Val val_q = eval_d expr2 in
    eval_d (subst def val_q body) env (* TODOOOO. dont use subst. j update environment *)
  | Raise -> Env.Val Raise
  | Unassigned -> Env.Val Unassigned 
  
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
   
let eval_l (exp : expr) (env : Env.env) : Env.value =
  match exp with 
  | Var v -> Env.Val exp
  | Num n -> n
  | Bool b -> b
  | Unop (unop, expr1) -> 
    (* let Val expr2 = eval_d expr1 env in 
    match unop, expr2 with
    | Negate, Num x -> Env.Val (Num (-x))
    | _, _ -> raise (EvalError "Can't negate non-integers") *)
  | Binop (binop, left_expr, right_expr) -> 
    (* let Env.Val left = eval_d left_expr env in
    let Env.Val right = eval_d right_expr env in
    Env.Val (binopeval binop left right)  *)
  | Conditional (if_expr, then_expr, else_expr) -> 
    (* if conditioneval if_expr then eval_d then_expr env else eval_d else_expr env *)
  | Fun (_v, _expr) -> Env.Val exp
  | Let (v, def_expr, body_expr) -> (* TODOOOO!!!! *)
    (* let Env.Val left = eval_d left_expr env in
    let Env.Val right = eval_d right_expr env in
    Env.Val (binopeval binop left right)  *)
  | Letrec (v, def_expr, body_expr) -> ??
  | App (expr1, expr2) ->
    (* let def, body = match expr1 with
    | Fun (v, body_expr) -> v, body_expr
    | _ -> raise (EvalError "Can't apply non function") in
    let Env.Val val_q = eval_d expr2 in
    eval_d (subst def val_q body) env TODOOOO *)
  | Raise -> Env.Val Raise
  | Unassigned -> Env.Val Unassigned 

(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within `eval_s`, `eval_d`, or `eval_l`. *)

let eval_e _ = eval_l ;;
  
(* Connecting the evaluators to the external world. The REPL in
   `miniml.ml` uses a call to the single function `evaluate` defined
   here. Initially, `evaluate` is the trivial evaluator `eval_t`. But
   you can define it to use any of the other evaluators as you proceed
   to implement them. (We will directly unit test the four evaluators
   above, not the `evaluate` function, so it doesn't matter how it's
   set when you submit your solution.) *)
   
let evaluate = eval_t ;;
