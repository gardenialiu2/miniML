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
      try !(List.assoc varname env)
      with 
      | Not_found -> raise (EvalError ("Unbound variable " ^ varname))

    let rec extend (env : env) (varname : varid) (loc : value ref) : env =
      match env with 
      | [] -> [(varname, loc)]
      | (varid, ref) :: tl ->
        if varid = varname then (varid, loc) :: tl 
        else (varid, ref) :: (extend tl varname loc)

    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val exp -> exp_to_concrete_string exp
      | Closure (exp, env) -> if printenvp then "Closure(" ^ exp_to_concrete_string exp ^
        ", " ^ env_to_string env ^ ")"
      else exp_to_concrete_string exp

    and env_to_string (env : env) : string =
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
  Val exp ;;

type model = 
  | Sub 
  | Dyn 
  | Lex

(* extracts expression from a value *)
let extract (v : Env.value) : expr =
  match v with
  | Val x -> x
  | _ -> raise (EvalError "Extract type not supported") ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)
let binopeval (b : binop) (left_expr : expr) (right_expr : expr) : expr = 
  match b, left_expr, right_expr with
  | Plus, Num x1, Num x2 -> Num (x1 + x2)
  | Minus, Num x1, Num x2 -> Num (x1 - x2)
  | Times, Num x1, Num x2 -> Num (x1 * x2)
  | Equals, Num x1, Num x2 -> Bool (x1 = x2) 
  | LessThan, Num x1, Num x2 -> Bool (x1 < x2) 
  | GreaterThan, Num x1, Num x2 -> Bool (x1 > x2) 
  | Equals, Bool x1, Bool x2 -> Bool (x1 = x2) 
  | LessThan, Bool x1, Bool x2 -> Bool (x1 < x2) 
  | GreaterThan, Bool x1, Bool x2 -> Bool (x1 > x2) 
  | Plus, Float x1, Float x2 -> Float (x1 +. x2)
  | Minus, Float x1, Float x2 -> Float (x1 -. x2)
  | Times, Float x1, Float x2 -> Float (x1 *. x2)
  | Equals, Float x1, Float x2 -> Bool (x1 = x2) 
  | LessThan, Float x1, Float x2 -> Bool (x1 < x2) 
  | GreaterThan, Float x1, Float x2 -> Bool (x1 > x2) 
  | _ -> raise (EvalError "Invalid binop") ;;

let unopeval (u : unop) (exp : expr) : expr = 
  match u, exp with
        | Negate, Num x -> Num (~-x)
        | Negate, Float x -> Float (~-.x)
        | _, _ -> raise (EvalError "Invalid Unop")

let conditioneval (exp : expr) : bool = 
  match exp with
  | Bool b -> b 
  | _ -> raise (invalid_arg "Invalid conditional") ;;

(* eval helper function for all the diff models *)
let rec eval_helper (m : model) (exp : expr) (env : Env.env) : Env.value =
  (* ev evals when env is same *)
  let rec ev (exp : expr) : Env.value = 
    match exp with
    | Var v ->
      (match m with
      | Sub -> raise (EvalError "Unbound variable")
      | Dyn | Lex -> 
        (match lookup env v with
        | Val e -> Val e 
        | Closure (e, env_new) -> eval_helper m e env_new))
    | Num _ | Bool _ | Float _ -> Val exp 
    | Fun _ -> if m = Lex then close exp env else Val exp
    | Unop (unop, e) -> 
      Val (unopeval unop e)
    | Binop (b, e1, e2) -> Val (binopeval b (extract (ev e1)) (extract (ev e2)))
    | Conditional (if_e, then_e, else_e) ->
      if conditioneval if_e then ev then_e else ev else_e
    | Let (v, e1, e2) ->
      (match m with
      | Sub -> ev (subst v (extract (ev e1)) e2)
      (* TODO IS THIS RIGHT!! *)
      | Dyn | Lex -> eval_helper m e2 (extend env v (ref (ev e1))))
    | Letrec (v, e1, e2) -> (* TODOOO IS THIS RIGHT *)
      (match m with
      | Sub -> let val_d = extract (ev e1) in 
        eval_helper m (subst v (subst v (Letrec (v, val_d, Var v)) val_d) e2) env
      | Dyn -> (* TODO: IDK IF THIS RIGHT *) eval_helper m e2 (extend env v (ref (ev e1)))
      | Lex -> 
        (* TODO: IDK IF THIS IS RIGHT same as dyn but use references. eval_l vs eval_d. *)
        let x = ref (Val Unassigned)
        in let env_new = extend env v x
        in let vd = eval_helper Lex e1 env_new
        in (match vd with
           | Val Var _ -> raise (EvalError "Letrec unbound variable")
           | _ -> x := vd; eval_helper Lex e2 env_new))
    | App (e1, e2) -> 
      (match m with
      | Sub -> (match ev e1 with
                (* substitute val_q for x in e *)
                | Val Fun (v, e) -> ev (subst v (extract (ev e2)) e)
                | _ -> raise (EvalError "Can't apply non-function"))
      | Dyn | Lex -> (* save environment w closures *) (* {} extend *)
        (match ev e1 with
          | Val Fun (v, e) -> 
            if m = Dyn then
              let val_q = ev e2 
              in let ext = extend env v (ref (val_q))
              in eval_helper m e ext
            else raise (EvalError "Invalid application input")
          (* has to be lexical *)
          | Closure (Fun (v, e), env_old) -> 
                          let val_q = ev e2 
                          in let ext = extend env_old v (ref val_q)
                          in eval_helper m e ext
          | _ -> raise (EvalError "Invalid application")))
    | Raise -> raise (EvalError "Raise")
    | Unassigned -> raise (EvalError "Unassigned")
  in ev exp ;;


let eval_s (exp : expr) (env : Env.env) : Env.value = 
  eval_helper Sub exp env ;;
   
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
let eval_d (exp : expr) (env : Env.env) : Env.value = 
  eval_helper Dyn exp env
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
let eval_l (exp : expr) (env : Env.env) : Env.value = 
  eval_helper Lex exp env

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
   
let evaluate = eval_s ;;
