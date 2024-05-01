open Expr ;;
open CS51Utils ;;
open Absbook ;;
open Miniml ;;
open Evaluation ;;
open Env ;;

let exp_to_abstract_string_test () =
  unit_test (exp_to_abstract_string (Num 1) = "Num(1)") "abstract string num";
  unit_test (exp_to_abstract_string (Var "v") = "Var(v)") 
            "abstract string var";
  unit_test (exp_to_abstract_string (Bool true) = "Bool(true)") 
            "abstract string bool true";
  unit_test (exp_to_abstract_string (Bool false) = "Bool(false)") 
            "abstract string bool false";
  unit_test (exp_to_abstract_string (Float 2.) = "Float(2.)") 
            "abstract string float";
  unit_test (exp_to_abstract_string (Unop (Negate, Num 1)) = 
            "Unop(Neg, Num(1))") "abstract string negate";
  unit_test (exp_to_abstract_string (Binop (Times, Num 1, Num 2))
          = "Binop(Times, Num(1), Num(2))") "abstract string *";
  unit_test (exp_to_abstract_string (Binop (Plus, Num 1, Num 2))
          = "Binop(Plus, Num(1), Num(2))") "abstract string +";
  unit_test (exp_to_abstract_string (Binop (Minus, Num 1, Num 2))
          = "Binop(Minus, Num(1), Num(2))") "abstract string -";
  unit_test (exp_to_abstract_string (Binop (Equals, Num 1, Num 2))
          = "Binop(Equals, Num(1), Num(2))") "abstract string =";
  unit_test (exp_to_abstract_string (Binop (LessThan, Num 1, Num 2))
          = "Binop(LessThan, Num(1), Num(2))") "abstract string <";
  unit_test (exp_to_abstract_string (Conditional (Bool true, Num 1, Num 2))
          = "Conditional(Bool(true), Num(1), Num(2))") 
            "abstract string conditional";
  unit_test (exp_to_abstract_string (Fun("x", Binop (Times, Var "x", Num 2)))
          = "Fun(x, Binop(Times, Var(x), Num(2)))") 
            "abstract string fun binop";
  unit_test (exp_to_abstract_string (Let("x", Binop (Times, Num 1, Num 2), 
            Binop (Plus, Num 1, Num 2))) = "Let(x, Binop(Times, Num(1), \
            Num(2)), Binop(Plus, Num(1), Num(2)))") "abstract string let";
  unit_test (exp_to_abstract_string (Letrec("x", Binop (Times, Var "x", Num 2), 
            Binop(Plus, Num 1, Num 2)))
          = "Letrec(x, Binop(Times, Var(x), Num(2)), \
            Binop(Plus, Num(1), Num(2)))") "abstract string letrec";
  unit_test (exp_to_abstract_string (App (Fun("x", 
            Binop (Times, Var "x", Num 2)), Num 2)) = "App(Fun(x, \
            Binop(Times, Var(x), Num(2))), Num(2))") "abstract string complex";
  unit_test (exp_to_abstract_string Raise = "Raise") "abstract string raise";
  unit_test (exp_to_abstract_string Unassigned = "Unassigned") 
            "abstract string unassigned";
;;

let exp_to_concrete_string_test () =
  unit_test (exp_to_concrete_string (Num 1) = "1") "concrete string num";
  unit_test (exp_to_concrete_string (Var "v") = "v") "concrete string var";
  unit_test (exp_to_concrete_string (Bool true) = "true") 
            "concrete string bool true";
  unit_test (exp_to_concrete_string (Bool false) = "false") 
            "concrete string bool false";
  unit_test (exp_to_concrete_string (Unop (Negate, Num 3)) = "~-(3)") 
            "concrete string negate num";
  unit_test (exp_to_concrete_string (Binop (Times, Num 1, Num 2)) = "1 * 2") 
            "concrete string *";
  unit_test (exp_to_concrete_string (Binop (Plus, Num 1, Num 2)) = "1 + 2") 
            "concrete string +";
  unit_test (exp_to_concrete_string (Binop (Minus, Num 1, Num 2)) = "1 - 2") 
            "concrete string -";
  unit_test (exp_to_concrete_string (Binop (Equals, Num 1, Num 2)) = "1 = 2") 
            "concrete string =";
  unit_test (exp_to_concrete_string (Binop (LessThan, Num 1, Num 2)) = "1 < 2") 
            "concrete string <";
  unit_test (exp_to_concrete_string (Binop (GreaterThan, Num 1, Num 2)) = 
            "1 > 2") "concrete string >";
  unit_test (exp_to_concrete_string (Conditional (Bool true, Num 1, Num 2))
          = "if true then 1 else 2") "concrete string conditional";
          unit_test (exp_to_concrete_string (Fun ("x", 
          Binop (Times, Var "x", Num 2))) = "(fun x -> x * 2)") 
          "concrete string function";
  unit_test (exp_to_concrete_string (Let("x", Binop (Times, Num 1, Num 2), 
          Binop (Plus, Num 3, Num 4))) = "let x = 1 * 2 in (3 + 4)") 
          "concrete string let";
  unit_test (exp_to_concrete_string (Letrec ("x", Binop (Times, Var "x",
          Num 1), Binop (Plus, Num 2, Num 3)))
          = "let rec x = x * 1 in (2 + 3)") "concrete string let rec";
  unit_test (exp_to_concrete_string (App (Fun ("x", Binop 
          (Times, Var "x", Num 2)), Num 3))
          = "(fun x -> x * 2) (3)") "concrete string app";
  unit_test (exp_to_concrete_string Raise = "Raise") 
          "concrete string raise";
  unit_test (exp_to_concrete_string Unassigned = "Unassigned") 
          "concrete string unassigned";
;;

let subst_tests () =
  unit_test ((subst "x" (str_to_exp "1;;") (str_to_exp "~- 2 + 1;;")) = 
            (str_to_exp "~- 2 + 1;;")) "subst none";
  unit_test ((subst "x" (str_to_exp "1;;") (str_to_exp "~- 2 + x;;")) = 
            (str_to_exp "~- 2 + 1;;")) "subst var once";
  unit_test ((subst "x" (str_to_exp "1;;") (str_to_exp 
            "if x = 1 then y + x else z;;")) = (str_to_exp 
            "if 1 = 1 then y + 1 else z;;")) "subst conditional";
  unit_test ((subst "x" (str_to_exp "y + 1;;") (str_to_exp "fun x -> x + 1;;"))
            = (str_to_exp "fun x -> x + 1;;")) 
            "subst x = y + 1 in fun x -> x + 1";
  unit_test ((subst "x" (str_to_exp "y;;") (str_to_exp "fun z -> z * x;;"))
           = (str_to_exp "fun z -> z * y;;"))
   "subst x = y in fun z -> z * x";
  unit_test ((subst "x" (str_to_exp "y + 1;;") (str_to_exp "let x = 2 * x in x;;"))
           = (str_to_exp "let x = 2 * (y + 1) in x;;"))
    "subst Let same var";;
  
let free_vars_tests () = 
  unit_test (free_vars (str_to_exp "1;;") = (vars_of_list [])) 
    "free vars in constant";
  unit_test (free_vars (str_to_exp "x;;") = (vars_of_list ["x"])) 
    "free vars in x";
  unit_test (free_vars (Raise) = (vars_of_list [])) 
    "free vars raise";
  unit_test (free_vars (Unassigned) = (vars_of_list []))
    "free vars unassigned";
  unit_test (free_vars (Unop (Negate, Var "x")) = (vars_of_list ["x"]))
    "free vars unop";
  unit_test (free_vars (Binop (Times, Var "x", Float 3.)) = (vars_of_list ["x"]))
    "free vars in Binop float";
  unit_test (free_vars (str_to_exp "x + y;;") = (vars_of_list ["x"; "y"]))
    "free vars in Binop num";
  unit_test (free_vars (str_to_exp "let x = 5 in x + y;;") = (vars_of_list ["y"]))
    "free vars in Let";
  unit_test (free_vars (App (Var "f", Var "x")) = (vars_of_list ["f"; "x"]))
    "free vars App";
  unit_test (free_vars (Let ("x", Num 3, Unop (Negate, Var "x"))) =
  (vars_of_list []))
    "free vars let";
  unit_test (free_vars (Letrec ("x", Binop (Plus, Var "x", Num 3),
    Unop (Negate, Var "x"))) =
  (vars_of_list []))
    "free vars letrec";
  unit_test (free_vars (Fun ("x", Var "x")) = (vars_of_list []))
    "free vars Fun x -> x";
  unit_test (free_vars (Conditional (Var "x", Var "y", Var "z")) = 
  (vars_of_list ["x"; "y"; "z"])) "free vars conditional";
;;

let new_var_name_test () = 
  unit_test (new_varname () = "v1")
    "new_varname 1";
  unit_test (new_varname () = "v2")
    "new_varname 2";
  unit_test (new_varname () = "v3")
    "new_varname 3" ;;

let expr0 = Num(3);; (*3*)
(* let x = 1 in x -> 1*)
let expr1 = Let("x", Num(1), Var("x")) ;; 
(* let x = 1 in let y = 2 in x -> 1 *)
let expr2 = Let("x", Num(1), Let("y", Num(2), Var("x")));; 
(* let x = 1 in let x = x + 1 in x -> 3 *)
let expr3 = Let("x", Num(1), Let("x", Binop(Plus, Var("x"), Num(2)), Var("x")))  ;;
(* let x = fun x -> x in x 1 -> 1 *)
let expr4 = Let("x", Fun("x", Var("x")), App(Var("x"), Num(1))) ;;
(* let x = 1 in let f = fun y -> x + y in f 2 -> 3 *)
let expr5 = Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))),  
            App(Var("f"), Num(2)))) ;;
let expr6 = Binop(FTimes, Float(3.), Float(2.)) ;;
(* let x = 5 in let rec x = 10 in x -> 10 *)
let expr7 = Let("x", Num(5), Letrec("x", Num(10), Var("x"))) ;;
(* let x = 10 in let x = fun y -> y * 2 in x 20 -> 40 *)
let expr8 = Let("x", Num(1), Let("x", Fun("y", Binop(Times, Var("y"), Num(2))),
  App(Var("x"), Num(3)))) ;;
(* let rec x = 2 in let x = 1 in x -> 1 *)
let expr9 = Letrec("x", Num(2), Let("x", Num(1), Var("x"))) ;;
(* let rec f = fun x -> if x = 0 then 1 else x * f (x-1) in f 4 -> 24 *)
let expr10 = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)),
            Num(1), Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"),
            Num(1)))))), App(Var("f"), Num(4))) ;;
(* let x = 5 in let f = fun y -> x + y in let x = 10 in f 0 *)
let expr11 =  Let("x", Num(5), Let("f", Fun("y", Binop(Plus, Var("x"), 
              Var("y"))), Let("x", Num(10), App(Var("f"), Num(0))))) ;;
(* -1. -> 1. *)
let expr12 = Unop (Negate, Float(-1.)) ;;
let expr13 = Binop (FPlus, Float 1., Float 2.) ;;
let expr14 = Binop (FMinus, Float 3., Float 2.) ;;
let expr15 = Binop (FTimes, Float 2., Float 3.) ;;
let expr16 = Binop (Equals, Float 2., Float 2.) ;;
let expr17 = Binop (LessThan, Float 2., Float 3.) ;;
let expr18 = Binop (GreaterThan, Float 2., Float 3.) ;;
let expr19 = Unop (Negate, Num (1));;


let empty_env = Evaluation.Env.empty() ;;
  
let sub_test () =
  unit_test((eval_s expr0 empty_env) = Val (Num (3))) "sub expr0";
  unit_test((eval_s expr1 empty_env) = Val (Num (1))) "sub expr1";
  unit_test((eval_s expr2 empty_env) = Val (Num (1))) "sub expr2";
  unit_test((eval_s expr3 empty_env) = Val (Num (3))) "sub expr3";
  unit_test((eval_s expr4 empty_env) = Val (Num (1))) "sub expr4";
  unit_test((eval_s expr5 empty_env) = Val (Num (3))) "sub expr5";
  unit_test((eval_s expr6 empty_env) = Val (Float (6.))) "sub expr6";
  unit_test((eval_s expr7 empty_env) = Val (Num (10))) "sub expr7";
  unit_test((eval_s expr8 empty_env) = Val (Num (6))) "sub expr8";
  unit_test((eval_s expr9 empty_env) = Val (Num (1))) "sub expr9";
  unit_test((eval_s expr10 empty_env) = Val (Num (24))) "sub expr10";
  unit_test((eval_s expr11 empty_env) = Val (Num (5))) "sub expr11";
  unit_test((eval_s expr12 empty_env) = Val (Float (1.))) "sub expr12";
  unit_test((eval_s expr13 empty_env) = Val (Float (3.))) "sub expr13";
  unit_test((eval_s expr14 empty_env) = Val (Float (1.))) "sub expr14";
  unit_test((eval_s expr15 empty_env) = Val (Float (6.))) "sub expr15";
  unit_test((eval_s expr16 empty_env) = Val (Bool (true))) "sub expr16";
  unit_test((eval_s expr17 empty_env) = Val (Bool (true))) "sub expr17";
  unit_test((eval_s expr18 empty_env) = Val (Bool (false))) "sub expr18";
  unit_test((eval_s expr19 empty_env) = Val (Num (~-1))) "sub expr19";
  
;;

let dyn_test () = 
  unit_test((eval_d expr0 empty_env) = Val (Num (3))) "dyn expr0";
  unit_test((eval_d expr1 empty_env) = Val (Num (1))) "dyn expr1";
  unit_test((eval_d expr2 empty_env) = Val (Num (1))) "dyn expr2";
  unit_test((eval_d expr3 empty_env) = Val (Num (3))) "dyn expr3";
  unit_test((eval_d expr4 empty_env) = Val (Num (1))) "dyn expr4";
  unit_test((eval_d expr5 empty_env) = Val (Num (3))) "dyn expr5";
  unit_test((eval_d expr6 empty_env) = Val (Float (6.))) "dyn expr6";
  unit_test((eval_d expr7 empty_env) = Val (Num (10))) "dyn expr7";
  unit_test((eval_d expr8 empty_env) = Val (Num (6))) "dyn expr8";
  unit_test((eval_d expr9 empty_env) = Val (Num (1))) "dyn expr9";
  unit_test((eval_d expr10 empty_env) = Val (Num (24))) "dyn expr10";
  unit_test((eval_d expr11 empty_env) = Val (Num (10))) "dyn expr11";
  unit_test((eval_d expr12 empty_env) = Val (Float (1.))) "dyn expr12";
  unit_test((eval_d expr13 empty_env) = Val (Float (3.))) "dyn expr13";
  unit_test((eval_d expr14 empty_env) = Val (Float (1.))) "dyn expr14";
  unit_test((eval_d expr15 empty_env) = Val (Float (6.))) "dyn expr15";
  unit_test((eval_d expr16 empty_env) = Val (Bool (true))) "dyn expr16";
  unit_test((eval_d expr17 empty_env) = Val (Bool (true))) "dyn expr17";
  unit_test((eval_d expr18 empty_env) = Val (Bool (false))) "dyn expr18";
  unit_test((eval_d expr19 empty_env) = Val (Num (~-1))) "dyn expr19";
;;

let lex_test () =
  unit_test((eval_l expr0 empty_env) = Val (Num (3))) "lex expr0";
  unit_test((eval_l expr1 empty_env) = Val (Num (1))) "lex expr1";
  unit_test((eval_l expr2 empty_env) = Val (Num (1))) "lex expr2";
  unit_test((eval_l expr3 empty_env) = Val (Num (3))) "lex expr3";
  unit_test((eval_l expr4 empty_env) = Val (Num (1))) "lex expr4";
  unit_test((eval_l expr5 empty_env) = Val (Num (3))) "lex expr5";
  unit_test((eval_l expr6 empty_env) = Val (Float (6.))) "lex expr6";
  unit_test((eval_l expr7 empty_env) = Val (Num (10))) "lex expr7";
  unit_test((eval_l expr8 empty_env) = Val (Num (6))) "lex expr8";
  unit_test((eval_l expr9 empty_env) = Val (Num (1))) "lex expr9";
  unit_test((eval_l expr10 empty_env) = Val (Num (24))) "lex expr10";
  unit_test((eval_l expr11 empty_env) = Val (Num (5))) "lex expr11";
  unit_test((eval_l expr12 empty_env) = Val (Float (1.))) "lex expr12";
  unit_test((eval_l expr13 empty_env) = Val (Float (3.))) "lex expr13";
  unit_test((eval_l expr14 empty_env) = Val (Float (1.))) "lex expr14";
  unit_test((eval_l expr15 empty_env) = Val (Float (6.))) "lex expr15";
  unit_test((eval_l expr16 empty_env) = Val (Bool (true))) "lex expr16";
  unit_test((eval_l expr17 empty_env) = Val (Bool (true))) "lex expr17";
  unit_test((eval_l expr18 empty_env) = Val (Bool (false))) "lex expr18";
  unit_test((eval_l expr19 empty_env) = Val (Num (~-1))) "lex expr19";
;;

let test_all () =
  exp_to_abstract_string_test ();
  exp_to_concrete_string_test ();
  subst_tests ();
  free_vars_tests ();
  new_var_name_test ();
  sub_test ();
  dyn_test ();
  lex_test ();
;;

(* Invoke the full set of unit tests *)
let _ = test_all () ;;