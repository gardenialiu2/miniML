open Expr ;;
open CS51Utils ;;
open Absbook ;;
open Miniml ;;
open Evaluation ;;
open Miniml_parse ;;
open Miniml_lex ;;


let exp_to_abstract_string_test () =
  unit_test (exp_to_abstract_string (Num 1) = "Num(1)") "abstract string num";
  unit_test (exp_to_abstract_string (Var "v") = "Var(v)") "abstract string var";
  unit_test (exp_to_abstract_string (Bool true) = "Bool(true)") "abstract string bool true";
  unit_test (exp_to_abstract_string (Bool false) = "Bool(false)") "abstract string bool false";
  unit_test (exp_to_abstract_string (Unop (Negate, Num 1)) = "Unop(Neg, Num(1))") "abstract string negate";
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
          = "Conditional(Bool(true), Num(1), Num(2))") "abstract string conditional";
  unit_test (exp_to_abstract_string (Fun("x", Binop (Times, Var "x", Num 2)))
          = "Fun(x, Binop(Times, Var(x), Num(2)))") "abstract string fun binop";
  unit_test (exp_to_abstract_string (Let("x", Binop (Times, Num 1, Num 2), 
            Binop (Plus, Num 1, Num 2)))
          = "Let(x, Binop(Times, Num(1), Num(2)), Binop(Plus, Num(1), Num(2)))") "abstract string let";
  unit_test (exp_to_abstract_string (Letrec("x", Binop (Times, Var "x", Num 2), 
            Binop(Plus, Num 1, Num 2)))
          = "Letrec(x, Binop(Times, Var(x), Num(2)), Binop(Plus, Num(1), Num(2)))") "abstract string letrec";
  unit_test (exp_to_abstract_string (App (Fun("x", Binop (Times, Var "x", Num 2)), Num 2))
          = "App(Fun(x, Binop(Times, Var(x), Num(2))), Num(2))") "abstract string complex";
  unit_test (exp_to_abstract_string Raise = "Raise") "abstract string raise";
  unit_test (exp_to_abstract_string Unassigned = "Unassigned") "abstract string unassigned";
;;

let exp_to_concrete_string_test () =
  unit_test (exp_to_concrete_string (Num 1) = "1") "concrete string num";
  unit_test (exp_to_concrete_string (Var "v") = "v") "concrete string var";
  unit_test (exp_to_concrete_string (Bool true) = "true") "concrete string bool true";
  unit_test (exp_to_concrete_string (Bool false) = "false") "concrete string bool false";
  unit_test (exp_to_concrete_string (Unop (Negate, Num 3)) = "~-(3)") "concrete string negate num";
  unit_test (exp_to_concrete_string (Binop (Times, Num 1, Num 2)) = "1 * 2") "concrete string *";
  unit_test (exp_to_concrete_string (Binop (Plus, Num 1, Num 2)) = "1 + 2") "concrete string +";
  unit_test (exp_to_concrete_string (Binop (Minus, Num 1, Num 2)) = "1 - 2") "concrete string -";
  unit_test (exp_to_concrete_string (Binop (Equals, Num 1, Num 2)) = "1 = 2") "concrete string =";
  unit_test (exp_to_concrete_string (Binop (LessThan, Num 1, Num 2)) = "1 < 2") "concrete string <";
  unit_test (exp_to_concrete_string (Binop (GreaterThan, Num 1, Num 2)) = "1 > 2") "concrete string >";
  unit_test (exp_to_concrete_string (Conditional (Bool true, Num 1, Num 2))
          = "if true then 1 else 2") "concrete string conditional";
          unit_test (exp_to_concrete_string (Fun ("x", Binop (Times, Var "x", Num 2)))
          = "(fun x -> x * 2)") "concrete string function";
          unit_test (exp_to_concrete_string (Let("x", Binop (Times, Num 1, Num 2), Binop (Plus,
                                                              Num 3,
                                                              Num 4)))
          = "let x = 1 * 2 in (3 + 4)") "concrete string let";
          unit_test (exp_to_concrete_string (Letrec ("x", Binop (Times, Var "x", Num 1), Binop (Plus,
                                                                    Num 2,
                                                                    Num 3)))
          = "let rec x = x * 1 in (2 + 3)") "concrete string let rec";
          unit_test (exp_to_concrete_string (App (Fun ("x", Binop (Times, Var "x", Num 2)), Num 3))
          = "(fun x -> x * 2) (3)") "concrete string app";
          unit_test (exp_to_concrete_string Raise = "Raise") "concrete string raise";
          unit_test (exp_to_concrete_string Unassigned = "Unassigned") "concrete string unassigned";
;;

let subst_tests () =
  unit_test ((subst "x" (str_to_exp "1;;") (str_to_exp "~- 2 + 1;;")) = (str_to_exp "~- 2 + 1;;")) 
   "subst none";
  unit_test ((subst "x" (str_to_exp "1;;") (str_to_exp "~- 2 + x;;")) = (str_to_exp "~- 2 + 1;;")) 
   "subst var once";
  unit_test ((subst "x" (str_to_exp "1;;") (str_to_exp "if x = 1 then y + x else z;;"))
           = (str_to_exp "if 1 = 1 then y + 1 else z;;"))
   "subst conditional";
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
  (* TODO test for unop *)
  unit_test (free_vars (str_to_exp "1;;") = (vars_of_list [])) 
    "free vars in constant";
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
  (* unit_test (free_vars (str_to_exp "let rec f x = if x = 0 then 1 else x * f (x - 1) in f 5;;") = (vars_of_list []))
    "free vars in Letrec";; *)
  
  (* TODO test for application + fun. exercise 129 in textbook *)

  (* let rec free_vars (exp : expr) : varidset = *)
;;

let test_all () =
  exp_to_abstract_string_test ();
  exp_to_concrete_string_test ();
  subst_tests ();
  free_vars_tests ();
;;

  
(* Invoke the full set of unit tests *)
let _ = test_all () ;;