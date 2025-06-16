(* 
                           MiniML -- Tests
*)

open Expr ;;
open Evaluation ;;
open CS51Utils ;;
open Absbook ;;


(* definition of expressions I will use throughout this test file *)
let var_e = Var "x" ;;
let num_e = Num 10 ;;
let bool_e = Bool true ;;
let unop_e = Unop (Negate, Num 10) ;;
let unop_var = Unop (Negate, Var "x") ;;
let binop_e = Binop (Plus, Num 10, Num 10) ;;
let binop_var = Binop (Times, Var "n", Num 10) ;;
let cond_e = Conditional (Binop (Equals, Num 10, Num 10), Num 1, Num 0)
let cond_vars =
  Conditional (Binop (Equals, Var "z", Num 10), Var "z", Var "y") ;;
let fun_e = Fun("x", Binop (Plus, var_e, var_e)) ;;
let fun_vars = Fun("y", Binop (Plus, Var "y", Var "x")) ;;
let let_e = Let("x", Num 10, Binop (Minus, var_e, var_e)) ;;
let let_vars = 
  Let("f", Fun("x", Binop(Times, Var "x", Var "y")), App (Var "f", Num 10)) ;;
let letrec_e = Letrec("f", Fun("x", Conditional(Binop(Equals, Var "x", Num 0),
               Num 1, Binop(Times, Var "x", App(Var "f", Binop(Minus, Var "x",
               Num 1))))), App(Var "f", Num 4)) ;;
let letrec_var = Letrec("f", Fun("x", Conditional(Binop(Equals, Var "x", Num 0),
                 Var "y", Binop(Times, Var "x", App(Var "f", Binop(Minus, 
                 Var "x", Num 1))))), App(Var "f", Num 4))
let raise_e = Raise ;;
let unass_e = Unassigned ;;
let app_e = App (Var "x", Num 10) ;;

(* exp_to_concrete_string tests *)
let concrete_tests () =
  unit_test (exp_to_concrete_string var_e = "x") "concrete var";
  unit_test (exp_to_concrete_string num_e = "10") "concrete num";
  unit_test (exp_to_concrete_string bool_e = "true") "concrete bool";
  unit_test (exp_to_concrete_string unop_e = "(~-10)") "concrete unop";
  unit_test (exp_to_concrete_string binop_e = "(10 + 10)") "concrete binop";
  unit_test (exp_to_concrete_string cond_e = "if (10 = 10) then 1 else 0")
            "concrete conditional";
  unit_test (exp_to_concrete_string fun_e = "fun x -> (x + x)") "concrete fun";
  unit_test (exp_to_concrete_string let_e = "let x = 10 in (x - x)")
            "concrete let";
  unit_test (exp_to_concrete_string letrec_e = 
            "let rec f = fun x -> if (x = 0) then 1 else (x * f (x - 1)) in f 4")
            "concrete letrec";
  unit_test (exp_to_concrete_string raise_e = "raise") "concrete raise";
  unit_test (exp_to_concrete_string unass_e = "Unassigned")
            "concrete unassigned" ;;

(* exp_to_abstrac_string tests, can't break letrec, otherwise test fails *)
let abstract_tests () =
  unit_test (exp_to_abstract_string var_e = "Var(x)") "abstract var";
  unit_test (exp_to_abstract_string num_e = "Num(10)") "abstract num";
  unit_test (exp_to_abstract_string bool_e = "Bool(true)") "abstract bool";
  unit_test (exp_to_abstract_string unop_e = "Unop(Negate, Num(10))")
            "abstract unop";
  unit_test (exp_to_abstract_string binop_e = "Binop(Plus, Num(10), Num(10))")
            "abstract binop";
  unit_test (exp_to_abstract_string cond_e = 
            "Conditional(Binop(Equals, Num(10), Num(10)), Num(1), Num(0))")
            "abstract conditional";
  unit_test (exp_to_abstract_string fun_e = 
            "Fun(x, Binop(Plus, Var(x), Var(x)))")
            "abstract fun";
  unit_test (exp_to_abstract_string let_e = 
            "Let(x, Num(10), Binop(Minus, Var(x), Var(x)))")
            "abstract let";
  unit_test (exp_to_abstract_string letrec_e = 
            "Letrec(f, Fun(x, Conditional(Binop(Equals, Var(x), Num(0)), Num(1), Binop(Times, Var(x), App(Var(f), Binop(Minus, Var(x), Num(1)))))), App(Var(f), Num(4)))")
            "abstract letrec";
  unit_test (exp_to_abstract_string raise_e = "raise") "abstract raise";
  unit_test (exp_to_abstract_string unass_e = "Unassigned")
            "abstract unassigned" ;;

(* free_vars tests *)
let free_vars_tests () =
  unit_test (same_vars (free_vars var_e) (vars_of_list ["x"])) "free_vars var";
  unit_test (same_vars (free_vars num_e) (vars_of_list [])) "free_vars num";
  unit_test (same_vars (free_vars bool_e) (vars_of_list [])) "free_vars bool";
  unit_test (same_vars (free_vars unop_e) (vars_of_list [])) "free_vars unop";
  unit_test (same_vars (free_vars binop_var) (vars_of_list ["n"])) 
            "free_vars binop";
  unit_test (same_vars (free_vars cond_vars) (vars_of_list ["z"; "y"]))
            "free_vars conditional";
  unit_test (same_vars (free_vars fun_e) (vars_of_list [])) "free_vars fun";
  unit_test (same_vars (free_vars fun_vars) (vars_of_list ["x"])) 
            "free_vars fun vars";
  unit_test (same_vars (free_vars let_e) (vars_of_list [])) "free_vars let";
  unit_test (same_vars (free_vars let_vars) (vars_of_list ["y"])) 
            "free_vars let vars";
  unit_test (same_vars (free_vars letrec_e) (vars_of_list []))
            "free_vars letrec";
  unit_test (same_vars (free_vars raise_e) (vars_of_list [])) 
            "free_vars raise";
  unit_test (same_vars (free_vars unass_e) (vars_of_list []))
            "free_vars unassigned" ;;

(* subst tests *)
let var_name = "x" ;;
let repl = Num 10 ;;

let subst_tests () =
  unit_test (subst var_name repl (Var "x") = Num 10) "subst var same";
  unit_test (subst var_name repl (Var "y") = Var "y") "subst var diff";
  unit_test (subst var_name repl num_e = Num 10) "subst num";
  unit_test (subst var_name repl bool_e = Bool true) "subst bool";
  unit_test (subst var_name repl unop_e = Unop(Negate, Num 10)) "subst unop";
  unit_test (subst "z" repl binop_var = Binop(Times, Var "n", Num 10))
            "subst binop no var";
  unit_test (subst "z" repl cond_vars = 
            Conditional (Binop (Equals, Num 10, Num 10), Num 10, Var "y"))
            "subst conditional";
  unit_test (subst "y" repl fun_vars = Fun("y", Binop (Plus, Var "y", Var "x")))
            "subst fun no free vars";
  unit_test (subst "y" repl let_vars = 
            Let("f", Fun("x", Binop(Times, Var "x", Num 10)),
            App (Var "f", Num 10)))
            "subst let";
  unit_test (subst "y" repl letrec_var = 
            Letrec("f", Fun("x", Conditional(Binop(Equals, Var "x", Num 0),
            Num 10, Binop(Times, Var "x", App(Var "f", Binop(Minus, Var "x",
            Num 1))))), App(Var "f", Num 4)))
            "subst letrec";
  unit_test (subst var_name repl raise_e = Raise)
            "subst raise";
  unit_test (subst var_name repl unass_e = Unassigned)
            "subst unassigned";          
  unit_test (subst var_name (Fun("x", Binop(Times, Var "x", Num 10)))
            app_e = App (Fun("x", Binop(Times, Var "x", Num 10)), Num 10))
            "subst app" ;;

(* eval_s tests, we will mostly evaluate the already substituted expressions
   from subst_tests *)
let envi = Env.empty () ;;

let eval_s_tests () =
  let open Env in 
  unit_test (eval_s (subst var_name repl (Var "x")) envi = Val(Num 10)) 
           "eval_s subst var";
  unit_test (try let _ = (eval_s (subst var_name repl (Var "y")) envi) in false
            with | EvalError _ -> true) "eval_s var";
  unit_test (eval_s (subst var_name repl num_e) envi = Val (Num 10)) 
            "eval_s num";
  unit_test (eval_s (subst var_name repl bool_e) envi = Val (Bool true)) 
            "eval_s bool";
  unit_test (eval_s (subst var_name repl unop_e) envi = Val (Num (~-10))) 
            "eval_s unop";
  unit_test (eval_s (subst "n" repl binop_var) envi = Val (Num 100))
            "eval_s binop";
  unit_test (eval_s (subst "z" repl cond_vars) envi = Val (Num 10))
            "eval_s conditional";
  unit_test (eval_s (subst "y" repl fun_vars) envi = 
            Val (Fun("y", Binop (Plus, Var "y", Var "x"))))
            "eval_s fun";
  unit_test (eval_s (subst "y" repl let_vars) envi = Val (Num 100))
            "eval_s let";
  unit_test (eval_s (subst "y" repl letrec_var) envi = Val (Num 240))
            "eval_s letrec";
  unit_test (try let _ = eval_s (subst var_name repl raise_e) envi in false 
             with | EvalException -> true)
            "eval_s raise";
  unit_test (eval_s (subst var_name repl unass_e) envi = Val (Unassigned))
            "eval_s unassigned";          
  unit_test (eval_s (subst var_name (Fun("x", Binop(Times, Var "x", Num 10)))
            app_e) envi = Val (Num 100))
            "eval_s app" ;;

(* environment tests *)
let env_tests () = 
  let open Env in
  unit_test (close (Num 10) envi = Closure (Num 10, envi)) "env close empty";
  unit_test (lookup (extend envi "x" (ref (Val(Num 10)))) "x" = Val (Num 10)) 
           "env lookup extend";
  unit_test (try let _ = lookup envi "x" in false with | EvalError _ -> true) 
  "env lookup empty";
  unit_test (value_to_string (Closure (fun_e, envi)) =
            "fun x -> (x + x) where {}") "env val_to_str closure true";
  unit_test (value_to_string ~printenvp: false (Closure (fun_e, envi)) =
            "fun x -> (x + x)") "env val_to_str closure false";
  unit_test (env_to_string (extend envi "x" (ref (Val(Num 10)))) = "{x -> 10}")
            "env env_to_str" ;;

(* tests for eval_d and eval_s, they will only differ in expressions where they
   should, for which I will provide two seperate tests *)
let diff = Let("x", Num 2, Let ("f", Fun ("y", 
             Binop (Times, Var "x", Var "y")), 
             Let ("x", Num 1, App(Var "f", Num 21)))) ;;

let eval_ld_tests () = 
  let open Env in 
  unit_test (eval_l (subst var_name repl (Var "x")) envi = Val(Num 10)) 
           "eval_l subst var";
  unit_test (try let _ = (eval_l (subst var_name repl (Var "y")) envi) in false
            with | EvalError _ -> true) "eval_l var";
  unit_test (eval_l (subst var_name repl num_e) envi = Val (Num 10)) 
            "eval_l num";
  unit_test (eval_l (subst var_name repl bool_e) envi = Val (Bool true)) 
            "eval_l bool";
  unit_test (eval_l (subst var_name repl unop_e) envi = Val (Num (~-10))) 
            "eval_l unop";
  unit_test (eval_l (subst "n" repl binop_var) envi = Val (Num 100))
            "eval_l binop";
  unit_test (eval_l (subst "z" repl cond_vars) envi = Val (Num 10))
            "eval_l conditional";
  unit_test (eval_l (subst "y" repl fun_vars) envi = 
            Closure (Fun("y", Binop (Plus, Var "y", Var "x")), envi))
            "eval_l diff fun";
  unit_test (eval_d (subst "y" repl fun_vars) envi = 
            Val (Fun("y", Binop (Plus, Var "y", Var "x"))))
            "eval_d diff fun";
  unit_test (eval_l (subst "y" repl let_vars) envi = Val (Num 100))
            "eval_l let";
  unit_test (eval_l (subst "y" repl letrec_var) envi = Val (Num 240))
            "eval_l letrec";
  unit_test (try let _ = eval_l (subst var_name repl raise_e) envi in false 
             with | EvalException -> true)
            "eval_l raise";
  unit_test (eval_l (subst var_name repl unass_e) envi = Val (Unassigned))
            "eval_l unassigned";          
  unit_test (eval_l (subst var_name (Fun("x", Binop(Times, Var "x", Num 10)))
            app_e) envi = Val (Num 100))
            "eval_l app";
  unit_test (eval_l diff envi = Val (Num 42)) "eval_l diff app";
  unit_test (eval_d diff envi = Val (Num 21)) "eval_d diff app" ;;

(* tests for eval_e, I will just set it equal to the results of previous tests
   depending on the scope, I called eval_l in the previous tests where
   dynamic and lexical evaluations evaluate to the same result, so in this
   I will evaluate them in dynamic, just to make sure they are the same *)

let eval_e_tests () = 
  let open Env in 
  unit_test (eval_l (subst var_name repl (Var "x")) envi =
            eval_e (subst var_name repl (Var "x")) envi Dyn) 
           "eval_e subst var";
  unit_test (try let _ = (eval_e (subst var_name repl (Var "y")) envi Dyn)
             in false with | EvalError _ -> true) "eval_e var";
  unit_test (eval_e (subst var_name repl num_e) envi Dyn = Val (Num 10)) 
            "eval_e num";
  unit_test (eval_e (subst var_name repl bool_e) envi Dyn = Val (Bool true)) 
            "eval_e bool";
  unit_test (eval_e (subst var_name repl unop_e) envi Dyn = Val (Num (~-10))) 
            "eval_e unop";
  unit_test (eval_e (subst "n" repl binop_var) envi Dyn = Val (Num 100))
            "eval_e binop";
  unit_test (eval_e (subst "z" repl cond_vars) envi Dyn = Val (Num 10))
            "eval_e conditional";
  unit_test (eval_e (subst "y" repl fun_vars) envi Lex = 
            Closure (Fun("y", Binop (Plus, Var "y", Var "x")), envi))
            "eval_e lex diff fun";
  unit_test (eval_e (subst "y" repl fun_vars) envi Dyn = 
            Val (Fun("y", Binop (Plus, Var "y", Var "x"))))
            "eval_e dyn diff fun";
  unit_test (eval_e (subst "y" repl let_vars) envi Dyn = Val (Num 100))
            "eval_e let";
  unit_test (eval_e (subst "y" repl letrec_var) envi Dyn = Val (Num 240))
            "eval_e letrec";
  unit_test (try let _ = eval_e (subst var_name repl raise_e) envi Dyn in false 
             with | EvalException -> true)
            "eval_e raise";
  unit_test (eval_e (subst var_name repl unass_e) envi Dyn = Val (Unassigned))
            "eval_e unassigned";          
  unit_test (eval_e (subst var_name (Fun("x", Binop(Times, Var "x", Num 10)))
            app_e) envi Dyn = Val (Num 100))
            "eval_e app";
  unit_test (eval_e diff envi Lex = Val (Num 42)) "eval_e lex diff app";
  unit_test (eval_e diff envi Dyn = Val (Num 21)) "eval_e dyn diff app" ;;


let test_all = 
  concrete_tests ();
  abstract_tests ();
  free_vars_tests ();
  subst_tests ();
  eval_s_tests ();
  env_tests ();
  eval_ld_tests ();
  eval_e_tests () ;; 
