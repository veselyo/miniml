(* 
                    MiniML -- Read-Eval-Print Loop
 *)

module Ev = Evaluation ;;
module MP = Miniml_parse ;;
module ML = Miniml_lex ;;
module Ex = Expr ;;

open Printf ;;

(* str_to_exp str -- Returns the expression specified by `str` using
   the MiniML parser. *)
let str_to_exp (str: string) : Ex.expr =
  let lexbuf = Lexing.from_string str in
  let exp = MP.input ML.token lexbuf in
  exp ;;

(* repl () -- Read-eval-print loop for MiniML, which prompts for and
   evaluates MiniML expressions, printing the resulting value. Exits
   the loop and terminates upon reading an end-of-file
   (control-d). *)
let repl () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  (* set up the initial environment *)
  let env = Ev.Env.empty () in

  (* the main LOOP *)
  while true do
    (try
        (* prompt *)
        printf "<== %!";
        
        (* READ and parse an expression from the input *)
        let exp = MP.input ML.token lexbuf in 
        
        (* EVALuate it *)

        (* helper to handle evaluating and priting the results of
           different evaluation processes *)
        let evaluate_helper (exp : Ev.Env.value Lazy.t) 
                            (output : string) : unit =
          try 
            match Lazy.force exp with
            | Ev.Env.Val e ->
                printf "%s=> %s\n" output (Ex.exp_to_concrete_string e)
            | Ev.Env.Closure (_, _) -> 
                printf "%s=> %s\n" output (Ev.Env.value_to_string
                                          (Lazy.force exp))
          with
          | Ev.EvalError msg -> printf "%sx> evaluation error: %s\n" output msg
          | Ev.EvalException -> printf "%sx> evaluation exception\n" output in
          
        let sub = lazy (Ev.eval_s exp env) in
        let dyn = lazy (Ev.eval_d exp env) in
        let lex = lazy (Ev.eval_l exp env) in
        evaluate_helper sub "s";
        evaluate_helper dyn "d";
        evaluate_helper lex "l";
      with
      | MP.Error -> printf "xx> parse error\n"
      | End_of_file -> printf "Goodbye.\n"; exit 0
    );
    flush stdout
  done
;;
        
(* Run REPL if called from command line *)

try
  let _ = Str.search_forward 
            (Str.regexp "miniml\\.\\(byte\\|native\\|bc\\|exe\\)")
            (Sys.argv.(0)) 0 in
  repl ()
with Not_found -> () ;;
