open Prettyprint
open Constantpropagation
open Calcul
open Variables

let compile e =
  begin
    pretty_print e;
(*    let r = get_all_variables e
    in let _ = Hashtbl.iter (fun name (level, p) -> Printf.printf "%s: %d, %s\n" name level (print_rw_flag p)) r
    in let _ = print_endline "nex======="
    in*) pretty_print [(constant_propagation (Ast.Bloc e))]
    (*in detect_pure_for_loop e*)
  end

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel stdin

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.main Lexer.token lexbuf

(* la fonction que l'on lance ci-dessous *)
let calc () =
  let result = parse () in
  (* Expr.affiche_expr result; print_newline (); flush stdout *)
  compile result; flush stdout
;;

let _ = calc()
