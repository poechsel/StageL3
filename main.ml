open Prettyprint
open Constantpropagation
open Calcul
open Variables


let analyse e = 
    let r = get_all_variables e
    in let _ = Hashtbl.iter (fun name (level, p) -> 
        List.iter (fun (p, f, i) ->
            match (p, f, i) with
            | p, [], [] ->
        Printf.printf "%s: %d, %s\n" name level (print_rw_flag p)
            | p, _, l -> 
              let n = 
                pretty_print_ast @@ List.fold_left (fun a b -> Ast.Access(Ast.Array, a, b)) (Ast.Identifier name) l
              in 
        Printf.printf "%s: %d, %s\n" n level (print_rw_flag p)
          ) p) r
    in ()

let compile e =
  begin
    pretty_print e;
(*    let r = get_all_variables e
    in let _ = Hashtbl.iter (fun name (level, p) -> Printf.printf "%s: %d, %s\n" name level (print_rw_flag p)) r
    in let _ = print_endline "nex======="
    in*) let e = (constant_propagation (Ast.Bloc e)) in
    let Ast.Bloc e  = e in
    let _ = pretty_print e
    in analyse e
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
