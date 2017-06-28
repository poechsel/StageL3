open Prettyprint
open Constantpropagation
open Calcul
open Variables


let analyse e = 
    let r = get_all_variables e
    in let _ = Hashtbl.iter (fun name (level, p) -> 
        List.iter (fun (p, f, i, uuid) ->
            match (p, f, i, uuid) with
            | p, [], [], _ ->
        Printf.printf "%s: %d, %s\n" name level (print_rw_flag p)
            | p, _, l , _-> 
              let n = 
                pretty_print_ast @@ List.fold_left (fun a b -> Ast.Access(Ast.Array, a, b)) (Ast.Identifier (name, 0)) l
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

let arith expr = 
  let [expr] = expr in 
  let _ = print_endline "analysing expression" in
  let _ =  print_endline @@ pretty_print_ast expr in
  let expr = expand expr in
  let _ =  print_endline @@ pretty_print_ast expr in
  let expr = convert_ast_to_arithms expr ["i"; "j"] in
  let _ = print_endline @@ pretty_print_arithm expr in
  let expr = reorient expr ["i"; "j"] in
  let _ = print_endline @@ pretty_print_arithm expr in

  let _ = print_endline "moving unop" in
  let expr = move_unop_sub expr  in
  let _ = print_endline @@ pretty_print_arithm expr in

  let results = get_coefficients expr ["i"; "j"] in
  let _ = Hashtbl.iter (fun n c -> 
      Printf.printf "%s : %s\n" n
        (String.concat ", " (List.map pretty_print_arithm c))
    ) results in
  ()

(* la fonction que l'on lance ci-dessous *)
let calc () =
  let result = parse () in

    arith result; flush stdout


  (* Expr.affiche_expr result; print_newline (); flush stdout *)
  (*compile result; flush stdout*)
;;

let _ = calc()
