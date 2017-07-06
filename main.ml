open Prettyprint
open Constantpropagation
open Calcul
open Variables



let debug_access r = 
    Hashtbl.iter (fun name (level, p) -> 
        List.iter (fun (p, f, i, uuid) ->
            match (p,  f, i, uuid) with
            | p, [], [], _ ->
        Printf.printf "%s: %d, %s\n" name level (print_rw_flag p)
            | p, f, l , _-> 
              let n = 
                pretty_print_ast @@ List.fold_left (fun a b -> Ast.Access(Ast.Array, a, b)) (Ast.Identifier (name, 0)) l
              in 
              Printf.printf "%s: %d, %s it = %s \t[%s]\n" n level (print_rw_flag p) (__print_list pretty_print_iterator "," f) (__print_list string_of_int "," uuid)
          ) p) r

let analyse ast ast_expanded = 
    let var_access = get_all_variables ast_expanded
    in let _ = debug_access var_access
    in let _ = List.iter (fun x -> print_endline @@ pretty_print_iterator x) (Generatecode.get_iterators_from_variables var_access)
    in let _ = Generatecode.create_iterators_in_c var_access
    in let _ = print_endline "\nBOUNDARIES:"
    in let _ = Generatecode.compute_boundaries_in_c var_access
    in let ast = Generatecode.transform_code_par ast var_access
    in let _ = print_endline @@ pretty_print_ast ast
    in let _ = Generatecode.generate_transfer_in_openacc var_access
    in ()

let compile ast =
  begin
    let ast = Ast.Bloc ast in
    let ast_expanded = (constant_propagation ast) in
    let _ = print_endline @@ pretty_print_ast ast_expanded in
    let _ = analyse ast ast_expanded
    in ()
    (*in detect_pure_for_loop e*)
  end

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel stdin

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.main Lexer.token lexbuf
(*
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
*)
(* la fonction que l'on lance ci-dessous *)
let calc () =
  let result = parse () in
  compile result;
  flush stdout
;;

let _ = calc()
