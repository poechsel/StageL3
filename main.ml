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
let debug_reindexable r = 
  let _ = print_endline "" in
  let _ =     Hashtbl.iter (fun (name, _)
         (wut, p, f, l, uuid) ->
          let w = if wut = None then "none" else "yas" in
              let n = 
                pretty_print_ast @@ List.fold_left (fun a b -> Ast.Access(Ast.Array, a, b)) (Ast.Identifier (name, 0)) l
              in 
              Printf.printf "%s: <%s> , %s it = %s \t[%s]\n" n w (print_rw_flag p) (__print_list pretty_print_iterator "," f) (__print_list string_of_int "," uuid)
          )  r
in print_endline ""

let analyse ?(verbose = true) ?(output_channel = stderr) ast ast_expanded = 
    let var_access = get_all_variables ast_expanded
    in let _ = debug_access var_access
    in let var_access = filter_global_variables var_access 
    in let array_summary = Generatecode.get_array_summary var_access
    in let temp = Generatecode.get_reindexable_vars var_access 
    in let _ = debug_reindexable temp
    in let _ = List.iter (fun x -> print_endline @@ pretty_print_iterator x) (Generatecode.get_iterators_from_variables var_access)
    in let _ = Generatecode.create_iterators_in_c output_channel var_access
    in let _ = Generatecode.generate_bounds_structures output_channel array_summary
    in let _ = print_endline "\nBOUNDARIES:"
    in let _ = Generatecode.compute_boundaries_in_c output_channel var_access
    in let ast = Generatecode.transform_code_par ast var_access
    in let _ = print_endline @@ pretty_print_ast ast
    in let _ = Generatecode.generate_transfer_in_openacc output_channel var_access
    in ()

let compile ?(optimisation_level=0) ?(verbose=true) path =
  begin
    let _ = if verbose then print_endline "Parsing file" 
    in let ast = Parser.main Lexer.token (Lexing.from_channel @@ open_in path) 
    in let ast = Ast.Bloc ast 
    in let _ = if verbose then print_endline "Expanding constants"
    in let ast_expanded = (constant_propagation ast) 
    in let _ = if verbose then print_endline "Expanded constant ast:"
    in let _ = print_endline @@ pretty_print_ast ast_expanded 
    in let _ = analyse ~verbose:verbose ast ast_expanded
    in ()
    (*in detect_pure_for_loop e*)
  end

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel stdin

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.main Lexer.token lexbuf



let main () = 
  let verbose = ref false in
  let o0 = ref true in
  let input_file = ref "" in
  let speclist = 
    [("-v", Arg.Set verbose, "enable verbose mode");
     ("-O0", Arg.Set o0, "apply optimisation level 0 (only checking for bounds)")]
  in let _ = Arg.parse speclist (fun x -> input_file := x) "Auto paralleliser dummy tool"
  in compile !input_file ~optimisation_level:0;
  flush stdout;
  flush stderr

let _ = main ()
