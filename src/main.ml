open Prettyprint
open Constantpropagation
open Calcul
open Variables




let analyse ?(verbose = true) ?(output_channel = stderr) ast ast_expanded = 
    let var_access = get_all_variables ast ast_expanded
    in let var_type_clues = Variables.get_preproc_types ast
    in let var_access = filter_global_variables var_access 
    in let array_summary = Generatecode.get_array_summary var_access
    in let code_iterators = Generatecode.create_iterators_in_c output_channel var_access
    in let code_structures = Generatecode.generate_bounds_structures output_channel array_summary var_type_clues
    in let code_boundaries = Generatecode.compute_boundaries_in_c output_channel var_access
    in let code_parallel = Generatecode.generate_parallel_loop output_channel ast array_summary
    in let _ = Printf.fprintf output_channel "%s\n" @@
         pretty_print_ast @@
         Ast.Bloc (code_iterators @ code_structures @ code_boundaries @ code_parallel)
    in ()

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;



let compile ?(optimisation_level=0) ?(verbose=true) path =
  begin
    (* I know converting a file to a string is bad and that I should use channel, must I must add a '\n' at the begin of the file for everything to work correctly*)
    let file_content = List.fold_left (fun a b -> a ^ b ^ "\n") ("\n") (read_file path) 
    in let ast = Parser.main_parsing_file Lexer.token (Lexing.from_string file_content) 
    in let ast = Ast.Bloc ast 
    in let ast_expanded = (constant_propagation ast) 
    in let _ = analyse ~verbose:verbose ast ast_expanded
    in ()
  end







let main () = 
  let verbose = ref false in
  let o0 = ref true in
  let input_file = ref "" in
  let speclist = 
    [("-v", Arg.Set verbose, "enable verbose mode");
     ("-O0", Arg.Set o0, "apply optimisation level 0 (only checking for bounds)")]
  in let _ = Arg.parse speclist (fun x -> input_file := x) "Auto paralleliser dummy tool"
  (*in test_ineq !input_file;*)
  in compile !input_file ~optimisation_level:0;
  flush stdout;
  flush stderr

let _ = main ()
