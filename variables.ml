open Ast
open Prettyprint

let read = 1 lsl 0
let write = 1 lsl 1
let is_array = 1 lsl 2
let is_function = 1 lsl 3

let print_rw_flag f =
  let r = if f land read = read then "read " else ""
  in let w = if f land write = write then "write " else ""
  in let a = if f land is_array = is_array then "array" else ""
  in let u = if f land is_function = is_function then "function " else ""
  in r ^ w ^ a ^ u
      

let get_all_variables program =
  let tbl = Hashtbl.create 0 in
  let rec aux level permission program = 
  match program with
    | Identifier ";" | Identifier ""-> ()
    | Identifier s -> 
      print_string "ident\n";
      print_endline (pretty_print_ast program);
      print_endline "";
      if Hashtbl.mem tbl s then 
              let l', p' = Hashtbl.find tbl s in
 Hashtbl.replace tbl s (l', permission lor p' )

      else Hashtbl.add tbl s (-1, permission)

    | Declaration (_, l) ->
      print_string "declaration\n";
      print_endline (pretty_print_ast program);
      print_endline "";
      List.iter (fun (s, _, d, a) ->
          let f_a = match d with | DeArray _ -> is_array | DeFunction _ -> is_function | _ -> 0 in
          let _ = if Hashtbl.mem tbl s then 
              let l', p' = Hashtbl.find tbl s in
              Hashtbl.replace tbl s (level, permission lor f_a lor p' lor write)
            else Hashtbl.add tbl s (level, permission lor write lor f_a)
          in match a with | None -> () | Some a -> aux level permission a  ) l

    | InitializerList l 
    | Expression l ->
      List.iter (aux level permission) l

    | Call (w, l) ->
      aux level (permission lor read lor is_function) w;
      List.iter (aux level permission) l

    | Access (Array, a, b) ->  begin
        aux level (permission lor write lor is_array) a;
        aux level (permission lor read) b;
      end

    | UnaryOp(UnOp.PostDecr, a)
    | UnaryOp(UnOp.PreDecr, a)
    | UnaryOp(UnOp.PreIncr, a)
    | UnaryOp(UnOp.PostIncr, a) ->
      aux level (permission lor read lor write) a

    | Access (_, a, _) 
    | UnaryOp(_, a) 
    | Cast (_, a) ->
      aux level (permission lor read) a

    | Default a
    | Label(_, a) ->
      aux level permission a

    | BinaryOp(_, a, b) -> 
      aux level (permission lor read) a;
      aux level (permission lor read) b;

    | Switch (a, b) 
    | While (_, a, b)
    | Case (a, b) -> 
      aux level permission a;
      aux level permission b;

    | Assign (_, a, b) ->
      aux level (permission lor write) a; aux level (permission lor read) b

    | IfThenElse (_, a, b, c) ->
      aux level permission a;
      aux level permission b;
      (match c with Some c -> aux level permission c | _ -> ())

    | Return (Some a) ->
      aux level permission a

    | For (a, b, c, d) ->
      let f x = match x with | None -> () | Some x -> aux (level + 1) permission x in
      f a; f b; f c; aux level permission d

    | Bloc l ->
      List.iter (aux (level + 1) permission) l

    | FunctionDeclaration(_, (name, _, _), _, content) ->
      print_string "fuction\n";
      print_endline (pretty_print_ast program);
      print_endline "";
      (* we should had the name, but laziness is the winner *)
      aux level permission content
    | _ -> ()



  in let _ = List.iter (fun x -> aux 0 0 x) program 
  in tbl
