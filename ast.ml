

type number_base = 
    | Hex
    | Dec
    | Oct


type exponent = 
    | Exponent of char * int * int
type number = 
    | Int of  number_base * Num.num * string
    | Float of  number_base * Num.num * Num.num * exponent option * string (* base du flottant, entiere, decimal, exposant, flags*)
type ast = 
    | Identifier of string
    | Const of number


let print_cst x = match x with
    | Int(Hex, num, s) ->
        Printf.printf "(hex, %s, %s)" (Num.string_of_num num) s
    | Int(Oct, num, s) ->
        Printf.printf "(oct, %s, %s)" (Num.string_of_num num) s
    | Int(Dec, num, s) ->
        Printf.printf "(dec, %s, %s)" (Num.string_of_num num) s
    | _ -> ()
