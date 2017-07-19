open Ast

type iterator_uuid =
| ItStart
| ItStop
| ItUuid of int

let one = Constant(CInt(Dec, Num.num_of_int 1, ""))
let zero = Constant(CInt(Dec, Num.num_of_int 0, ""))

let unique_list l =
  List.sort_uniq Pervasives.compare  l
