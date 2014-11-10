exception Missing

type otype =
  | Int
  | Bool
  | TVar of string 
  | Arrow of otype * otype 
  | Pair of otype * otype
  | Type of string * otype list
    

val pp_print_otype : Format.formatter -> otype -> unit
