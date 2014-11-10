(* Type of expressions *)
type expr = 
  | Int of int
  | Var of string
  | Plus of expr * expr
  | Times of expr * expr
  | Let of string * expr * expr


(* Evaluate expression with environment in an association list *)
let rec eval env = function 

  | Int i -> i

  | Plus (x, y) -> 

    eval env x + eval env y

  | Times (x, y) -> 

    eval env x * eval env y

  | Var x -> List.assoc x env

  | Let (x, e1, e2) -> eval ((x, eval env e1) :: env) e2


(* Evaluate with an empty environment *)
let eval = eval []


(* Pretty-printer for expressions *)
let rec pp_print_expr ppf = function

  | Int i -> Format.fprintf ppf "%d" i

  | Var v -> Format.fprintf ppf "%s" v

  | Plus (e1, e2) -> 
    Format.fprintf ppf "@[<hv 1>(%a +@ %a)@]" pp_print_expr e1 pp_print_expr e2

  | Times (e1, e2) -> 
    Format.fprintf ppf "@[<hv 1>(%a *@ %a)@]" pp_print_expr e1 pp_print_expr e2

  | Let (v, e1, e2) -> 
    Format.fprintf 
      ppf
      "@[<hv 1>let %s = %a in@ %a@;<1 -1>end@]" 
      v
      pp_print_expr e1
      pp_print_expr e2

