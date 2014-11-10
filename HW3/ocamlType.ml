exception Missing

type otype =
  | Int
  | Bool
  | TVar of string 
  | Arrow of otype * otype 
  | Pair of otype * otype
  | Type of string * otype list


let rec pp_print_otype ppf = function

  | Int -> Format.fprintf ppf "int"

  | Bool -> Format.fprintf ppf "bool"

  | TVar v -> Format.fprintf ppf "%s" v

  | Arrow (l, r) ->
    Format.fprintf ppf "@[<hv 2>(%a ->@ %a)@]" pp_print_otype l pp_print_otype r

  | Pair (l, r) ->
    Format.fprintf ppf "@[<hv 2>(%a *@ %a)@]" pp_print_otype l pp_print_otype r

  | Type (v, []) ->
    Format.fprintf ppf "@[<hv 2>%s@]" v

  | Type (v, [t]) ->
    Format.fprintf ppf "@[<hv 2>(%a %s)@]" pp_print_otype t v

  | Type (v, t) ->
    Format.fprintf ppf "@[<hv 2>((%a) %s)@]" pp_print_otype_list t v

and pp_print_otype_list ppf = function

  | [] -> ()

  | [t] -> Format.fprintf ppf "%a" pp_print_otype t

  | h :: tl ->
    Format.fprintf ppf "%a,@ %a"pp_print_otype h pp_print_otype_list tl

and pp_print_otype_tuple ppf = function

  | [] -> ()

  | [t] -> Format.fprintf ppf "%a" pp_print_otype t

  | h :: tl ->
    Format.fprintf ppf "%a *@ %a"pp_print_otype h pp_print_otype_tuple tl
