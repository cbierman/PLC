module T = OcamlType
module L = OcamlLexer
module P = OcamlParser


let verbose = ref false

let tests =
  [("int", 
    Some T.Int);
   ("bool", 
    Some T.Bool);
   ("otype", 
    Some (T.Type ("otype", [])));
   ("(int)", 
    Some T.Int);
   ("a5", 
    Some  (T.Type ("a5", [])));
   ("\'a", 
    Some (T.TVar "'a"));
   ("int -> bool", 
    Some (T.Arrow (T.Int, T.Bool)));
   ("int -> 'a list -> bool", 
    Some 
      (T.Arrow
         (T.Int, 
          T.Arrow
            (T.Type ("list", [T.TVar "'a"]),
             T.Bool))));
   ("int * bool",
    Some (T.Pair (T.Int, T.Bool)));
   ("('a, 'b) trie", 
    Some
      (T.Type
         ("trie",
          [T.TVar "'a"; T.TVar "'b"] )));
   ("int -> int -> int",
    Some (T.Arrow (T.Int, T.Arrow (T.Int, T.Int))));
   ("int -> bool list",
   Some (T.Arrow (T.Int, T.Type ("list", [T.Bool]))));
   ("'a * 'b trie", 
    Some (T.Pair (T.TVar "\'a", T.Type ("trie", [T.TVar "\'b"]))));
   ("int -> int * bool",
    Some (T.Arrow (T.Int, T.Pair (T.Int, T.Bool))));
   ("* a", None);
   ("c ->", None);
   ("('a, 'b)", None);
   ("list 'a", None);
   ("a *-> b", None);
   ("(a", None);
   ("+", None);
   ("5", None);
   ("()", None)
  ]
  


let main () = 

  if Array.length Sys.argv > 1 then
    verbose := Sys.argv.(1) = "-v" || Sys.argv.(1) = "--verbose"; 
  
  let score =
    List.fold_left
      (fun a (s, t) ->
         if !verbose then Format.printf "@[<v>Testing \"%s\".@," s; 
         let r = match t with
           | None -> 
             (try let t' = P.main L.token (Lexing.from_string s) in
                if !verbose then 
                  Format.printf 
                    "Parsed as %a.@,Wrong (expecting: failed).@]@." 
                    T.pp_print_otype t';
                false
              with 
                | T.Missing -> 
                  if !verbose then
                    Format.printf "Missing.@]@."; false
                | _ -> 
                  if !verbose then
                    Format.printf "Parsing failed.@,Correct.@]@.";
                  true)
           | Some t -> 
             (try
                let t' = P.main L.token (Lexing.from_string s) in
                if !verbose then 
                  Format.printf "Parsed as %a.@,%t@]@." 
                    T.pp_print_otype t'
                    (function ppf -> 
                      (if t' = t then 
                         Format.fprintf ppf "Correct." 
                       else
                         Format.fprintf ppf "Wrong (expecting: %a)." T.pp_print_otype t));
                t' = t
              with _ ->                 
                if !verbose then Format.printf "Parsing failed.@,Wrong.@]@.";
                false)
         in
         if r then succ a else a)
      0
      tests
  in
  
  Format.printf "Correctly recognized %d/%d@." score (List.length tests) 

;;

main ()
      
 
