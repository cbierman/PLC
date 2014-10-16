(* 22c:111 Programming Language Concepts Fall 2014
   Micro-Assignment 2
	
   Authored by Cale Bierman

   ocaml micro2.ml 

*)


let rec insertion x y = 
	match y with 
  		| [] -> [x]                               
  		| h :: tl -> if x < h then x :: y else h :: insertion x tl



let rec insertion_sort x = 
	match x with
		| [] -> []
		| [a] -> [a]
		| h :: tl -> insertion h (insertion_sort tl)


(* Do not change anything below *)

let rec print fmt = function
 | [] -> ()
 | [x] -> Format.fprintf fmt "%d" x
 | x :: l -> Format.fprintf fmt "%d, %a" x print l

let rec is_sorted = function
 | [] | [_] -> true
 | x :: (y :: _ as l) -> x <= y && is_sorted l

let check l =
 let r = insertion_sort l in
 let ok = is_sorted r in
 Format.printf "[%a] => [%a]: %s@."
   print l print r (if ok then "OK" else "FAILED");
 if not ok then exit 1

let () =
 check [1; 2; 3];
 check [3; 2; 1];
 check [];
 check [1];
 check [2; 1; 1]
