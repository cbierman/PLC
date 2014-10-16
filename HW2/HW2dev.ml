let int_of_char ch = 
	match ch with
	| "a" -> 2
	| "b" -> 2
	| "c" -> 2

	| "d" -> 3
	| "e" -> 3
	| "f" -> 3

	| "g" -> 4
	| "h" -> 4
	| "i" -> 4

	| "j" -> 5
	| "k" -> 5
	| "l" -> 5

	| "m" -> 6
	| "n" -> 6
	| "o" -> 6

	| "p" -> 7
	| "q" -> 7
	| "r" -> 7
	| "s" -> 7

	| "t" -> 8
	| "u" -> 8
	| "v" -> 8

	| "w" -> 9
	| "x" -> 9
	| "y" -> 9
	| "z" -> 9

	| _ -> raise Not_found
;;

let rec intlist_of_string string = 
	match string with
	| [] -> []
	| h :: tl -> (int_of_char h) :: intlist_of_string tl
;;

let rec assoc key dict = 
	match dict with
	| [] -> raise Not_found
	| (a,b) :: tl -> if a = key then b else assoc key tl
;;


let rec change key value dict = 
	match dict with
	| [] -> (key, value) :: dict
	| (a,b) :: tl -> if a = key then (*change value found*) (a, value) :: dict 
					else (*continue search*) (a,b) :: change key value tl
;;



