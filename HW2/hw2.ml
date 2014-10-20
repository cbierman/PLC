let int_of_char ch = 
  match ch with
  | 'a' -> 2
  | 'b' -> 2
  | 'c' -> 2

  | 'd' -> 3
  | 'e' -> 3
  | 'f' -> 3

  | 'g' -> 4
  | 'h' -> 4
  | 'i' -> 4

  | 'j' -> 5
  | 'k' -> 5
  | 'l' -> 5

  | 'm' -> 6
  | 'n' -> 6
  | 'o' -> 6

  | 'p' -> 7
  | 'q' -> 7
  | 'r' -> 7
  | 's' -> 7

  | 't' -> 8
  | 'u' -> 8
  | 'v' -> 8

  | 'w' -> 9
  | 'x' -> 9
  | 'y' -> 9
  | 'z' -> 9

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
  | (a,b) :: tl -> if a = key then (key, value) :: tl 
          else (a,b) :: change key value tl
;;

type ('a, 'b) trie = Node of 'b list * ('a * ('a, 'b) trie) list;;

let words trie = match trie with
| Node (words, _) -> words
;;

let branches trie = match trie with
| Node (_, branches) -> branches
;;

let empty = Node([], []);;

let trie_of_key trie key =
  match trie with
  | Node(a, b) -> try assoc key b with Not_found -> empty
 
let rec find trie keys =
  match keys with
    | [] -> words trie
    | h::tl -> find (trie_of_key trie h) tl
  
let add_word word trie =
  match trie with
    | Node(a,b) -> Node((word :: a), b)
  
;;
  
let replace key branch trie =
  match trie with
    | Node(a,b) -> Node(a, (change key branch b))
;;
  
let rec add keys word trie =
  match keys with
    | [] -> add_word word trie
    | h::tl -> replace h (add tl word (trie_of_key trie h)) trie
;;
  
  
let l = [(0, 0); (1, 1); (2, 2); (3, 3)];;
  
  





