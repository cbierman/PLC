(* Cale Bierman *)
(* Programming Language Concepts *)
(* 10/20/14 *)
(* Homework 2 *)

(* Q1.1 Maps character to integer (phone keys) *)
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

(* Q1.2 Returns and integer list from the given string *)
let rec intlist_of_string string = 
  match string with
  | [] -> []
  | h :: tl -> (int_of_char h) :: intlist_of_string tl
;;

(* Q 2.1 Returns the value in a (key, value) pair *)
let rec assoc key dict = 
  match dict with
  | [] -> raise Not_found
  | (a,b) :: tl -> if a = key then b else assoc key tl
;;

(* Question 2.2: My function is tail recursive because as soon as it encounters the
   key, it just returns the value. It wouldn't be tail recursive if the recursive
   calls higher in the stack used the result of the ones lower in the stack *)

(* Q 2.3: Returns an association list with a binding of a new key to a new value *)
let rec change key value dict = 
  match dict with
  | [] -> (key, value) :: dict
  | (a,b) :: tl -> if a = key then (key, value) :: tl 
          else (a,b) :: change key value tl
;;

type ('a, 'b) trie = Node of 'b list * ('a * ('a, 'b) trie) list;;

(* Returns list of words stored in a trie node *)
let words trie = match trie with
| Node (words, _) -> words
;;
(* Returns branches of a trie node *)
let branches trie = match trie with
| Node (_, branches) -> branches
;;

(* Q 3.1 the empty trie *)
let empty = Node([], []);;

(*  Q 3.2 Returns a trie from a given key *)
let trie_of_key trie key =
  match trie with
  | Node(a, b) -> try assoc key b with Not_found -> empty

(* q 3.3 Returns the word list by following the edges of the trie *)
let rec find trie keys =
  match keys with
    | [] -> words trie
    | h::tl -> find (trie_of_key trie h) tl

(* Q 3.4 Adds a word to a trie node *)
let add_word word trie =
  match trie with
    | Node(a,b) -> Node((word :: a), b)
  
;;

(* Q 3.4 Replaces the mapping of the key to it's subtree by branch *)
let replace key branch trie =
  match trie with
    | Node(a,b) -> Node(a, (change key branch b))
;;

(* Q 3.5 Adds a word to a trie dictionary *)
let rec add keys word trie =
  match keys with
    | [] -> add_word word trie
    | h::tl -> replace h (add tl word (trie_of_key trie h)) trie
;;
  
  





