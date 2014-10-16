exception Missing

(* ********************************************************************** *)
(* Question 1                                                             *)
(* ********************************************************************** *)

(* 1.1 Return the integer number corresponding to the key of the
   character *)
let int_of_char char = raise Missing

(* 1.2 Return the list of integer numbers corresponing to the keys to
   produce the list of characters *)
let rec intlist_of_string string = raise Missing

(* ********************************************************************** *)
(* Question 2                                                             *)
(* ********************************************************************** *)

(* 2.1 Return the first value mapped to a key. Raise the exception
   Not_found if no mapping is found for the key *)
let rec assoc key dict = raise Missing


(* 2.3 Replace mapping of the key to a trie with a mapping of the key to
   the given trie *)
let rec change key value dict = raise Missing


(* ********************************************************************** *)
(* Question 3                                                             *)
(* ********************************************************************** *)

(* A trie with edges labelled with keys of type 'a and nodes labeled
   with lists of words of type 'b *)
type ('a, 'b) trie = Node of 'b list * ('a * ('a, 'b) trie) list

(* Return the words at the root *)
let words trie = match trie with Node (words, _) -> words

(* Return an association list of edge labels to branches of the root  *)
let branches trie = match trie with Node (_, branches) -> branches

(* 2.1 The empty trie has no outgoing edges and no words at the root *)
(* let empty = *)

(* 2.2 Return the sub-trie corresponding to the key, or the empty trie
   if there is no sub-trie for the key. *)
let trie_of_key trie key = raise Missing

(* 2.3 Find the words mapped to a sequence of keys *)
let rec find trie keys = raise Missing

(* 2.4 Add word to the root *)
let add_word word trie = raise Missing

(* 2.4 Replace mapping of key in the root with a mapping of the key to the given branch *)
let replace key branch trie = raise Missing

(* 2.5 Add a word to the trie *)
let rec add keys word trie = raise Missing
