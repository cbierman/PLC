open Hw2

let check g s f v = 

  let r, g' = 
    try 
      if Lazy.force f = v then ("passed", succ g) else ("failed", g)
    with 
      | Missing -> ("missing", g)
      | _ -> ("error", g)
  in

  Format.printf "Testing %s: %s@." s r;

  g'

let test_q11 () = 

  let g = 0 in 

  let g = 
    check g
      "int_of_char: chars mapped to right keys"
      (lazy 
	 (List.map
            int_of_char
            ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
             'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']))
      [2; 2; 2; 3; 3; 3; 4; 4; 4; 5; 5; 5; 6; 
       6; 6; 7; 7; 7; 7; 8; 8; 8; 9; 9; 9; 9]
  in

  let g =
    check g
      "int_of_char: raise exception for chars out of range" 
      (lazy (try int_of_char '@' with Not_found -> (-1)))
      (-1)
  in
  
  g

let test_q12 () =

  let g = 0 in

  let g =
    check 
      g
      "intlist_of_string: words mapped to correct keys sequence"
      (lazy
	 (List.map
            intlist_of_string 
            [[]; ['i'; 'n']; ['i'; 'f']; ['h'; 'd']; ['g'; 'e'; 't']; 
             ['t'; 'o']; ['v'; 'o'; 'i'; 'd']; ['u'; 'n'; 'i'; 't']]))
      [[]; [4; 6]; [4; 3]; [4; 3]; [4; 3; 8]; [8; 6]; [8; 6; 4; 3]; 
       [8; 6; 4; 8]]
  in

  g

let test_q21 () = 

  let g = 0 in

  let l = [(0, 0); (1, 1); (2, 2); (3, 3)] in

  let g = 
    check g "assoc: at head of list" (lazy (assoc 0 l)) 0
  in

  let g = 
    check g "assoc: in middle of list" (lazy (assoc 1 l)) 1
  in

  let g =
    check g "assoc: at end of list" (lazy (assoc 3 l)) 3
  in

  let g = 
    check g
      "assoc: raise exception if not found in empty list"
      (lazy (try assoc 0 [] with Not_found -> true)) 
      true
  in

  let g =
    check g "assoc: raise exception if not found in non-empty list" 
      (lazy (try assoc (-1) l with Not_found -> (-1))) (-1)
  in

  g



let test_q22 () = 

  let g = 0 in

  let l = [(0, 0); (1, 1); (2, 2); (3, 3)] in
  
  let g = 
    check g "change: at head of list" 
      (lazy (change 0 4 l)) 
      [(0, 4); (1, 1); (2, 2); (3, 3)] 
  in

  let g = 
    check g "change: in middle of list" 
      (lazy (change 1 4 l)) 
      [(0, 0); (1, 4); (2, 2); (3, 3)]
  in

  let g =
    check g "change: at end of list" 
      (lazy (change 3 4 l)) 
      [(0, 0); (1, 1); (2, 2); (3, 4)]
  in

  let g = 
    check g
      "change: add to empty list"
      (lazy (change 0 0 [])) 
      [(0, 0)]
  in

  let g =
    check g "change: add to non-empty list" 
      (lazy (change 4 4 l))
      [(0, 0); (1, 1); (2, 2); (3, 3); (4, 4)]
  in

  g

let test_q32 () = 

  let g = 0 in 

  let t = Node ([], [(1, Node ([1], [])); (2, Node ([2], []))]) in

  let g = 
    check g "trie_of_key: trie of first key" 
      (lazy (trie_of_key t 1)) 
      (Node ([1], []))
  in
  
  let g = 
    check g "trie_of_key: trie of last key" 
      (lazy (trie_of_key t 2))
      (Node ([2], []))
  in
  
  let g =
    check g "trie_of_key: trie of not existing key" 
      (lazy (trie_of_key t 3))
      (Node ([], []))
  in

  g

let test_q33 () = 

  let g = 0 in 

  let t = 
    Node ([],
	  [(4,
	    Node ([],
		  [(6, Node ([['i'; 'n']], []));
		   (3,
		    Node ([['h'; 'd']; ['i'; 'f']], [(8, Node ([['g'; 'e'; 't']], []))]))]));
	   (8,
	    Node ([],
		  [(6,
		    Node ([['t'; 'o']],
			  [(4,
			    Node ([],
				  [(3, Node ([['v'; 'o'; 'i'; 'd']], []));
				   (8, Node ([['u'; 'n'; 'i'; 't']], []))]))]))]))])
  in

  let g = 
    check g "find: empty key sequence" (lazy (find (Node ([1], [])) [])) [1]
  in
  
  let g = 
    check g "find: not existing key sequence" (lazy (find (Node ([], [])) [1])) []
  in
  
  let g = 
    check g "find: not existing key sequence in full trie" 
      (lazy (find t [1]))
      []
  in
  
  let g =
    check g "find: existing key sequence in full trie" 
      (lazy (find t [4; 6])) [['i'; 'n']]
  in

  let g =
    check g "find: another existing key sequence in full trie" 
      (lazy (find t [4; 3])) [['h'; 'd']; ['i'; 'f']]
  in

  g


let test_q34 () = 

  let g = 0 in 

  let g = 
    check g "add_word: adding word to empty list"
      (lazy (add_word 'a' (Node ([], [])))) 
      (Node (['a'], []))
  in
  
  let g =
    check g "add_word: adding word to non-empty list"
      (lazy (add_word 'a' (Node (['b'; 'c'], [])))) 
      (Node (['a'; 'b'; 'c'], []))
  in

  let g =
    check g "replace: adding branch for not existing key"
      (lazy (replace 4 (Node (['b'; 'c'], [])) (Node ([], [])))) 
      (Node ([], [(4, (Node (['b'; 'c'], [])))]))
  in

  let g =
    check g "replace: replacing branch for existing key"
      (lazy 
	 (replace 
	    4
	    (Node (['d'; 'e'], [])) 
	    (Node ([], [(4, (Node (['b'; 'c'], [])))]))))
      (Node ([], [(4, (Node (['d'; 'e'], [])))]))
  in

  g


let test_q35 () = 

  let g = 0 in 

  let t2 = 
    Node ([],
	  [(4,
	    Node ([],
		  [(6, Node ([['i'; 'n']], []));
		   (3,
		    Node ([['h'; 'd']; ['i'; 'f']], [(8, Node ([['g'; 'e'; 't']], []))]))]));
	   (8,
	    Node ([],
		  [(6,
		    Node ([['t'; 'o']],
			  [(4,
			    Node ([],
				  [(3, Node ([['v'; 'o'; 'i'; 'd']], []));
				   (8, Node ([['u'; 'n'; 'i'; 't']], []))]))]))]))])
  in

  let g = 
    check g "add: rebuild example trie" 
      (lazy 
	 (List.fold_left 
	    (fun t w -> add (intlist_of_string w) w t)
	    (Node ([], []))
	    [['i'; 'n']; ['i'; 'f']; ['h'; 'd']; ['g'; 'e'; 't']; 
	     ['t'; 'o']; ['v'; 'o'; 'i'; 'd']; ['u'; 'n'; 'i'; 't']]))
      t2
  in

  g

;;

let scores = [] in

let scores = ("Q1.1", test_q11 ()) :: scores in

let scores = ("Q1.2", test_q12 ()) :: scores in

let scores = ("Q2.1", test_q21 ())  :: scores in

let scores = ("Q2.2", test_q22 ())  :: scores in

let scores = ("Q3.2", test_q32 ())  :: scores in

let scores = ("Q3.3", test_q33 ())  :: scores in

let scores = ("Q3.4", test_q34 ())  :: scores in

let scores = ("Q3.5", test_q35 ())  :: scores in

let total = 
  List.fold_left 
    (fun a (q, s) -> Format.printf "Score for %s: %d@." q s; a + s)
    0
    (List.rev scores)
in

Format.printf "Total score: %d@." total

