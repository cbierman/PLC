open Memory
  
  (* Copy an object to the To-space or return its address. Modify the
  object in the From-space to contain a forwarding pointer to the new
  address. Copy the whole object, that is, the number of memory cells
  determined by the size of the object.
  
  [free] is the pointer to the next free address in the To-space,
  [addr] is the address of the object to copy. 
  
  Return a pair of addresses, the first is the updated [free]
  pointer, the second is the new address of the object. 

*)

let copy_obj (free : int) (addr : int) =
 
  let rec copy start size address n =
    if start = n
    then start,(start-size)
    else 
      begin
        ram.(start) <- ram.(address);
        ram.(address) <- FwdPointer (start);
        copy (start + 1) (size) (address + 1) (n)
      end
  in                                                                
  match ram.(addr) with
   | FwdPointer (x) -> free, x
   | Object (a, b, c) -> copy free b addr (b+free)
   | _ -> raise (Failure ("Der Versuch, eine freie oder Objektdaten Zelle kopieren ist nicht erlaubt."))

  
  
  (* Scan To-space, copy all referenced objects to the To-space and
  update references in objects. Recurse until the free pointer is
  identical to the unscanned pointer.
  
  [free] is the pointer to the next free address in the To-space,
  [unscanned] is the address of the first unscanned object in the
  To-space.
  
  Return the address of the free pointer after all objects have been
  scanned.

*)
let rec scan_tospace (free : int) (unscanned : int) = 
  let first = function (x,y) -> x in
  if free = unscanned 
  then free
  else
    (* Creates a list of tuples (freevalue, newaddress) *)
    let rec apply refs free_this acc = 
      match refs with
        | [] -> (free_this, acc)
        | h::tl -> let (x,y) = (copy_obj free_this h) in apply tl x (y::acc)
    in
 (* function taking a list of tupels and removing the first tuple item *)
    (*let rec flattify list =
       match list with
         | [] -> []
         | (a,b)::tl -> b :: flattify tl
    in    *)  
    match ram.(unscanned) with
      | Object(a,b,c) -> (*(let a1 = (apply c free) in 
            let a2 = (try (first (List.hd a1)) with Failure (x) -> free) and *) let (a1,b1) = (apply c free []) in ram.(unscanned) <- Object (a, b, (List.rev b1));
                                                                   scan_tospace (a1) (unscanned + 1)
      | _ -> scan_tospace free (unscanned + 1)


  (* Garbage collect with given root set. Copy all reachable objects in
  the From-space to the To-space, update references in objects and
  keep the To-space compact without holes between objects. 
  
  [root_set] is a list of references into the From-space
  
  Return the new addresses in the To-space of the objects in the root
  set.
*)
let copy_gc (root_set : int list) = 
  let rec starting list free_ptr acc =
     match list with
       | [] -> (free_ptr,acc)
       | h :: tl -> let (a,b) = (copy_obj free_ptr h) in starting tl a (b::acc)
  in
  let free = ram_size/2 in
  let (a1,b1) = starting root_set free []
  and unscanned = ram_size/2 in
  ((List.rev b1), scan_tospace a1 unscanned)

  

