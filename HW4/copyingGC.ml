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
 
  let rec copy start size address =
    if start = size 
    then start,free 
    else begin
        ram.(start) <- ram.(address);
        ram.(address) <- FwdPointer (start);
        copy (start + 1) (size) (address + 1)
      end
 
  in                                                                
  let thing = ram.(addr) in 
    match thing with
      | FwdPointer (x) -> free, x
      | Object (a, b, c) -> copy free (b+free) addr
      | _ -> raise Missing

  
  
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
  let first = function (x,y) -> x
  in
  if free = unscanned 
  then free
  else
    let rec apply refs =
      match refs with
       | [] -> ()
       | h :: tl -> scan_tospace (first (copy_obj free h)) (unscanned + 1)
    in
    match ram.(unscanned) with
      | Object(_, _, c) -> 
      | _ -> ()
  
  
  (* Garbage collect with given root set. Copy all reachable objects in
  the From-space to the To-space, update references in objects and
  keep the To-space compact without holes between objects. 
  
  [root_set] is a list of references into the From-space
  
  Return the new addresses in the To-space of the objects in the root
  set.
*)
let copy_gc (root_set : int list) = raise Missing
  
