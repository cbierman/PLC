open Memory
  
(* Check if objects at each pair of addresses are identical *)
let rec verify_object first free reachable heap_in heap_out visited = 

  function 

    (* Nothing else to check *)
    | [] ->

      (* No gaps in heap: size must be exactly size of reached objects *)
      if not (first + reachable = free) then
        (Format.printf
           "Failure: heap is not compact: \
            reaching %d words, but allocated %d."
           reachable
           (free - first);
         false)
      else
        true

    (* Pair already checked, break cylces *)
    | (addr_in, addr_out) :: tl when List.mem (addr_in, addr_out) visited ->

      verify_object first free reachable heap_in heap_out visited tl
      
    (* Check first pair *)
    | (addr_in, addr_out) :: tl -> 

      Format.printf "Checking addresses %d and %d@." addr_in addr_out;

      (* Fail if allocated at or after free pointer *)
      if addr_out >= free then
        (Format.printf "Failure: allocated in free space at %d." addr_out;
         false)
      else 

        (* Get object from heap *)
        match heap_in.(addr_in), heap_out.(addr_out) with 

          (* Both cells contain objects *)
          | Object (uid1, size1, refs1), Object (uid2, size2, refs2) -> 

            (* Fail if uids are different *)
            if not (uid1 = uid2) then
              (Format.printf "Failure: object references not identical. Expected: %d, found %d." uid1 uid2;
               false)
            else

              (* Fail if sizes are different *)
            if not (size1 = size2) then
              (Format.printf  "Failure: object sizes not identical. Expected: %d, found %d." size1 size2;
               false)
            else 

              (* Need to check all references *)
              let tl' = 
                List.fold_left2
                  (fun accum a1 a2 -> (a1, a2) :: accum)
                  tl
                  refs1
                  refs2
              in

              (* Need to check contents of memory cells belonging to object *)
              let tl'' = 
                let rec aux accum i = 
                  if i < 1 then accum else
                    aux ((addr_in + i, addr_out + i) :: accum) (pred i)
                in
                aux tl' (size1 - 1)
              in

              (Format.printf "Passed.@.";

               (* Check if referenced objects are identical *)
               verify_object
                 first
                 free
                 (succ reachable)
                 heap_in
                 heap_out
                 ((addr_in, addr_out) :: visited)
                 tl'')
                 
          (* Both cells contain data of objects *)
          | ObjData uid1, ObjData uid2 ->

            (* Fail if uids are different *)
            if not (uid1 = uid2) then
              (Format.printf "Failure: object references not identical. Expected: %d, found %d." uid1 uid2;
               false)
            else

              (Format.printf "Passed.@.";

               (* Check tail of stack *)
               verify_object
                 first
                 free
                 (succ reachable)
                 heap_in
                 heap_out
                 ((addr_in, addr_out) :: visited)
                 tl)

          | _, Free ->

            (Format.printf "Failure: free space in heap at %d." addr_out;
             false)

          | _, FwdPointer _ ->

            (Format.printf "Failure: forwarding pointer in heap at %d." addr_out;
             false)

          | _ ->

            (Format.printf "Failure: invalid reference in heap at %d." addr_out;
             false)


let verify_heap first free heap_in heap_out root_set_in root_set_out = 

  verify_object
    first 
    free
    0
    heap_in 
    heap_out
    []
    (List.combine root_set_in root_set_out)

;;

let mem1 = 
    [| Object (1, 2, [3;4]);
       ObjData 1;
       Free;
       Object (2, 1, []);
       Object (3, 1, []);
       Free;
       Free;       
       Free;       
       Free;       
       Free |];;

let mem2 = 
    [| Object (1, 2, [3;11]);
       ObjData 1;
       Object (42, 1, [6]);
       Object (2, 1, [11;15]);
       Object (3, 2, [7]);
       ObjData 0;
       Object (43, 1, [2]);
       Object (4, 3, []);
       ObjData 0;
       ObjData 0;
       Free;
       Object (5, 3, [15;19;22]);
       ObjData 7;
       ObjData 9;
       Free;
       Object (6, 1, []);
       Free;
       Free;
       Object (7, 1, []);
       Object (8, 2, []);
       ObjData 3;
       Object (9, 1, []);
       Object (10, 1, []);
       Free;
       Object (11, 3, [29]);
       ObjData 42;
       ObjData 43;
       Free;
       Free;
       Object (12, 1, []);
       Object (13, 2, []);
       ObjData 0;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free;
       Free |];;

Array.blit mem2 0 ram 0 31;;

let root_set = [0; 2] in

try
  
  Format.printf "Memory before garbage collection:@.@.%t@." pp_print_ram;

  let root_set', free = CopyingGC.copy_gc root_set in

  Format.printf "Memory after garbage collection:@.@.%t@." pp_print_ram;
  
  verify_heap 32 free mem2 ram root_set root_set'

with

  | Missing ->
    Format.printf "Missing implementation.@."; exit 2
  
  | e ->
    Format.printf "Runtime error.@.%s@." (Printexc.to_string e); exit 2
  
