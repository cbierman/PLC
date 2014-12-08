
(* Code not implemented yet *)
exception Missing
  
(* Content of a memory cell *)
type cell =

  (* Cell is free *)
  | Free

  (* Forward pointer to new location of object *)
  | FwdPointer of int

  (* Object with uniqe identifier, size and list of references *)
  | Object of int * int * int list

  (* Cell is part of an object *)
  | ObjData of int


(* Total size of memory *)
let ram_size = 64

(* Initialize memory *)
let ram = Array.make ram_size Free


(* ********************************************************************** *)
(* Pretty-printing functions                                              *)
(* ********************************************************************** *)


(* Number of digits to display positive integer *)
let digits_of_int d = int_of_float (log10 (float_of_int (abs d))) + 1

(* Pretty-print a list of references *)
let rec pp_print_refs ppf = function
  | [] -> ()
  | [r] -> Format.fprintf ppf "%d" r
  | h :: tl -> Format.fprintf ppf "%d,@ " h; pp_print_refs ppf tl

(* Pretty-print a cell *)
let pp_print_cell ppf = function
  | Free -> Format.fprintf ppf "Free"
  | FwdPointer addr -> Format.fprintf ppf "FwdPointer %d" addr
  | Object (uid, size, refs) -> Format.fprintf ppf "@[<hv>Object@ (%d, %d,@ [@[<hov 1>%a@]])@]" uid size pp_print_refs refs
  | ObjData d -> Format.fprintf ppf "ObjData %d" d

(* Pretty-print the memory contents *)
let rec pp_print_ram' ppf limit addr =
  if addr >= limit then () else
    (Format.fprintf ppf "%*d: %a@," (digits_of_int ram_size) addr pp_print_cell ram.(addr);
     pp_print_ram' ppf limit (succ addr))

(* Pretty-print the memory *)
let pp_print_ram ppf =

  Format.fprintf
    ppf
    "@[<v>From-space:@,%t@,To space:@,%t@]"
    (function ppf -> pp_print_ram' ppf (ram_size / 2) 0)
    (function ppf -> pp_print_ram' ppf ram_size (ram_size / 2))

(* Pretty-print the memory to the standard output *)
let print_ram () = pp_print_ram Format.std_formatter

