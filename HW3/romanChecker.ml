

let verbose = ref false
    
let units_of_int i = 
  [| ""; "I"; "II";"III";"IV";"V";"VI";"VII";"VIII";"IX"|].(i)

let tens_of_int i =
  [| ""; "X"; "XX";"XXX";"XL";"L";"LX";"LXX";"LXXX";"XC"|].(i)

let hundreds_of_int i =
  [| ""; "C"; "CC";"CCC";"CD";"D";"DC";"DCC";"DCCC";"CM"|].(i)

let thousands_of_int i = String.make i 'M'  
    
let test_neg = 
  [ "IIV"; "IIX"; "IL"; "IC"; "ID"; "IM";
    "VX"; "VL"; "VC"; "VD"; "VM";
    "XD"; "XM";
    "IIII"; "VIIII"; 
    "VV";
    "XXXX";
    "LL";
    "CCCC";
    "DD"]
  
let roman_of_int i = 
  
  let mod_split m i = i mod m, i / m in

  let units, units_rest = mod_split 10 i in
  let tens, tens_rest = mod_split 10 units_rest in
  let hundreds, thousands = mod_split 10 tens_rest in
  
  Format.sprintf 
    "%s%s%s%s"
    (thousands_of_int thousands)
    (hundreds_of_int hundreds)
    (tens_of_int tens)
    (units_of_int units)

let int_of_roman s = 
  RomanParser.roman RomanLexer.token (Lexing.from_string s) 

let main () = 

  if Array.length Sys.argv > 1 then
    verbose := Sys.argv.(1) = "-v" || Sys.argv.(1) = "--verbose"; 
  

  let score_neg = 
    List.fold_left 
      (fun a s -> 
         let r = 
           try 
             if !verbose then 
               Format.printf
                 "Testing %s. " s;
             let i' = int_of_roman s in 
             (if !verbose then 
                Format.printf 
                  "Wrong (parsed as %d, expecting failed).@." 
                  i');
             false 
           with 
             | OcamlType.Missing -> 
               if !verbose then 
                 Format.printf 
                   "Missing.@.";
               false
             | _ -> 
               if !verbose then 
                 Format.printf 
                   "Correct (not valid).@.";
               true
         in
         if r then succ a else a)
      0
      test_neg
  in

  let rec aux a i = if i > 2000 then a else
      (let a' =
        try
          if !verbose then
            Format.printf
              "Testing %s (%d). " (roman_of_int i) i;
          let i' = int_of_roman (roman_of_int i) in 
          if (i' = i) then 
            (if !verbose then Format.printf "Correct.@."; succ a) 
          else
            (if !verbose then
               Format.printf
                 "Wrong (evaluated to%d)@." i';
             a)
        with
             | OcamlType.Missing -> 
               if !verbose then 
                 Format.printf 
                   "Missing.@.";
               a
             | _ ->
               if !verbose then
                 Format.printf
                   "Wrong (failed)@.";
               a
       in
       aux a' (succ i))
  in

  let score_pos = aux 0 0 in

  Format.printf
    "Correctly recognized %d/%d@."
    (score_pos + score_neg)
    (2001 + List.length test_neg)

;;

main ()
      
 
