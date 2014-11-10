let main () = 

  (* Standard if no arguments given, otherwise read from file *)
  let input_channel = 
    if Array.length Sys.argv < 2 then stdin else open_in Sys.argv.(1) 
  in

  (* Parse into an AST *)
  let e =
    ExprParser.main
      ExprLexer.token
      (Lexing.from_channel input_channel)
  in

  (* Output AST  *)
  Format.printf "%a@." Expr.pp_print_expr e;

  (* Evaluate and output *)
  Format.printf "%d@." (Expr.eval e)

;;

main ()
      
