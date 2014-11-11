exception Error

type token = 
  | STAR
  | SQUOTE
  | RPAREN
  | LPAREN
  | INT
  | IDT of (string)
  | EOF
  | COMMA
  | BOOL
  | ARROW

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState20
  | MenhirState19
  | MenhirState18
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState9
  | MenhirState8
  | MenhirState7
  | MenhirState4
  | MenhirState1
  | MenhirState0

  

open OcamlType
  
let _eRR =
  Error

let rec _menhir_run8 : _menhir_env -> 'ttv_tail * _menhir_state * (OcamlType.otype) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IDT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | SQUOTE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (OcamlType.otype) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | SQUOTE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_typexpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (OcamlType.otype) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState7 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BOOL ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | IDT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | SQUOTE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
        | IDT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState7 in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : (OcamlType.otype) =                              ( e ) in
            _menhir_goto_typexpr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STAR ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | ARROW | COMMA | EOF | IDT _ | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (OcamlType.otype) =                                   ( Pair ( e1, e2) ) in
            _menhir_goto_typexpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | IDT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState15 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | STAR ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | STAR ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | COMMA | EOF | IDT _ | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _v : (OcamlType.otype) =                                    ( Arrow ( e1, e2) ) in
            _menhir_goto_typexpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState20 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _v : (OcamlType.otype) =                    ( e ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | IDT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | STAR ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
    | _ ->
        _menhir_fail ()

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | IDT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | SQUOTE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (OcamlType.otype) =        ( Int ) in
    _menhir_goto_typexpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (string) =        ( _1 ) in
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let v = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (OcamlType.otype) =                     ( TVar v ) in
        _menhir_goto_typexpr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 | MenhirState20 | MenhirState4 | MenhirState7 | MenhirState14 | MenhirState15 | MenhirState18 | MenhirState19 | MenhirState16 | MenhirState8 | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let e = _v in
        let _v : (string) =              ( e ) in
        match _menhir_s with
        | MenhirState20 | MenhirState7 | MenhirState15 | MenhirState19 | MenhirState9 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let e2 = _v in
            let (_menhir_stack, _menhir_s, e1) = _menhir_stack in
            let _v : (OcamlType.otype) =                                 ( Type (e2, [e1]) ) in
            _menhir_goto_typexpr _menhir_env _menhir_stack _menhir_s _v
        | MenhirState0 | MenhirState4 | MenhirState14 | MenhirState18 | MenhirState8 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let e = _v in
            let _v : (OcamlType.otype) =                   ( TVar( e ) ) in
            _menhir_goto_typexpr _menhir_env _menhir_stack _menhir_s _v
        | MenhirState16 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let e3 = _v in
            let (((((_menhir_stack, _menhir_s), _, e1), _), _, e2), _) = _menhir_stack in
            let _v : (OcamlType.otype) =                                                                  ( Type ( e3, [e1; e2] ) ) in
            _menhir_goto_typexpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (OcamlType.otype) =         ( Bool ) in
    _menhir_goto_typexpr _menhir_env _menhir_stack _menhir_s _v

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (OcamlType.otype) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SQUOTE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



