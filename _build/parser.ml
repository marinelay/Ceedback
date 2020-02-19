type token =
  | INT of (int)
  | VAR of (string)
  | EXAMPLES
  | INTCOMPS
  | INTVARCOMPS
  | ARRVARCOMPS
  | PARTIALPGM
  | FUN
  | SKIP
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | WHILE
  | PLUS
  | MINUS
  | MUL
  | DIV
  | MOD
  | EQUAL
  | GT
  | LT
  | EQUALEQUAL
  | NOT
  | AND
  | OR
  | HOLE
  | RETURN
  | MAPSTO
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | SEMICOLON
  | NONE
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Imp 
# 48 "parser.ml"
let yytransl_const = [|
  259 (* EXAMPLES *);
  260 (* INTCOMPS *);
  261 (* INTVARCOMPS *);
  262 (* ARRVARCOMPS *);
  263 (* PARTIALPGM *);
  264 (* FUN *);
  265 (* SKIP *);
  266 (* TRUE *);
  267 (* FALSE *);
  268 (* IF *);
  269 (* THEN *);
  270 (* ELSE *);
  271 (* WHILE *);
  272 (* PLUS *);
  273 (* MINUS *);
  274 (* MUL *);
  275 (* DIV *);
  276 (* MOD *);
  277 (* EQUAL *);
  278 (* GT *);
  279 (* LT *);
  280 (* EQUALEQUAL *);
  281 (* NOT *);
  282 (* AND *);
  283 (* OR *);
  284 (* HOLE *);
  285 (* RETURN *);
  286 (* MAPSTO *);
  287 (* COMMA *);
  288 (* LPAREN *);
  289 (* RPAREN *);
  290 (* LBRACE *);
  291 (* RBRACE *);
  292 (* LBRACKET *);
  293 (* RBRACKET *);
  294 (* SEMICOLON *);
  295 (* NONE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\007\000\008\000\008\000\009\000\
\010\000\010\000\011\000\011\000\006\000\006\000\004\000\004\000\
\012\000\005\000\005\000\005\000\003\000\014\000\014\000\014\000\
\014\000\015\000\015\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\016\000\016\000\016\000\016\000\
\016\000\000\000"

let yylen = "\002\000\
\011\000\011\000\002\000\001\000\004\000\003\000\001\000\001\000\
\001\000\001\000\003\000\003\000\002\000\001\000\003\000\003\000\
\001\000\001\000\003\000\001\000\007\000\001\000\001\000\003\000\
\001\000\001\000\004\000\001\000\001\000\003\000\003\000\003\000\
\002\000\003\000\003\000\001\000\003\000\002\000\001\000\003\000\
\011\000\007\000\007\000\001\000\001\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\050\000\014\000\000\000\000\000\000\000\
\010\000\000\000\000\000\000\000\009\000\013\000\000\000\000\000\
\000\000\003\000\000\000\000\000\012\000\000\000\011\000\000\000\
\000\000\000\000\008\000\006\000\015\000\000\000\017\000\018\000\
\000\000\000\000\000\000\005\000\000\000\000\000\000\000\000\000\
\000\000\039\000\000\000\000\000\044\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\000\022\000\000\000\023\000\028\000\029\000\
\000\000\000\000\000\000\000\000\000\000\000\000\037\000\000\000\
\000\000\000\000\045\000\046\000\047\000\048\000\049\000\027\000\
\000\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\035\000\034\000\000\000\000\000\001\000\002\000\000\000\000\000\
\000\000\043\000\000\000\000\000\000\000\041\000"

let yydgoto = "\002\000\
\004\000\008\000\025\000\015\000\033\000\060\000\010\000\011\000\
\026\000\012\000\013\000\034\000\046\000\067\000\062\000\081\000\
\068\000"

let yysindex = "\020\000\
\020\255\000\000\005\255\000\000\000\000\028\255\015\255\038\255\
\000\000\005\255\029\255\056\255\000\000\000\000\025\255\249\254\
\064\255\000\000\005\255\005\255\000\000\015\255\000\000\001\255\
\081\255\069\255\000\000\000\000\000\000\073\255\000\000\000\000\
\080\255\078\255\015\255\000\000\066\255\001\255\106\255\002\255\
\077\255\000\000\083\255\091\255\000\000\013\255\109\255\000\000\
\001\255\001\255\024\255\063\255\063\255\125\255\066\255\024\255\
\135\255\138\255\000\000\000\000\250\254\000\000\000\000\000\000\
\063\255\000\000\133\255\132\255\134\255\093\255\000\000\164\255\
\001\255\001\255\000\000\000\000\000\000\000\000\000\000\000\000\
\024\255\000\000\024\255\024\255\024\255\063\255\063\255\104\255\
\108\255\000\000\140\000\146\000\164\255\164\255\164\255\164\255\
\000\000\000\000\066\255\066\255\000\000\000\000\018\255\023\255\
\140\255\000\000\130\255\066\255\054\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\159\255\000\000\145\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\255\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\079\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\041\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\102\255\000\000\000\000\000\000\000\000\000\000\048\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\110\255\136\255\144\255\146\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\055\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\158\000\000\000\251\255\226\255\047\000\000\000\154\000\
\000\000\157\000\000\000\000\000\203\255\209\255\219\255\000\000\
\218\255"

let yytablesize = 287
let yytable = "\047\000\
\020\000\071\000\031\000\061\000\016\000\005\000\050\000\048\000\
\072\000\075\000\076\000\077\000\078\000\079\000\069\000\005\000\
\029\000\047\000\057\000\058\000\001\000\006\000\003\000\022\000\
\005\000\041\000\082\000\023\000\014\000\039\000\080\000\006\000\
\022\000\093\000\016\000\094\000\095\000\096\000\007\000\032\000\
\006\000\054\000\091\000\092\000\017\000\103\000\104\000\097\000\
\098\000\009\000\055\000\059\000\105\000\016\000\109\000\055\000\
\009\000\106\000\019\000\021\000\055\000\047\000\047\000\005\000\
\041\000\009\000\009\000\041\000\030\000\038\000\047\000\024\000\
\063\000\064\000\042\000\038\000\040\000\043\000\038\000\006\000\
\044\000\040\000\040\000\042\000\035\000\040\000\020\000\065\000\
\110\000\042\000\066\000\055\000\042\000\045\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\022\000\
\026\000\026\000\036\000\026\000\038\000\037\000\049\000\026\000\
\051\000\026\000\052\000\026\000\026\000\025\000\025\000\025\000\
\025\000\025\000\053\000\025\000\025\000\025\000\070\000\036\000\
\036\000\056\000\090\000\024\000\024\000\024\000\036\000\024\000\
\024\000\099\000\024\000\101\000\073\000\100\000\024\000\074\000\
\024\000\102\000\024\000\024\000\075\000\076\000\077\000\078\000\
\079\000\107\000\083\000\084\000\085\000\086\000\087\000\086\000\
\087\000\030\000\030\000\108\000\088\000\004\000\089\000\018\000\
\030\000\031\000\031\000\032\000\032\000\028\000\007\000\027\000\
\031\000\000\000\032\000\075\000\076\000\077\000\078\000\079\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000"

let yycheck = "\037\000\
\000\000\055\000\002\001\051\000\005\001\001\001\005\001\038\000\
\056\000\016\001\017\001\018\001\019\001\020\001\053\000\001\001\
\022\000\055\000\049\000\050\000\001\000\017\001\003\001\031\001\
\001\001\002\001\065\000\035\001\001\001\035\000\037\001\017\001\
\031\001\081\000\035\001\083\000\084\000\085\000\034\001\039\001\
\017\001\029\001\073\000\074\000\007\001\099\000\100\000\086\000\
\087\000\003\000\038\001\028\001\035\001\007\000\108\000\038\001\
\010\000\035\001\030\001\035\001\038\001\099\000\100\000\001\001\
\002\001\019\000\020\000\002\001\022\000\029\001\108\000\008\001\
\010\001\011\001\009\001\035\001\029\001\012\001\038\001\017\001\
\015\001\035\000\035\001\029\001\004\001\038\001\031\001\025\001\
\035\001\035\001\028\001\038\001\038\001\028\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\031\001\
\026\001\027\001\038\001\029\001\031\001\030\001\005\001\033\001\
\036\001\035\001\032\001\037\001\038\001\016\001\017\001\018\001\
\019\001\020\001\032\001\022\001\023\001\024\001\002\001\026\001\
\027\001\021\001\038\001\022\001\023\001\024\001\033\001\026\001\
\027\001\034\001\029\001\000\000\006\001\034\001\033\001\006\001\
\035\001\000\000\037\001\038\001\016\001\017\001\018\001\019\001\
\020\001\014\001\022\001\023\001\024\001\026\001\027\001\026\001\
\027\001\026\001\027\001\034\001\033\001\007\001\033\001\010\000\
\033\001\026\001\027\001\026\001\027\001\020\000\030\001\019\000\
\033\001\255\255\033\001\016\001\017\001\018\001\019\001\020\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\030\001"

let yynames_const = "\
  EXAMPLES\000\
  INTCOMPS\000\
  INTVARCOMPS\000\
  ARRVARCOMPS\000\
  PARTIALPGM\000\
  FUN\000\
  SKIP\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  MOD\000\
  EQUAL\000\
  GT\000\
  LT\000\
  EQUALEQUAL\000\
  NOT\000\
  AND\000\
  OR\000\
  HOLE\000\
  RETURN\000\
  MAPSTO\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  SEMICOLON\000\
  NONE\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : 'examples) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'pgm) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'integers) in
    let _8 = (Parsing.peek_val __caml_parser_env 3 : 'variables) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'variables) in
    Obj.repr(
# 58 "parser.mly"
                                                                                                       (_2, _4, _6, _8, _10)
# 305 "parser.ml"
               : Imp.example list * Imp.prog * int list * Imp.var list * Imp.var list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : 'examples) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'pgm) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'integer) in
    let _8 = (Parsing.peek_val __caml_parser_env 3 : 'variables) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'variables) in
    Obj.repr(
# 59 "parser.mly"
                                                                                                      (_2, _4, [_6], _8, _10)
# 316 "parser.ml"
               : Imp.example list * Imp.prog * int list * Imp.var list * Imp.var list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'example) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'examples) in
    Obj.repr(
# 63 "parser.mly"
                     (_1::_2)
# 324 "parser.ml"
               : 'examples))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'example) in
    Obj.repr(
# 64 "parser.mly"
            ([_1])
# 331 "parser.ml"
               : 'examples))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'input) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'output) in
    Obj.repr(
# 68 "parser.mly"
                                  ((_1,_3))
# 339 "parser.ml"
               : 'example))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'input) in
    Obj.repr(
# 72 "parser.mly"
                      (_1::_3)
# 347 "parser.ml"
               : 'input))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 73 "parser.mly"
          ([_1])
# 354 "parser.ml"
               : 'input))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 77 "parser.mly"
          (_1)
# 361 "parser.ml"
               : 'output))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arr) in
    Obj.repr(
# 81 "parser.mly"
        (Imp.VArr (_1))
# 368 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'integer) in
    Obj.repr(
# 82 "parser.mly"
            (Imp.VInt (_1))
# 375 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'integer) in
    Obj.repr(
# 86 "parser.mly"
                          ([_2])
# 382 "parser.ml"
               : 'arr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'integers) in
    Obj.repr(
# 87 "parser.mly"
                           (_2)
# 389 "parser.ml"
               : 'arr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
              ((_2*(-1)))
# 396 "parser.ml"
               : 'integer))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 92 "parser.mly"
        ((_1))
# 403 "parser.ml"
               : 'integer))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'integer) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'integers) in
    Obj.repr(
# 96 "parser.mly"
                           (_1::_3)
# 411 "parser.ml"
               : 'integers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'integer) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'integer) in
    Obj.repr(
# 97 "parser.mly"
                          ([_1;_3])
# 419 "parser.ml"
               : 'integers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "parser.mly"
        ((_1))
# 426 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
         ([])
# 432 "parser.ml"
               : 'variables))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'variables) in
    Obj.repr(
# 106 "parser.mly"
                             (_1::_3)
# 440 "parser.ml"
               : 'variables))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 107 "parser.mly"
             ([_1])
# 447 "parser.ml"
               : 'variables))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'variables) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'cmd) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 111 "parser.mly"
                                                  ((_2,_4,_6))
# 456 "parser.ml"
               : 'pgm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'integer) in
    Obj.repr(
# 115 "parser.mly"
            (Imp.Int _1)
# 463 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lv) in
    Obj.repr(
# 116 "parser.mly"
       (Imp.Lv _1)
# 470 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 117 "parser.mly"
                  (Imp.BinOpLv (_2,_1,_3))
# 479 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
         (Imp.ahole ())
# 485 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
        (Imp.Var _1)
# 492 "parser.ml"
               : 'lv))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aexp) in
    Obj.repr(
# 123 "parser.mly"
                               (Imp.Arr (_1,_3))
# 500 "parser.ml"
               : 'lv))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
         (Imp.True)
# 506 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
          (Imp.False)
# 512 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 128 "parser.mly"
                 (Imp.Gt (_1,_3))
# 520 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 129 "parser.mly"
                 (Imp.Lt (_1,_3))
# 528 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 130 "parser.mly"
                         (Imp.Eq (_1,_3))
# 536 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 131 "parser.mly"
             (Imp.Not _2)
# 543 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 132 "parser.mly"
                 (Imp.Or (_1,_3))
# 551 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 133 "parser.mly"
                  (Imp.And (_1,_3))
# 559 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "parser.mly"
         (Imp.bhole ())
# 565 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 138 "parser.mly"
                      (Imp.Seq (_1, _3))
# 573 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 139 "parser.mly"
                  (_1)
# 580 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
         (Imp.Skip)
# 586 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lv) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 141 "parser.mly"
                  (Imp.Assign (_1, _3))
# 594 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'bexp) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'cmd) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 142 "parser.mly"
                                                                   (Imp.If (_3,_6,_10))
# 603 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bexp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 143 "parser.mly"
                                            (Imp.If (_3,_6,Imp.Skip))
# 611 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bexp) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 144 "parser.mly"
                                               (Imp.While (_3,_6))
# 619 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "parser.mly"
         (Imp.chole ())
# 625 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "parser.mly"
         (Imp.Plus)
# 631 "parser.ml"
               : 'bop))
; (fun __caml_parser_env ->
    Obj.repr(
# 150 "parser.mly"
          (Imp.Minus)
# 637 "parser.ml"
               : 'bop))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "parser.mly"
        (Imp.Mult)
# 643 "parser.ml"
               : 'bop))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
        (Imp.Div)
# 649 "parser.ml"
               : 'bop))
; (fun __caml_parser_env ->
    Obj.repr(
# 153 "parser.mly"
        (Imp.Mod)
# 655 "parser.ml"
               : 'bop))
(* Entry resource *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let resource (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Imp.example list * Imp.prog * int list * Imp.var list * Imp.var list)
;;
