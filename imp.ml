open Vocab

type bop = Plus | Minus | Mult | Div | Mod
and var = string

and aexp = 
   | Int of int
   | Lv of lv
   | BinOpLv of bop * aexp * aexp
   | AHole of int

and lv =
   | Var of var
   | Arr of var * aexp (* 원랜 var * var 였는데 var * aexp 가 맞는듯 일단*)

and bexp = 
   | True
   | False
   | Gt of aexp * aexp (* > *)
   | Lt of aexp * aexp (* < *)
   | Eq of aexp * aexp (* == *)
   | Not of bexp
   | Or of bexp * bexp
   | And of bexp * bexp
   | BHole of int

and cmd = 
   | Assign of lv * aexp 
   | Skip
   | Seq of cmd * cmd
   | If of bexp * cmd * cmd
   | While of bexp * cmd      
   | CHole of int

type value = 
  | VInt of int 
  | VArr of int list

let rec ts_aexp : aexp -> string
= fun aexp -> 
  match aexp with
  | Int n -> string_of_int n
  | Lv lv -> ts_lv lv
  | BinOpLv (bop,lv1,lv2) -> ts_lv lv1 ^ ts_bop bop ^ ts_lv lv2
  | BinOpN (bop,lv,n) -> ts_lv lv ^ ts_bop bop ^ string_of_int n 
  | AHole _ -> "?" 

and ts_lv : lv -> string
= fun lv ->
  match lv with
  | Var x -> x
  | Arr (x,y) -> x ^ "[" ^ y ^ "]"

and ts_bop : bop -> string
= fun bop -> 
  match bop with
  | Plus -> " + "
  | Minus -> " - "
  | Mult -> " * "
  | Div -> " / "
  | Mod -> " % "

and ts_bexp : bexp -> string
= fun bexp ->
  match bexp with
  | True -> "true" 
  | False -> "false"
  | GtLv (lv1,lv2) -> ts_aexp (Lv lv1) ^ " > " ^ ts_aexp (Lv lv2)
  | GtN (lv,n) -> ts_aexp (Lv lv) ^ " > " ^ ts_aexp (Int n)
  | LtLv (lv1,lv2) -> ts_aexp (Lv lv1) ^ " < "  ^ ts_aexp (Lv lv2)
  | LtN (lv,n) -> ts_aexp (Lv lv) ^ " < " ^ ts_aexp (Int n)
  | EqLv (lv1,lv2) -> ts_aexp (Lv lv1) ^ " == " ^ ts_aexp (Lv lv2)
  | EqN (lv,n) -> ts_aexp (Lv lv) ^ " == " ^ ts_aexp (Int n)
  | Not b -> "!(" ^ ts_bexp b ^ ")"
  | Or (b1,b2) -> "(" ^ ts_bexp b1 ^ " || " ^ ts_bexp b2 ^ ")"
  | And (b1,b2) -> "(" ^ ts_bexp b1 ^ " && " ^ ts_bexp b2 ^ ")"
  | BHole _ -> "?"

and ts_cmd_onerow : cmd -> string
= fun cmd -> 
  match cmd with
  | Assign (lv,aexp) -> ts_lv lv ^ " = " ^ ts_aexp aexp ^ "; "
  | Skip -> "skip; "
  | Seq (c1,c2) -> ts_cmd_onerow c1 ^ ts_cmd_onerow c2
  (*| If (b,c1,Skip) -> "if (" ^ ts_bexp b ^ ") {" ^ ts_cmd_onerow c1 ^ "}; "*)
  | If (b,c1,c2) -> "if (" ^ ts_bexp b ^ ") {" ^ ts_cmd_onerow c1 ^ "} else {" ^ ts_cmd_onerow c2 ^ "}; "
  | While (b,c) -> "while(" ^ ts_bexp b ^ ") {" ^ ts_cmd_onerow c ^ "}; "
  | CHole _ -> "?; "

and ts_cmd_rows : cmd -> string
= fun cmd -> 
  match cmd with
  | Assign (lv,aexp) -> ts_lv lv ^ " = " ^ ts_aexp aexp ^ ";\n"
  | Skip -> ""
  | Seq (c1,c2) -> ts_cmd_rows c1 ^ ts_cmd_rows c2
  | If (b,c1,Skip) -> "if (" ^ ts_bexp b ^ ") {\n" ^ ts_cmd_rows c1 ^ "};\n"
  | If (b,c1,c2) -> "if (" ^ ts_bexp b ^ ") {\n" ^ ts_cmd_rows c1 ^ "}\nelse {" ^ ts_cmd_rows c2 ^ "};\n"
  | While (b,c) -> "while(" ^ ts_bexp b ^ ") {\n" ^ ts_cmd_rows c ^ "};\n"
  | CHole n -> " ?;\n"

let ts_pgm_onerow : pgm -> string
= fun (vars,cmd,var) -> 
  "fun " ^
  List.fold_left (fun acc var -> acc ^ var ^ " ") "" vars ^ 
  "-> " ^
  ts_cmd_onerow cmd ^
  "return " ^ var ^ "; "

let ts_pgm_rows : pgm -> string
= fun (vars,cmd,var) -> 
  "fun " ^
  List.fold_left (fun acc var -> acc ^ var ^ " ") "" vars ^ 
  "->\n" ^
  ts_cmd_rows cmd ^
  "return " ^ var ^ ";"