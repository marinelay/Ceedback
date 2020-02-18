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

type prog = var list * cmd * var

type example = value list * value
type examples = example list

let exp_hole_count = ref 0
let gen_hole_aexp : unit -> aexp
= fun () -> exp_hole_count:=!exp_hole_count+1; AHole (!exp_hole_count)
let gen_hole_bexp : unit -> bexp
= fun () -> exp_hole_count:=!exp_hole_count+1; BHole (!exp_hole_count)
let gen_hole_cmd : unit -> cmd
= fun () -> exp_hole_count:=!exp_hole_count+1; CHole (!exp_hole_count)

module Memory = struct
  type t = (var,value) BatMap.t
  let add = BatMap.add
  let mem = BatMap.mem
  let find x m = BatMap.find x m 
  let empty = BatMap.empty
end

exception BufferOverFlow

let rec cost_a : aexp -> int
= fun aexp ->
  match aexp with
  | Int _ -> 10
  | Lv _ -> 10
  | BinOpLv (Mod,_,_) -> 10 
  | BinOpLv (Div,_,_) -> 10 
  | BinOpLv (_,_,_) -> 10
  | BinOpN (_,_,_) -> 10
  | AHole _ -> 80 

let rec cost_b : bexp -> int
= fun bexp ->
  match bexp with
  | True -> 25
  | False -> 25
  | GtLv (_,_) -> 10
  | GtN (_,_) -> 10
  | LtLv (_,_) -> 10
  | LtN (_,_) -> 10
  | EqLv (_,_) -> 10
  | EqN (_,_) -> 10
  | Not b -> 5 + cost_b b
  | Or (b1,b2) -> 5 + cost_b b1 + cost_b b2
  | And (b1,b2) -> 5 + cost_b b1 + cost_b b2
  | BHole _ -> 90 

let rec cost_c : cmd -> int
= fun cmd ->
  match cmd with
  | Assign (lv,a) -> 10 + cost_a (Lv lv) + cost_a a
  | Skip -> 35
  | Seq (c1,c2) -> 5 + cost_c c1 + cost_c c2
  | If (b,c1,c2) -> 25 + cost_b b + cost_c c1 + cost_c c2
  | While (b,c) -> 20 + cost_b b + cost_c c 
  | CHole _ -> 100 
  
let rec cost : prog -> int
= fun (_,cmd,_) -> cost_c cmd 

let rec eval_aexp : aexp -> Memory.t -> value
= fun aexp mem ->
  match aexp with
  | Int n -> VInt n
  | Lv lv -> eval_lv lv mem
  | BinOpLv -> (bop, e1, e2) -> eval_bop bop (eval_aexp e1 mem) (eval_aexp e2 mem)
  | AHole _ -> raise (Failure "eval_aexp : hole encountered")

and eval_lv : lv -> Memory.t -> value
= fun lv mem ->
  match lv with
  | Var x -> Memory.find x mem
  | Arr (x,e) ->
    (match Memory.find x mem, (eval_aexp e mem) with
    | VArr lst, VInt idx ->
      let size = List.length lst in
        if idx < 0 || idx >= size then raise BufferOverFlow
        else VInt (List.nth lst idx)
    | _ -> raise (Failure "imp.ml : eval_lv - variable type error")) 
    )

and value2int : value -> int
= fun value -> 
  match value with 
  | VInt n -> n 
  | _ -> raise (Failure "array value is not integer type")

and eval_bop : bop -> value -> value -> value
= fun bop v1 v2 ->
  match bop with
  | Plus  -> VInt ((value2int v1) + (value2int v2))
  | Minus -> VInt ((value2int v1) - (value2int v2))
  | Mult  -> VInt ((value2int v1) * (value2int v2))
  | Div   -> VInt ((value2int v1) / (value2int v2))
  | Mod   -> VInt ((value2int v1) mod (value2int v2))

and eval_bexp : bexp -> Memory.t -> bool
= fun bexp mem ->
  match bexp with
  | True -> true 
  | False -> false
  | Gt (e1,e2) -> (value2int (eval_aexp e1 mem)) > (value2int (eval_aexp e2 mem))
  | Lt (e1,e2) -> (value2int (eval_aexp e1 mem)) < (value2int (eval_aexp e2 mem))
  | Eq (e1,e2) -> (value2int (eval_aexp e1 mem)) = (value2int (eval_aexp e2 mem)) 
  | Not b -> not (eval_bexp b mem) 
  | Or (b1,b2) -> (eval_bexp b1 mem) || (eval_bexp b2 mem)
  | And (b1,b2) -> (eval_bexp b1 mem) && (eval_bexp b2 mem)
  | BHole _ -> raise (Failure "eval_bexp: hole encountered")

and eval_cmd : cmd -> Memory.t -> Memory.t
= fun cmd mem ->
  match cmd with
  | Assign (Var x, aexp) -> Memory.add x (eval_aexp aexp mem) mem
  | Assign (Arr (x, e), aexp) ->
    (match Memory.find x mem, (eval_aexp e mem) with
    | VArr lst, VInt idx ->
      let size = List.length lst ->
        if (idx < 0) || (idx >= size) then raise BufferOverFlow
        else
          Memory.add x (VArr (BatList.modify_at idx (fun v -> value2int (eval_aexp aexp mem)) lst)) mem
    | _ -> raise (Failure "imp.ml - eval_cmd : variable type error"))
    )
  | Seq (c1, c2) -> eval_cmd c2 (eval_cmd c1 mem)
  | If (b, c1, c2) ->
    if eval_bexp b mem then eval_cmd c1 mem else eval_cmd c2 mem
  | While (b,c) ->
    if eval_bexp b mem then eval_cmd (eval_cmd c mem) else mem
  | Skip -> mem
  | CHole _ -> raise (Failure "eval_cmd: hole encountered")

let rec value_equality : value -> value -> bool
= fun v1 v2 ->
  match v1, v2
  | VInt n1, VInt n2 -> n1=n2

let run : prog -> value list -> value (* input = value list *)
= fun (args,cmd,res) input_params ->
  let init_mem = 
  List.fold_left2 (fun mem x v -> Memory.add x v mem) Memory.empty args input_params in
    let r = Memory.find res (eval_cmd cmd init_mem) in
      r
(* let rec ts_aexp : aexp -> string
= fun aexp -> 
  match aexp with
  | Int n -> string_of_int n
  | Lv lv -> ts_lv lv
  | BinOpLv (bop,lv1,lv2) -> ts_lv lv1 ^ ts_bop bop ^ ts_lv lv2
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
  | Gt (lv1,lv2) -> ts_aexp (Lv lv1) ^ " > " ^ ts_aexp (Lv lv2)
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
  "return " ^ var ^ ";" *)