open Imp
open Normalize_lang

type trans =
  | TInt of int
  | TVar of var
  | TBop of bop * translist
and translist = (trans, int) BatMap.t

let trans_add : bool -> trans -> translist -> translist
= fun opposite t t_list ->
  try
    BatMap.modify t (fun n -> if opposite then n-1 else n+1) t_list
  with Not_found -> BatMap.add t (if opposite then -1 else 1) t_list

let trans_merge : translist -> translist -> translist
= fun t1 t2 ->
  BatMap.merge (fun x t1_val t2_val -> 
    let n1 = (match t1_val with
    | Some n1 -> n1
    | None -> 0
    ) in
    let n2 = (match t2_val with
    | Some n2 -> n2
    | None -> 0
    ) in Some (n1+n2)
  ) t1 t2

let rec trans_bop : aexp -> bool -> bop -> translist -> translist
= fun e opposite pre_bop t_list ->
  match e with
  | Int n -> trans_add opposite (TInt n) t_list
  | Lv lv ->
    if is_minus then [NMinus (NLv lv)] else [NLv lv]  
  | BinOp (bop, e1, e2) ->
    (match bop with
    | Plus -> 
      if pre_bop = Plus || pre_bop = Minus
      then (trans_bop_list e1 is_minus bop)@(trans_bop_list e2 is_minus bop)
      else [trans_bop bop e1 e2]
    | Minus ->
      if pre_bop = Plus || pre_bop = Minus
      then (trans_bop_list e1 is_minus bop)@(trans_bop_list e2 (not is_minus) bop)
      else [trans_bop bop e1 e2]
    | Mult ->
      if pre_bop = Mult || pre_bop = Div
      then (trans_bop_list e1 is_minus bop)@(trans_bop_list e2 is_minus bop)
      else [trans_bop bop e1 e2]
    | Div ->
      if pre_bop = Mult || pre_bop = Div
      then (trans_bop_list e1 is_minus bop)@(trans_bop_list e2 is_minus bop)
      else [trans_bop bop e1 e2]
    | Mod ->
      if pre_bop = Mod
      then (trans_bop_list e1 is_minus bop)@(trans_bop_list e2 is_minus bop)
      else [trans_bop bop e1 e2]
    )
  | AHole n -> [NAHole n]

and trans_bop_list : bop -> aexp -> aexp -> naexp
= fun bop e1 e2 -> TBop (bop, trans_merge (trans_bop e1 false bop BatMap.empty) (trans_bop e2 false bop BatMap.empty))

let rec trans_aexp : aexp -> naexp
= fun aexp ->
  match aexp with
  | Int n -> NInt n
  | BinOp (bop, e1, e2) -> trans_bop bop e1 e2
  | AHole n -> NAHole n

let rec trans_bexp : bexp -> nbexp
= fun bexp ->
  match bexp with
  | True -> NTrue
  | False -> NFalse
  | Lt (e1, e2) -> NLt (trans_aexp e1, trans_aexp e2)
  | Gt (e1, e2) -> NGt (trans_aexp e1, trans_aexp e2)
  | Eq (e1, e2) -> NEq (trans_aexp e1, trans_aexp e2)
  | Not b -> NNot (trans_bexp b)
  | Or (b1, b2) -> NOr (trans_bexp b1, trans_bexp b2)
  | And (b1, b2) -> NAnd (trans_bexp b1, trans_bexp b2)
  | BHole n -> NBHole n

let rec trans_cmd : cmd -> ncmd
= fun cmd ->
  match cmd with
  | Assign (x, e) -> NAssign (x, trans_aexp e)
  | Skip -> NSkip
  | Seq (c1, c2) -> NSeq (trans_cmd c1, trans_cmd c2)
  | If (b, c1, c2) -> NIf (trans_bexp b, trans_cmd c1, trans_cmd c2)
  | While (b, c) -> NWhile (trans_bexp b, trans_cmd c)
  | CHole n -> NCHole n

let trans_pgm : prog -> nprog
= fun (init, cmd, res) ->
  (init, trans_cmd cmd, res)