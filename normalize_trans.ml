open Imp
open Normalize_lang




(* Translation aexp to taexp *)

let trans_add : bool -> taexp -> translist -> translist
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
  | Lv lv -> trans_add opposite (TLv (trans_lv lv)) t_list
  | BinOpLv (bop, e1, e2) ->
    (match bop with
    | Plus -> 
      if pre_bop = Plus || pre_bop = Minus
      then trans_merge (trans_bop e1 opposite bop t_list) (trans_bop e2 opposite bop t_list)
      else trans_add opposite (trans_bop_list bop e1 e2) t_list
    | Minus ->
      if pre_bop = Plus || pre_bop = Minus
      then trans_merge (trans_bop e1 opposite bop t_list) (trans_bop e2 (not opposite) bop t_list)
      else trans_add opposite (trans_bop_list bop e1 e2) t_list
    | Mult ->
      if pre_bop = Mult || pre_bop = Div
      then trans_merge (trans_bop e1 opposite bop t_list) (trans_bop e2 opposite bop t_list)
      else trans_add opposite (trans_bop_list bop e1 e2) t_list
    | Div ->
      if pre_bop = Mult || pre_bop = Div
      then trans_merge (trans_bop e1 opposite bop t_list) (trans_bop e2 (not opposite) bop t_list)
      else trans_add opposite (trans_bop_list bop e1 e2) t_list
    | Mod ->
      if pre_bop = Mod
      then trans_merge (trans_bop e1 opposite bop t_list) (trans_bop e2 opposite bop t_list)
      else trans_add opposite (trans_bop_list bop e1 e2) t_list
    )
  | AHole n -> trans_add opposite (TAHole n) t_list

and trans_bop_list : bop -> aexp -> aexp -> taexp
= fun bop e1 e2 -> 
  match bop with
  | Plus | Mult | Mod ->
    TBop (bop, trans_merge (trans_bop e1 false bop BatMap.empty) (trans_bop e2 false bop BatMap.empty))
  | Minus | Div ->
    TBop (bop, trans_merge (trans_bop e1 false bop BatMap.empty) (trans_bop e2 true bop BatMap.empty)) 



and trans_aexp : aexp -> taexp
= fun aexp ->
  match aexp with
  | Int n -> TInt n
  | Lv lv -> TLv (trans_lv lv)
  | BinOpLv (bop, e1, e2) -> trans_bop_list bop e1 e2
  | AHole n -> TAHole n

and trans_lv : lv -> tlv
= fun lv ->
  match lv with
  | Var x -> TVar x
  | Arr (x, e) -> TArr (x, trans_aexp e)
  | AbsVar -> TAbsVar

and trans_bexp : bexp -> tbexp
= fun bexp ->
  match bexp with
  | True -> TTrue
  | False -> TFalse
  | Lt (e1, e2) -> TLt (trans_aexp e1, trans_aexp e2)
  | Gt (e1, e2) -> TGt (trans_aexp e1, trans_aexp e2)
  | Eq (e1, e2) -> TEq (trans_aexp e1, trans_aexp e2)
  | Not b -> TNot (trans_bexp b)
  | Or (b1, b2) -> TOr (trans_bexp b1, trans_bexp b2)
  | And (b1, b2) -> TAnd (trans_bexp b1, trans_bexp b2)
  | BHole n -> TBHole n

and trans_cmd : cmd -> tcmd
= fun cmd ->
  match cmd with
  | Assign (x, e) -> TAssign (trans_lv x, trans_aexp e)
  | Skip -> TSkip
  | Seq (c1, c2) -> TSeq (trans_cmd c1, trans_cmd c2)
  | If (b, c1, c2) -> TIf (trans_bexp b, trans_cmd c1, trans_cmd c2)
  | While (b, c) -> TWhile (trans_bexp b, trans_cmd c)
  | CHole n -> TCHole n

let rec ts_taexp : taexp -> string
= fun taexp ->
  match taexp with
  | TInt n -> string_of_int n
  | TLv lv -> ts_tlv lv
  | TBop (bop, t_list) -> BatMap.foldi (fun x n s ->
    (ts_taexp x) ^ "->" ^ (string_of_int n) ^ " " ^ s 
  ) t_list ""
  | _ -> "" 

and ts_tlv : tlv -> string
= fun tlv ->
  match tlv with
  | TVar x -> x
  | _ -> ""

let trans_pgm : prog -> tprog
= fun (init, cmd, res) ->
  (init, trans_cmd cmd, res)

(* END *)

(* Translation taexp to aexp *)

let rec make_add : int -> aexp -> aexp
= fun n aexp ->
  if n = 0 then Int 0
  else
    if n > 0
    then BinOpLv (Plus, aexp, make_add (n-1) aexp)
    else BinOpLv (Minus, aexp, make_add (n+1) aexp)

let rec make_mult : int -> aexp -> aexp
= fun n aexp ->
  if n = 0 then Int 1
  else
    if n > 0
    then BinOpLv (Mult, aexp, make_mult (n-1) aexp)
    else BinOpLv (Div, aexp, make_mult (n+1) aexp)

let rec restore_bop : bop -> translist -> aexp
= fun bop t_list ->
  if BatMap.is_empty t_list
  then
    if bop = Plus || bop = Minus then Int 0 else Int 1
  else 
    let ((taexp, n), t_list) = BatMap.pop_max_binding t_list in
    (match bop with
    | Plus | Minus -> BinOpLv (Plus, BinOpLv(Mult, Int n, restore_aexp taexp), restore_bop bop t_list)
    | Mult | Div -> BinOpLv (Mult, make_mult n (restore_aexp taexp), restore_bop bop t_list)
    | _ -> BinOpLv (bop, BinOpLv (Mult, Int n, restore_aexp taexp), restore_bop bop t_list)
    )
    

and restore_aexp : taexp -> aexp
= fun taexp ->
  match taexp with
  | TInt n -> Int n
  | TLv lv -> Lv (restore_lv lv)
  | TBop (bop, t_list) -> restore_bop bop t_list
  | TAHole n -> AHole n

and restore_lv : tlv -> lv
= fun tlv ->
  match tlv with
  | TVar x -> Var x
  | TArr (x, e) -> Arr (x, restore_aexp e)
  | TAbsVar -> AbsVar

let rec restore_bexp : tbexp -> bexp
= fun tbexp ->
  match tbexp with
  | TTrue -> True
  | TFalse -> False
  | TLt (e1, e2) -> Lt (restore_aexp e1, restore_aexp e2)
  | TGt (e1, e2) -> Gt (restore_aexp e1, restore_aexp e2)
  | TEq (e1, e2) -> Eq (restore_aexp e1, restore_aexp e2)
  | TNot e -> Not (restore_bexp e)
  | TAnd (e1, e2) -> And (restore_bexp e1, restore_bexp e2)
  | TOr (e1, e2) -> Or (restore_bexp e1, restore_bexp e2)
  | TBHole n -> BHole n

let rec restore_cmd : tcmd -> cmd
= fun tcmd ->
  match tcmd with
  | TAssign (x, e) -> Assign (restore_lv x, restore_aexp e)
  | TSkip -> Skip
  | TSeq (c1, c2) -> Seq (restore_cmd c1, restore_cmd c2)
  | TIf (b, c1, c2) -> If (restore_bexp b, restore_cmd c1, restore_cmd c2)
  | TWhile (b, c) -> While (restore_bexp b, restore_cmd c)
  | TCHole n -> CHole n 

let restore_pgm : tprog -> prog
= fun (init, tcmd, res) -> (init, restore_cmd tcmd, res)

(* End *)

let expression_trans : prog -> prog
= fun pgm -> 
  restore_pgm (trans_pgm pgm)