open Imp
open Itv
open Vocab
open Abs
open Normalize_lang
open Normalize_trans

(***********************
 ***********************
 EXPRESSION SIMPLIFICATION 
 **********************
 **********************)

let rec simple_bop : bop -> aexp -> aexp -> aexp
= fun bop e1 e2 ->
  match bop with
  | Plus ->
    (match (e1, e2) with
    | Int n1, Int n2 -> Int (n1 + n2)
    | Int 0, e | e, Int 0 -> e
    | _ -> BinOpLv (bop, e1, e2)
    )
  | Minus ->
    (match (e1, e2) with
    | Int n1, Int n2 -> Int (n1 - n2)
    | e, Int 0 -> e
    | _ -> if e1 = e2 then Int 0 else BinOpLv (bop, e1, e2)
    )
  | Mult ->
    (match (e1, e2) with
    | Int n1, Int n2 -> Int (n1*n2)
    | Int 0, _ | _, Int 0 -> Int 0
    | Int 1, e | e, Int 1 -> e
    | _ -> BinOpLv (bop, e1, e2)
    )
  | Div ->
    (match (e1, e2) with
    | Int n1, Int n2 -> Int (n1 / n2)
    | Int 0, _ -> Int 0
    | _, Int 0 -> raise (Failure "Division_by_zero : simple_bop_div")
    | e, Int 1 -> e
    | _ -> BinOpLv (bop, e1, e2)
    )
  | Mod ->
    (match (e1, e2) with
    | Int n1, Int n2 -> Int (n1 mod n2)
    | _, Int 0 -> raise (Failure "Division_by_zero : simple_bop_mod")
    | _, Int 1 -> Int 0
    | Int 0, _ -> Int 0
    | _ -> BinOpLv (bop, e1, e2)
    )

let rec simple_aexp : aexp -> aexp
= fun aexp ->
  match aexp with
  | BinOpLv (bop, e1, e2) -> simple_bop bop (simple_aexp e1) (simple_aexp e2)
  | _ -> aexp

let rec simple_bexp : bexp -> bexp
= fun bexp ->
  match bexp with
  | Gt (e1,e2) ->
    if e1 = e2 then False
    else Gt (simple_aexp e1, simple_aexp e2)
  | Lt (e1,e2) ->
    if e1 = e2 then False
    else Lt (simple_aexp e1, simple_aexp e2)
  | Eq (e1,e2) ->
    if e1 = e2 then False
    else Eq (simple_aexp e1, simple_aexp e2)
  | Not True -> False
  | Not False -> True
  | Not (Not b) -> simple_bexp b
  | Not b -> Not (simple_bexp b)
  | Or (b1,b2) ->
    if true_exists (Or (b1,b2)) then True else
    if all_false (Or (b1,b2)) then False 
    else Or (simple_bexp b1, simple_bexp b2)
  | And (b1,b2) -> 
    if false_exists (And (b1,b2)) then False else
    if all_true (And (b1,b2)) then True 
    else And (simple_bexp b1, simple_bexp b2)
  | _ -> bexp

and all_true : bexp -> bool
= fun bexp ->
  match bexp with
  | True -> true
  | Or (b1,b2) -> all_true b1 && all_true b2
  | And (b1,b2) -> all_true b1 && all_true b2
  | Not b -> all_true b
  | _ -> false

and true_exists : bexp -> bool
= fun bexp ->
  match bexp with
  | True -> true
  | Or (b1,b2) -> true_exists b1 || true_exists b2
  | And (b1,b2) -> true_exists b1 || true_exists b2
  | Not b -> true_exists b
  | _ -> false

and all_false : bexp -> bool
= fun bexp ->
  match bexp with
  | False -> true
  | Or (b1,b2) -> all_false b1 && all_false b2
  | And (b1,b2) -> all_false b1 && all_false b2
  | Not b -> all_false b
  | _ -> false

and false_exists : bexp -> bool
= fun bexp ->
  match bexp with
  | False -> true
  | Or (b1,b2) -> false_exists b1 || false_exists b2
  | And (b1,b2) -> false_exists b1 || false_exists b2
  | Not b -> false_exists b
  | _ -> false

let rec simple_cmd : cmd -> cmd
= fun cmd ->
  match cmd with
  | Assign (lv,aexp) -> Assign (lv, simple_aexp aexp) 
  | Skip -> cmd
  | Seq (c1,c2) -> Seq (simple_cmd c1, simple_cmd c2)
  | If (b,c1,c2) -> If (simple_bexp b, simple_cmd c1, simple_cmd c2)
  | While (b,c) -> While (simple_bexp b, simple_cmd c)
  | CHole _ -> cmd

let expression_simplification : prog -> prog
= fun (args,cmd,res) -> 
  let cmd' = simple_cmd cmd in
    (args,cmd',res)


(******************************
 ******************************
    EXPRESSION REORDERING 
 ******************************
 ******************************)

let rec arg_num_aexp : aexp -> int
= fun e ->
  match e with
  | BinOpLv (_, e1, e2) -> arg_num_aexp e1 + arg_num_aexp e2
  | _ -> 1

let rec compare_aexp : aexp -> aexp -> bool
= fun e1 e2 ->
  match (e1, e2) with
  | (Int n1, Int n2) -> n1 > n2
  | (Lv lv1, Lv lv2) -> compare_lv lv1 lv2
  | (Lv _, _) -> true
  | (_, Lv _) -> false
  | (AHole _, _) -> true
  | (_, AHole _) -> false
  | _ -> ((arg_num_aexp e1) >= (arg_num_aexp e2))

and compare_lv : lv -> lv -> bool
= fun lv1 lv2 ->
  match (lv1, lv2) with
  | (Var x1, Var x2) -> x1 > x2
  | (Arr (x1, _), Arr (x2, _)) -> x1 > x2
  | _ -> true
 
let rec exp_reorder_aexp : aexp -> aexp
= fun aexp -> 
  match aexp with
  | Lv lv ->
    (match lv with
    | Arr (x, y) -> Lv (Arr (x, (exp_reorder_aexp y)))
    | _ -> aexp
    )
  | BinOpLv (bop,lv1,lv2) when bop = Plus || bop = Mult -> 
    if compare_aexp lv1 lv2 then BinOpLv (bop, exp_reorder_aexp lv1, exp_reorder_aexp lv2) else BinOpLv (bop, exp_reorder_aexp lv2, exp_reorder_aexp lv1)
  | BinOpLv (bop, lv1, lv2) when bop = Minus || bop = Div ->
    BinOpLv (bop, exp_reorder_aexp lv1, exp_reorder_aexp lv2)
  | _ -> aexp  

and exp_reorder_bexp : bexp -> bexp
= fun bexp -> 
  match bexp with
  | Gt (lv1, lv2) -> exp_reorder_bexp (Lt (lv2, lv1))
  | Lt (lv1, lv2) -> Lt (exp_reorder_aexp lv1, exp_reorder_aexp lv2)
  | Eq (lv1,lv2) ->
    if compare_aexp lv1 lv2 then Eq (exp_reorder_aexp lv1, exp_reorder_aexp lv2) else Eq (exp_reorder_aexp lv2, exp_reorder_aexp lv1)
  | Not b -> Not (exp_reorder_bexp b)
  | Or (b1,b2) -> Or (exp_reorder_bexp b1, exp_reorder_bexp b2)
  | And (b1,b2) -> And (exp_reorder_bexp b1, exp_reorder_bexp b2)
  | _ -> bexp

and exp_reorder_cmd : cmd -> cmd
= fun cmd ->
  match cmd with
  | Assign (lv,aexp) -> Assign (lv, exp_reorder_aexp aexp)
  | Skip -> Skip
  | Seq (c1,c2) -> Seq (exp_reorder_cmd c1, exp_reorder_cmd c2)
  | If (b,c1,c2) -> If (exp_reorder_bexp b, exp_reorder_cmd c1, exp_reorder_cmd c2)
  | While (b,c) -> While (exp_reorder_bexp b, exp_reorder_cmd c)
  | CHole n -> CHole n

let expression_reorder : prog -> prog
= fun (args,cmd,res) -> (args, exp_reorder_cmd cmd, res)  

let onestep_equivalence : lv list -> prog -> prog
= fun lv_comps pgm ->
  let pgm' = expression_simplification pgm in
  let pgm' = expression_reorder pgm' in
  let pgm' = expression_trans pgm' in
    pgm'

let rec equivalence : lv list -> int * prog -> int * prog
= fun lv_comps (rank, pgm) ->
    let (args,_,res) = pgm in
    try
      let pgm' = onestep_equivalence lv_comps pgm in
        if BatString.equal (ts_pgm_onerow pgm') (ts_pgm_onerow pgm) then (rank, pgm')
        else equivalence lv_comps (rank, pgm')
    with 
      | Not_found -> (rank, (args,Skip,res))
      | Division_by_zero -> (rank, (args,Skip,res))
