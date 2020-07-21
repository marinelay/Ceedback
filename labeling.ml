open Imp
open Label_lang

let rec labeling_aexp : aexp -> labeled_aexp
= fun aexp ->
  match aexp with
  | Int n -> (new_label (), Int n)
  | Lv lv -> (new_label (), Lv (labeling_lv lv))
  | BinOpLv (bop, e1, e2) -> (new_label (), BinOpLv (bop, labeling_aexp e1, labeling_aexp e2))
  | AHole n -> (new_label (), AHole n)

and labeling_lv : lv -> labeled_lv
= fun lv ->
  match lv with
  | Var x -> (new_label (), Var x)
  | Arr (x, e) -> (new_label (), Arr (x, labeling_aexp e))

and labeling_bexp : bexp -> labeled_bexp
= fun bexp ->
  match bexp with
  | True -> (new_label (), True)
  | False -> (new_label (), False)
  | Gt (e1, e2) -> (new_label(), Gt (labeling_aexp e1, labeling_aexp e2))
  | Lt (e1, e2) -> (new_label(), Lt (labeling_aexp e1, labeling_aexp e2))
  | Eq (e1, e2) -> (new_label(), Eq (labeling_aexp e1, labeling_aexp e2))
  | Not e -> (new_label(), Not (labeling_bexp e))
  | Or (e1, e2) -> (new_label(), Or (labeling_bexp e1, labeling_bexp e2))
  | And (e1, e2) -> (new_label(), And (labeling_bexp e1, labeling_bexp e2))
  | BHole n -> (new_label (), BHole n)

and labeling_cmd : cmd -> labeled_cmd
= fun cmd ->
  match cmd with
  | Assign (Var x, e) -> (new_label (), Assign (Var x, labeling_aexp e))
  | Assign (Arr (x, e1), e2) -> (new_label (), Assign (Arr (x, labeling_aexp e1), labeling_aexp e2))
  | Skip -> (new_label(), Skip)
  | Seq (c1, c2) -> (new_label (), Seq (labeling_cmd c1, labeling_cmd c2))
  | If (b, c1, c2) -> (new_label (), If (labeling_bexp b, labeling_cmd c1, labeling_cmd c2))
  | While (b, c) -> (new_label (), While (labeling_bexp b, labeling_cmd c))
  | CHole n -> (new_label (), CHole n)

let labeling_prog : prog -> labeled_prog
= fun (args, cmd, res) -> (args, labeling_cmd cmd, res)(*List.fold_left (fun acc x -> acc@[labeling_cmd x]) [] pgm*)

(* labeled AST -> AST *)
let rec unlabeling_aexp : labeled_aexp -> aexp
= fun (l, exp) ->
  match exp with
  | Int n -> Int n
  | Lv lv -> Lv (unlabeling_lv lv)
  | BinOpLv (bop, e1, e2) -> BinOpLv (bop, unlabeling_aexp e1, unlabeling_aexp e2)
  | AHole n -> AHole n

and unlabeling_lv : labeled_lv -> lv
= fun (l, exp) ->
  match exp with
  | Var x -> Var x
  | Arr (x, e) -> Arr (x, unlabeling_aexp e)

and unlabeling_bexp : labeled_bexp -> bexp
= fun (l, exp) ->
  match exp with
  | True -> True
  | False -> False
  | Gt (e1, e2) -> Gt (unlabeling_aexp e1, unlabeling_aexp e2)
  | Lt (e1, e2) -> Lt (unlabeling_aexp e1, unlabeling_aexp e2)
  | Eq (e1, e2) -> Eq (unlabeling_aexp e1, unlabeling_aexp e2)
  | Not e -> Not (unlabeling_bexp e)
  | Or (e1, e2) -> Or (unlabeling_bexp e1, unlabeling_bexp e2)
  | And (e1, e2) -> And (unlabeling_bexp e1, unlabeling_bexp e2)
  | BHole n -> BHole n

and unlabeling_cmd : labeled_cmd -> cmd
= fun (l, exp) ->
  match exp with
  | Assign (Var x, e) -> Assign (Var x, unlabeling_aexp e)
  | Assign (Arr (x, e1), e2) -> Assign (Arr (x, unlabeling_aexp e1), unlabeling_aexp e2)
  | Skip -> Skip
  | Seq (c1, c2) -> Seq (unlabeling_cmd c1, unlabeling_cmd c2)
  | If (b, c1, c2) -> If (unlabeling_bexp b, unlabeling_cmd c1, unlabeling_cmd c2)
  | While (b, c) -> While (unlabeling_bexp b, unlabeling_cmd c)
  | CHole n -> CHole n

let unlabeling_prog : labeled_prog -> prog
= fun (args, l_cmd, res) -> (args, unlabeling_cmd l_cmd, res)

(* Generating Hole *)
let rec gen_hole_aexp : int -> labeled_aexp -> labeled_aexp
= fun n (label, aexp) ->
  if label = n then labeling_aexp (ahole ()) else
  match aexp with
  | BinOpLv (bop, e1, e2) -> (label, BinOpLv (bop, gen_hole_aexp n e1, gen_hole_aexp n e2))
  | _ -> (label, aexp)

and gen_hole_bexp : int -> labeled_bexp -> labeled_bexp
= fun n (label, bexp) ->
  if label = n then labeling_bexp (bhole ()) else
  match bexp with
  | Gt (e1, e2) -> (label, Gt (gen_hole_aexp n e1, gen_hole_aexp n e2))
  | Lt (e1, e2) -> (label, Lt (gen_hole_aexp n e1, gen_hole_aexp n e2))
  | Eq (e1, e2) -> (label, Eq (gen_hole_aexp n e1, gen_hole_aexp n e2))
  | Not e -> (label, Not (gen_hole_bexp n e))
  | Or (e1, e2) -> (label, Or (gen_hole_bexp n e1, gen_hole_bexp n e2))
  | And (e1, e2) -> (label, And (gen_hole_bexp n e1, gen_hole_bexp n e2))
  | _ -> (label, bexp)

and gen_hole_cmd : int -> labeled_cmd -> labeled_cmd
= fun n (label, cmd) ->
  if label = n then labeling_cmd (chole ()) else
  match cmd with
  | Assign (x, e) -> (label, Assign (x, gen_hole_aexp n e))
  | Seq (c1, c2) -> (label, Seq (gen_hole_cmd n c1, gen_hole_cmd n c2))
  | If (b, c1, c2) -> (label, If (gen_hole_bexp n b , gen_hole_cmd n c1, gen_hole_cmd n c2))
  | While (b, c) -> (label, While (gen_hole_bexp n b, gen_hole_cmd n c))
  | _ -> (label, cmd)

let rec gen_hole_prog : int -> labeled_prog -> labeled_prog
= fun n (args, l_cmd, res) -> (args, gen_hole_cmd n l_cmd, res) 
  