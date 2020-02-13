open Imp
open Label_lang

let rec labeling_aexp : aexp -> labeled_aexp
= fun aexp ->
  match aexp with
  | Int n -> (new_label (), Int n)
  | Lv lv -> (new_label (), Lv (labeling_lv lv))
  | BinOpLv (bop, e1, e2) -> (new_label (), BinOpLv (bop, labeling_aexp e1, labeling_aexp e2)
  | AHole n -> (new_label (), AHole n)

and labeling_lv : lv -> labeled_lv
= fun lv ->
  match lv with
  | Var x -> (new_label (), Var x)
  | Arr (x, e) -> (new_label (), Arr (x, labeling_aexp e)

and labeling_bexp : bexp -> labeled_bexp
= fun bexp ->
  match bexp with
  | True -> (new_label (), True)
  | False -> (new_label (), False)
  
  