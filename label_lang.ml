open Imp

type labeled_exp = 
  | Labeled_aexp of labeled_aexp
  | Labeled_bexp of labeled_bexp
  | Labeled_cmd of labeled_cmd 

and labeled_aexp = int * laexp
and laexp =
  | Int of int
  | Lv of labeled_lv
  | BinOpLv of bop * labeled_aexp * labeled_aexp
  | AHole of int

and labeled_lv = int * llv
and llv =
  | Var of var
  | Arr of var * labeled_aexp

and labeled_bexp = int * lbexp
and lbexp = 
  | True
  | False
  | Gt of labeled_aexp * labeled_aexp
  | Lt of labeled_aexp * labeled_aexp
  | Eq of labeled_aexp * labeled_aexp
  | Not of labeled_bexp
  | Or of labeled_bexp * labeled bexp
  | And of labeled_bexp * labeled_bexp
  | BHole of int

and labeled_cmd = int * lcmd
and lcmd =
  | Assign of lv * labeled_aexp
  | Skip
  | Seq of labeled_cmd * labeled_cmd
  | If of labeled_bexp * labeled_cmd * labeled_cmd

let label_count = ref 0
let init_label () = (label_count := 0)
let new_label () = (label_count := !label_count+1; !label_count)