open Imp

type nbop = Plus | Minus | Mult | Div | Mod
and var = string

type naexp =
  | NInt of int
  | NLv of nlv
  | NMinus of naexp
  | NBinOp of nbop * naexp list
  | NAHole of int

and nlv =
  | NVar of var
  | NArr of var * naexp

and nbexp =
  | NTrue | NFalse
  | NLt of naexp * naexp
  | NGt of naexp * naexp
  | NEq of naexp * naexp
  | NNot of nbexp
  | NAnd of nbexp * nbexp
  | NOr of nbexp * nbexp
  | NBHole of int

and ncmd =
  | NAssign of nlv * naexp
  | NSkip
  | NSeq of ncmd * ncmd
  | NIf of nbexp * ncmd * ncmd
  | NWhile of nbexp * ncmd
  | NCHole of int

and normal = bool * nbop (* Is Not? / Nbop *)
and nprog = var list * ncmd * var