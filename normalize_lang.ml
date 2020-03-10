open Imp

type var = string

type taexp =
  | TInt of int
  | TLv of tlv
  | TBop of bop * translist
  | TAHole of int
and translist = (taexp, int) BatMap.t

and tlv =
  | TVar of var
  | TArr of var * taexp

and tbexp =
  | TTrue | TFalse
  | TLt of taexp * taexp
  | TGt of taexp * taexp
  | TEq of taexp * taexp
  | TNot of tbexp
  | TAnd of tbexp * tbexp
  | TOr of tbexp * tbexp
  | TBHole of int

and tcmd =
  | TAssign of tlv * taexp
  | TSkip
  | TSeq of tcmd * tcmd
  | TIf of tbexp * tcmd * tcmd
  | TWhile of tbexp * tcmd
  | TCHole of int

and tprog = var list * tcmd * var