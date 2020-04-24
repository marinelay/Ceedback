open Imp

type path = bexp BatList.t
and path_set = path BatSet.t

  (* Pruning Infinite case *)

let rec find_aexp_hole : aexp -> bool
= fun aexp ->
  match aexp with
  | Lv lv -> find_lv_hole lv
  | BinOpLv (_, e1, e2) -> (find_aexp_hole e1) || (find_aexp_hole e2)
  | AHole _ -> true
  | _ -> false

and find_lv_hole : lv -> bool
= fun lv ->
  match lv with
  | Arr (_, e) -> find_aexp_hole e
  | _ -> false

and find_bexp_hole : bexp -> bool
= fun bexp ->
  match bexp with
  | Gt (e1, e2) -> (find_aexp_hole e1) || (find_aexp_hole e2)
  | Lt (e1, e2) -> (find_aexp_hole e1) || (find_aexp_hole e2)
  | Eq (e1, e2) -> (find_aexp_hole e1) || (find_aexp_hole e2)
  | Not e -> find_bexp_hole e
  | Or (e1, e2) -> (find_bexp_hole e1) || (find_bexp_hole e2)
  | And (e1, e2) -> (find_bexp_hole e1) || (find_bexp_hole e2)
  | BHole _ -> true
  | _ -> false

let rec find_cmd_hole : cmd -> bool
= fun cmd ->
  match cmd with
  | Seq (c1, c2) | If (_, c1, c2) -> (find_cmd_hole c1) || (find_cmd_hole c2)
  | While (_, c) -> find_cmd_hole c
  | CHole _ -> true
  | _ -> false

let rec set_of_var_aexp : aexp -> lv BatSet.t
= fun aexp ->
  match aexp with
  | Lv lv -> BatSet.singleton lv
  | BinOpLv (bop, e1, e2) -> BatSet.union (set_of_var_aexp e1) (set_of_var_aexp e2)
  | _ -> BatSet.empty

let rec set_of_var_bexp : bexp -> lv BatSet.t
= fun bexp ->
  match bexp with
  | Gt (e1, e2) | Lt (e1, e2) | Eq (e1, e2) ->
    BatSet.union (set_of_var_aexp e1) (set_of_var_aexp e2)
  | Not e -> set_of_var_bexp e
  | Or (e1, e2) | And (e1, e2) -> BatSet.union (set_of_var_bexp e1) (set_of_var_bexp e2)
  | _ -> BatSet.empty

let rec set_of_assign : cmd -> cmd BatSet.t
= fun cmd ->
  match cmd with
  | Assign (x, e) ->
    (match e with
    | Lv lv when x = lv -> BatSet.empty
    | _ -> BatSet.singleton cmd
    )
  | Seq (c1, c2) | If(_, c1, c2) ->
    BatSet.union (set_of_assign c1) (set_of_assign c2)
  | While (_, c) -> set_of_assign c
  | _ -> BatSet.empty

let rec cntvar_redefined : bexp -> cmd -> bool
= fun bexp cmd ->
  let find_hole = (find_bexp_hole bexp) || (find_cmd_hole cmd) in
  let assign_set = set_of_assign cmd in
  let var_set = set_of_var_bexp bexp in
  let redefined = BatSet.fold (fun lv find -> 
    (BatSet.exists (fun cmd ->
      match cmd with
      | Assign (lv_a, _) when lv = lv_a -> true
      | _ -> false
    ) assign_set) || find
  ) var_set false in
  find_hole || redefined

let rec infinite : cmd -> bool
= fun cmd ->
  match cmd with
  | Seq (c1, c2) | If(_, c1, c2) -> infinite c1 || infinite c2
  | While (b, c) -> (not (cntvar_redefined b c)) || infinite c
  | _ -> false

let rec infinite_possible : (int * prog)-> bool
= fun (_, (_, cmd, _)) -> infinite cmd

