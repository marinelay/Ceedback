open Imp

type delete = var BatSet.t

type block_in = var BatSet.t
and block_out = var BatSet.t
and blocks = (int, cmd) BatMap.t
and block_info = (int, (block_in * block_out)) BatMap.t
and edge = (int * int) BatSet.t

let instr_count = ref 0
let init_instr () = (instr_count := 0)
let new_instr () = (instr_count := !instr_count+1; !instr_count)


let rec find_var_aexp : aexp -> var BatSet.t
= fun aexp ->
  match aexp with
  | Lv lv -> find_var_lv lv
  | BinOpLv (_, e1, e2) ->
    BatSet.union (find_var_aexp e1) (find_var_aexp e2)
  | _ -> BatSet.empty

and find_var_lv : lv -> var BatSet.t
= fun lv ->
  match lv with
  | Var v -> BatSet.singleton v
  | Arr (v, e) -> BatSet.add v (find_var_aexp e)

let rec find_var_bexp : bexp -> var BatSet.t
= fun bexp ->
  match bexp with
  | Gt (e1, e2) | Lt (e1, e2) | Eq (e1, e2) ->
    BatSet.union (find_var_aexp e1) (find_var_aexp e2)
  | Not b -> find_var_bexp b
  | Or (b1, b2) | And (b1, b2) ->
    BatSet.union (find_var_bexp b1) (find_var_bexp b2)
  | _ -> BatSet.empty

let rec find_var_cmd : cmd -> var BatSet.t
= fun cmd ->
  match cmd with
  | Assign (lv, e) ->
    BatSet.union (find_var_lv lv) (find_var_aexp e)
  | If (b, _, _) -> find_var_bexp b
  | While (b, _) -> find_var_bexp b
  | _ -> BatSet.empty 

let rec equal_info : block_info -> block_info -> bool
= fun prev next ->
  BatMap.equal (fun (prev_in, prev_out) (next_in, next_out) ->
    (BatSet.equal prev_in next_in) && (BatSet.equal prev_out next_out)
  ) prev next

let rec make_block_info : int -> edge -> blocks -> block_info -> block_info
= fun cnt edge blocks out info ->
  let cmd = BatMap.find cnt blocks in
  match cmd with
  | Assign (lv, aexp) ->

let rec fix_block_info : edge -> blocks -> block_info -> block_info
= fun edge blocks info ->
  let result = make_block_info edge blocks info in
  if equal_info info result then result else fix_block_info edge blocks result


let rec make_blocks : cmd -> edge -> blocks -> (edge * blocks)
= fun cmd edge blocks ->
  match cmd with
  | Assign _ | Skip _ | CHole _ -> 
    let blocks = BatMap.add new_instr() cmd blocks in
    (edge, blocks)
  | Seq (c1, c2) -> 
    let (edge, blocks) = make_blocks c1 edge blocks in
    let cnt = !instr_count in
    let edge = BatSet.add (cnt, cnt+1) edge in
    make_blocks c2 edge blocks
  | If (b, c1, c2) ->
    let blocks = BatMap.add new_instr() cmd blocks in
    let start = !instr_count in
    (* t_branch *)
    let edge = BatSet.add (start, !instr_count+1) edge in
    let (edge, blocks) = make_blocks c1 edge blocks in
    let t_branch = !instr_count in
    (* f_branch *)
    let edge = BatSet.add (start, !instr_count+1) edge in
    let (edge, blocks) = make_blocks c2 edge blocks in
    let f_branch = !instr_count in
    (* add edge *)
    let edge = BatSet.add (t_branch, !instr_count+1) edge in
    let edge = BatSet.add (f_branch, !instr_count+1) edge in
    (edge, blocks)
  | While (b, c) ->
    let blocks = BatMap.add new_instr() cmd blocks in
    let start = !instr_count in 
    (* entry loop *)
    let edge = BatSet.add (start, !instr_count+1) edge in
    let (edge, blocks) = make_blocks c edge blocks in
    let loop = !instr_count in
    (* looping *)
    let edge = BatSet.add (loop, start) edge in
    (* exit loop *)
    let edge = BatSet.add (start, !instr_count+1) edge in
    (edge, blocks)


let rec delete_dead : cmd -> delete -> (delete * cmd)
= fun cmd delete ->
  match cmd with
  | Assign (lv, aexp) ->
    begin match lv with
    | Var v ->
      if BatSet.mem v delete then (BatSet.remove v delete, Skip)
      else
      let delete = BatSet.diff (BatSet.add v delete) (find_var_aexp aexp) in
      (delete, cmd)
    | Arr (v, e) ->
      if BatSet.mem v delete then (BatSet.remove v delete, Skip)
      else
      let delete = BatSet.diff (BatSet.add v delete) (BatSet.union (find_var_aexp e) (find_var_aexp aexp)) in
      (delete, cmd)
    end
  | Skip -> (delete, Skip)
  | Seq (c1, c2) ->
    let (delete, c2) = delete_dead c2 delete in
    let (delete, c1) = delete_dead c1 delete in
    (delete, Seq (c1, c2))
  | If (b, c1, c2) ->
    let (delete', c1) = delete_dead c1 delete in
    let (delete'', c2) = delete_dead c2 delete in
    let delete = BatSet.diff (BatSet.intersect delete' delete'') (find_var_bexp b)
    (delete, If (b, c1, c2))
  | While (b, c) ->
    let (delete', c) = delete_dead c BatSet.empty in
    let delete' = BatSet.diff delete' (find_var_bexp b) in
    let (delete, c) = delete_dead c (BatSet.intersect delete delete') in
    let delete = BatSet.diff delete (find_var_bexp b) in
    (delete, While (b, c))
  | CHole _ -> (delete, cmd)

let rec optimize : prog -> lv list -> prog
= fun (args, cmd, res) lv_list ->
  let lv_set = List.fold_left (fun lv_set lv -> 
    BatSet.add lv lv_set
  ) BatSet.empty lv_list in
  let (_, cmd) = delete_dead cmd (BatSet.remove res lv_set) in
  (args, cmd, res)
  