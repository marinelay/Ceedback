open Imp

type hole = A of aexp | B of bexp | C of cmd

module Workset = struct
  type work = int * prog * Type.HoleType.t * Type.VariableType.t

  module OrderedType = struct
    type t = work
    let compare (rank1,p1,_,_) (rank2,p2,_,_) =
    let (c1,c2) = (rank1 + (cost p1), rank2 + (cost p2)) in
      if c1=c2 then 0 else
      if c1>c2 then 1
      else -1
  end

  module Heap = BatHeap.Make (OrderedType)

  (* type of workset : heap * (string set) *)
  type t = Heap.t * string BatSet.t
  let empty = (Heap.empty, BatSet.empty)

  let explored : prog -> t -> bool
  = fun pgm (_,sset) -> BatSet.mem (Print.program_to_string pgm) sset

  let add : work -> t -> t
  = fun (n,pgm,h_t,h_e) (heap,sset) ->
    try
      if explored (Normalize.normalize pgm) (heap,sset) then (heap,sset)
      else
        (Heap.add (n,pgm,h_t,h_e) heap, BatSet.add (Print.program_to_string (Normalize.normalize pgm)) sset)
    with
      |_ -> (heap,sset)
  let choose : t -> (work * t) option
  = fun (heap,sset) ->
    try
      let elem = Heap.find_min heap in
      Some (elem, (Heap.del_min heap, sset))
    with
      | _ -> None

  let workset_info : t -> string
  = fun (heap,sset) ->
    "To explore : " ^ (string_of_int (Heap.size heap)) ^
    " Explored : " ^ (string_of_int (BatSet.cardinal sset))
end

let replace_aexp : prog -> aexp -> aexp -> prog
= fun (args, cmd, res) ah acandi ->
  let rec replace_a : cmd -> aexp -> aexp -> cmd
  = fun cmd ah acandi ->
    match cmd with
    | Assign (x, e) -> Assign (x, replace_a'' e ah acandi)
    | Seq (c1, c2) -> Seq (replace_a c1 ah acandi, replace_a c2 ah acandi)
    | If (b, c1, c2) -> If (replace_a' b ah acandi, replace_a c1 ah acandi, replace_a c2 ah acandi)
    | While (b, c) -> While (replace_b' b ah acandi, replace_a c ah acandi)
    | _ -> cmd
  and replace_a' : bexp -> aexp -> aexp -> bexp
  = fun bexp ah acandi ->
    match bexp with
    | Gt (e1, e2) -> Gt (replace_a'' e1 ah acandi, replace_a'' e2 ah acandi)
    | Lt (e1, e2) -> Lt (replace_a'' e1 ah acandi, replace_a'' e2 ah acandi)
    | Eq (e1, e2) -> Eq (replace_a'' e1 ah acandi, replace_a'' e2 ah acandi)
    | Not b -> Not (replace_a' b ah acandi)
    | Or (e1, e2) -> Or (replace_a' e1 ah acandi, replace_a' e2 ah acandi)
    | And (e1, e2) -> And (replace_a' e1 ah acandi, replace_a' e2 ah acandi)
    | _ -> bexp
  and replace_a'' : aexp -> aexp -> aexp -> aexp
  = fun aexp ah acandi ->
    match aexp with
    | Lv lv ->
      (match lv with
      | Arr (x, e) -> replace_a'' e ah acandi
      )
    | BinOpLv (bop, e1, e2) -> BinOpLv (bop, replace_a'' e1 ah acandi, replace_a'' e2 ah acandi)
    | AHole n when ah = aexp -> acandi
    | _ -> aexp
  in (args, (replace_a cmd ah acandi), res)

let replace_bexp : prog -> bexp -> bexp -> prog
= fun (args, cmd, res) bh bcandi ->
  let rec replace_b : cmd -> bexp -> bexp -> cmd
  = fun cmd bh bcandi ->
    match cmd with
    | Seq (c1, c2) -> Seq (replace_b c1 bh bcandi, replace_b c2 bh bcandi)
    | If (b, c1, c2) -> If (replace_b' b bh bcandi, replace_b c1 bh bcandi, replace_b c2 bh bcandi)
    | While (b, c) -> While (replace_b' b bh bcandi, replace_b bh bcandi)
    | _ -> cmd
  and replace_b' : bexp -> bexp -> bexp -> bexp
  = fun bexp bh bcandi ->
    match bexp with
    | Not b -> Not (replace_b' b bh bcandi)
    | Or (b1, b2) -> Or (replace_b' b1 bh bcandi, replace_b' b2 bh bcandi)
    | And (b1, b2) -> And (replace_b' b1 bh bcandi, replace_b' b2 bh bcandi)
    | BHole n when bh = bexp -> bcandi
    | _ -> bexp
  in (args, (replace_b cmd bh bcandi), res)

let replace_cmd : prog -> cmd -> cmd -> prog
= fun (args, cmd, res) ch ccandi ->
  let rec replace_c : cmd -> cmd -> cmd -> cmd
  = fun cmd ch ccandi ->
    match cmd with
    | Seq (c1, c2) -> Seq (replace_c c1 ch ccandi, replace_c c2 ch ccandi)
    | If (b, c1, c2) -> If (b, replace_c c1 ch ccandi, replace_c c2 ch ccandi)
    | While (b, c) -> While (b, replace_c c ch ccandi)
    | CHole n when ch = cmd -> ccandi

let gen_nextstates_a : aexp BatSet.t -> (prog * aexp) -> prog BatSet.t
= fun candidates (p, ah) ->
  BatSet.fold (fun acandi acc ->
    BatSet.add (replace_a p ah acandi) acc
  ) candidates BatSet.empty

let gen_nextstates_b : bexp BatSet.t -> (prog * bexp) -> prog BatSet.t
= fun candidates (p, bh) ->
  BatSet.fold (fun bcandi acc ->
    BatSet.add (replace_b p bh bcandi) acc
  ) candidates BatSet.empty

let gen_nextstates_b : cmd BatSet.t -> (prog * cmd) -> prog BatSet.t
= fun candidates (p, ch) ->
  BatSet.fold (fun bcandi acc ->
    BatSet.add (replace_c p ch ccandi) acc
  ) candidates BatSet.empty

let nextof_a : lv list -> prog * aexp -> aexp BatSet.t -> prog BatSet.t
= fun lv_comps (p, ah) a_comps ->
  let variable = List.fold_left (fun acc n -> BatSet.add (Lv lv) acc) BatSet.empty lv_comps) in
  gen_nextstates_a (BatSet.union variable a_comps) (p, ah)

let nextof_b : lv list -> prog * bexp -> bexp BatSet.t -> prog BatSet.t
= fun lv_comps (p, bh) b_comps ->
  gen_nextstates_b b_comps (p, bh)

let nextof_b : lv list -> prog * cmd -> cmd BatSet.t -> prog BatSet.t
  = fun lv_comps (p, ch) c_comps ->
  gen_nextstates_c c_comps (p, cmd)

  (* Pruning Infinite case *)
let rec infinite_possible : pgm -> bool
= fun (_,cmd,_) -> infinite cmd

and infinite : cmd -> bool
= fun cmd ->
  match cmd with
  | Seq (c1,c2) 
  | If (_,c1,c2) -> infinite c1 || infinite c2
  | While (b,c) ->
    let cmd_list = list_of_cmd c in
    let (remaining_cmd,last_cmd) = BatList.split_at (List.length cmd_list - 1) cmd_list in
    let last_cmd = List.hd last_cmd in 
      cntvar_redefined b remaining_cmd || not (permitted_last b last_cmd) || infinite c
  | _ -> false 
  
and list_of_cmd : cmd -> cmd list 
= fun cmd -> 
  match cmd with
  | Seq (c1,c2) -> (list_of_cmd c1)@(list_of_cmd c2)
  | _ -> [cmd]

    (* Update Components *)
let rec update_components_aexp : aexp -> aexp
= fun aexp ->
  match aexp with
  | BinOpLv (bop, e1, e2) -> BinOpLv (bop, ahole(), ahole())
  | _ -> aexp

and update_components_bexp : bexp -> bexp
= fun bexp ->
  match bexp with
  | Gt (e1, e2) -> Gt (ahole(), ahole())
  | Lt (e1, e2) -> Lt (ahole(), ahole())
  | Eq (e1, e2) -> Eq (ahole(), ahole())
  | Not b -> Not bhole()
  | Or (e1, e2) -> Or (bhole(), bhole())
  | And (e1, e2) -> And (bhole(), bhole())
  | _ -> bexp

and update_components_cmd : cmd -> cmd
= fun cmd ->
  match cmd with
  | Assign (x, e) -> Assign (x, ahole())
  | Seq (c1, c2) -> Seq (chole(), chole())
  | If (b, c1, c2) -> If (bhole(), chole(), chole())
  | While (b, c) -> While (bhole(), chole())
  | _ -> cmd


let rec update_components : components -> components
= fun (a_comps, b_comps, c_comps) ->
  let a_comps = BatSet.map update_component_aexp a_comps in
  let b_comps = BatSet.map update_component_bexp b_comps in
  let c_comps = BatSet.map update_component_cmd c_comps in
  (a_comps, b_comps, c_comps)
  

let rec find_cmdholes : cmd -> components -> components
= fun cmd comps ->
  match cmd with
  | Assign (x, e) -> find_aexpholes e comps
  | Seq (c1, c2) ->
    let t = find_cmdholes c1 comps in
    

let find_aholes : prog -> aexp BatSet.t
= fun (_, cmd, _) ->
  let rec aholes : cmd -> aexp BatSet.t
  = fun cmd ->
    match cmd with
    | Assign (x, e) -> aholes'' e
    | Seq (c1, c2) ->
      let t = aholes c1 in
      if (BatSet.is_empty t) then aholes c2 else t
    | If (b, c1, c2) ->
      let t = aholes' b in
      if (BatSet.is_empty t) then
        let t = aholes c1 in
        if (BatSet.is_empty t) then
          aholes c2
        else t
      else t
    | While (b,c) ->
      let t = aholes' b in
      if (BatSet.is_empty t) then aholes c else t
    | _ -> BatSet.empty
  and aholes' : bexp -> bexp BatSet.t
  = fun bexp ->
    match bexp with
    | Gt (e1, e2) ->
      let t = aholes'' e1 in
      if (BatSet.is_empty t) then aholes'' e2 else t
    | Lt (e1, e2) ->
      let t = aholes'' e1 in
      if (BatSet.is_empty t) then aholes'' e2 else t
    | Eq (e1, e2) ->
      let t = aholes'' e1 in
      if (BatSet.is_empty t) then aholes'' e2 else t    
    | Not b -> bholes' b
    | Or (b1, b2) | And (b1, b2) ->
      let t = aholes' b1 in
      if (BatSet.is_empty t) then aholes' b2 else t
    | _ -> BatSet.empty
  and aholes'' : aexp -> aexp BatSet.t
  = fun aexp ->
    match aexp with
    | Lv lv ->
      (match lv with
      | Arr (x, e) -> aholes'' e 
      )
    | BinOpLv (_, e1, e2) ->
      let t = aholes'' e1 in
      if (BatSet.is_empty t) then aholes'' e2 else t
    | AHole _ -> BatSet.singleton aexp
    | _ -> BatSet.empty
  in aholes cmd

let find_bholes : prog -> bexp BatSet.t
= fun (_, cmd, _) ->
  let rec bholes : cmd -> bexp BatSet.t
  = fun cmd ->
    match cmd with
    | Seq (c1, c2) ->
      let t = bholes c1 in
      if (BatSet.is_empty t) then bholes c2 else t
    | If (b, c1, c2) ->
      let t = bholes' b in
      if (BatSet.is_empty t) then
        let t= bholes c1 in
        if (BatSet.is_empty t) then
          bholes c2
        else t
      else t
    | While (b,c) ->
      let t = bholes' b in
      if (BatSet.is_empty t) then bholes c else t
    | _ -> BatSet.empty
  and bholes' : bexp -> bexp BatSet.t
  = fun bexp ->
    match bexp with
    | Not b -> bholes' b
    | Or (b1, b2) | And (b1, b2) ->
      let t = bholes' b1 in
      if (BatSet.is_empty t) then bholes' b2 else t
    | BHole _ -> BatSet.singleton bexp
    | _ -> BatSet.empty
  in bholes cmd

let find_choles : prog -> cmd BatSet.t
= fun (_, cmd, _) ->
  let rec choles : cmd -> aexp BatSet.t
  = fun cmd ->
    match cmd with
    | Seq (c1, c2) ->
      let t = choles c1 in
      if (BatSet.is_empty t) then choles c2 else t
    | If (_, c1, c2) ->
      let t = find_choles c1 in
      if (BatSet.is_empty t) then choles c2 else t
    | While (_, c) -> choles c1
    | CHole _ -> BatSet.singleton cmd
    | _ -> BatSet.empty
  in choles cmd

let next : components -> int list -> lv list -> prog -> prog BatSet.t
= fun components int_comps lv_comps pgm ->
  let (acomps, bcomps, ccomps) = components in
  let aholes = find_aholes prog in
  let bholes = find_bholes prog in
  let choles = find_choles prog in
  let next_a = BatSet.fold (fun ah acc -> BatSet.union acc (nextof_a lv_comps (pgm, ah) acomps)) aholes BatSet.empty in
  let next_b = BatSet.fold (fun bh acc -> BatSet.union acc (nextof_b lv_comps (pgm, bh) bcomps)) bholes BatSet.empty in
  let next_c = BatSet.fold (fun ch acc -> BatSet.union acc (nextof_c lv_comps (pgm, ch)ccomps)) choles BatSet.empty in
  BatSet.union next_a (BatSet.union next_b next_c)   

let is_solution : prog -> example list -> bool
= fun pgm examples -> 
  List.for_all (fun (i,o) ->
    try
      Imp.run pgm i = o
    with
      | _ -> false
  ) examples

let is_closed : prog -> bool
= fun pgm -> BatSet.is_empty (aholes pgm) && BatSet.is_empty (bholes pgm) && BatSet.is_empty (choles pgm)

let rec work : components -> example list -> int list -> lv list -> Workset.t -> pgm option
= fun exp_set examples int_comps lv_comps workset ->
  iter := !iter + 1;
  if !iter mod 10000 = 0 && not !Options.simple
  then
    begin 
      print_string ("Iter : " ^ (string_of_int !iter) ^ " ");
      print_endline ((Workset.workset_info workset) ^ (" Total elapsed : " ^ (string_of_float (Sys.time () -. !start_time))))
    end;
  if Sys.time () -. !start_time > 3600.0 then None
  else
  match Workset.choose workset with
  | None -> None
  | Some (pgm, remaining_workset) ->
    if is_closed pgm then
      if is_solution pgm examples then Some pgm(*(equivalence lv_comps pgm)*)
      else work examples int_comps lv_comps remaining_workset
    else 
      if Abs.hopeless pgm examples lv_comps then work examples int_comps lv_comps remaining_workset
      else 

        let exp_set = update_components exp_set in

        let nextstates = next exp_set int_comps lv_comps pgm in
        (*let nextstates = BatSet.filter (fun ns -> not (infinite_possible ns)) nextstates in
        let nextstates = BatSet.map (fun ns -> equivalence lv_comps ns) nextstates in*)
        let new_workset = BatSet.fold Workset.add nextstates remaining_workset in 
          work examples int_comps lv_comps new_workset
   
let synthesize : components -> example list -> pgm -> int list -> lv list -> pgm option
= fun components examples pgm int_comps lv_comps ->
  let workset = Workset.add pgm Workset.empty in
    work components examples int_comps lv_comps workset
