open Imp
open Normalize

type hole = A of aexp | B of bexp | C of cmd

module Workset = struct
  type work = int * prog

  module OrderedType = struct
    type t = work
    let compare (rank1, p1) (rank2, p2) =
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
  = fun pgm (_,sset) -> BatSet.mem (ts_pgm_onerow pgm) sset

  let add : work -> t -> t
  = fun (rank, pgm) (heap,sset) ->
    try
      if explored pgm (heap,sset) then (heap,sset)
      else
        (Heap.add (rank,pgm) heap, BatSet.add (ts_pgm_onerow pgm) sset)
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
    | While (b, c) -> While (replace_a' b ah acandi, replace_a c ah acandi)
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
      | Var x -> aexp
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
    | While (b, c) -> While (replace_b' b bh bcandi, replace_b c bh bcandi)
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
    | _ -> cmd
  in (args, (replace_c cmd ch ccandi), res)

let gen_nextstates_a : aexp BatSet.t -> (Workset.work * aexp) -> Workset.work BatSet.t
= fun candidates ((rank, p), ah) ->
  BatSet.fold (fun acandi acc ->
    BatSet.add (rank, replace_aexp p ah acandi) acc
  ) candidates BatSet.empty

let gen_nextstates_b : bexp BatSet.t -> (Workset.work * bexp) -> Workset.work BatSet.t
= fun candidates ((rank,p), bh) ->
  BatSet.fold (fun bcandi acc ->
    BatSet.add (rank, replace_bexp p bh bcandi) acc
  ) candidates BatSet.empty

let gen_nextstates_c : cmd BatSet.t -> (Workset.work * cmd) -> Workset.work BatSet.t
= fun candidates ((rank,p), ch) ->
  BatSet.fold (fun ccandi acc ->
    BatSet.add (rank, replace_cmd p ch ccandi) acc
  ) candidates BatSet.empty

let nextof_a : lv list -> Workset.work * aexp -> aexp BatSet.t -> Workset.work BatSet.t
= fun lv_comps ((rank, p), ah) a_comps ->
  let variable = List.fold_left (fun acc lv -> 
    match lv with
    | Var x -> BatSet.add (Lv lv) acc
    | Arr (x, _) -> BatSet.add (Lv (Arr (x, ahole()))) acc) BatSet.empty lv_comps in
  gen_nextstates_a (BatSet.union variable a_comps) ((rank,p), ah)

let nextof_b : lv list -> Workset.work * bexp -> bexp BatSet.t -> Workset.work BatSet.t
= fun lv_comps (p, bh) b_comps ->
  gen_nextstates_b b_comps (p, bh)

let nextof_c : lv list -> Workset.work * cmd -> cmd BatSet.t -> Workset.work BatSet.t
  = fun lv_comps (p, ch) c_comps ->
  gen_nextstates_c c_comps (p, ch)


  (* Pruning Infinite case *)

let rec infinite_possible : Workset.work -> bool
= fun (_, (, cmd, _)) -> infinite cmd

and infinite : cmd -> bool
= fun cmd ->
  match cmd with
  | Seq (c1, c2) | If(_, c1, c2) -> infinite c1 || infinite c2
  | While (b, c) ->
  
(*let rec infinite_possible : Workset.work -> bool
= fun (_, (_,cmd,_)) -> infinite cmd

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

(* if cannot be dtermined -> say true : to be conservative *)  
and permitted_last : bexp -> cmd -> bool
= fun bexp cmd ->
  match bexp, cmd with
  | BHole _,_
  | Gt _,CHole _ 
  | Lt _,CHole _ -> true (* to wait *)
  | Gt (Lv (Var x), Int _),Assign (Var x',AHole _) 
    when BatString.equal x x' -> true (* to wait *) 
  | Gt (Lv (Var x), Int _),Assign (Var x',BinOpLv (Minus, Lv (Var x''), Int n)) 
    when BatString.equal x x' && BatString.equal x' x'' && n>0 -> true
  | Lt (Lv (Var x), Int _),Assign (Var x',AHole _)
    when BatString.equal x x' -> true (* to wait *)
  | Lt (Lv (Var x), Int _),Assign (Var x',BinOpLv (Plus, Lv (Var x''), Int n))
    when BatString.equal x x' && BatString.equal x' x'' && n>0 -> true 
  | _ -> false
  
and cntvar_redefined : bexp -> cmd list -> bool
= fun bexp cmdlist ->
  let assign_set = List.fold_left (fun acc cmd ->
    match cmd with
    | Assign _ -> BatSet.add cmd acc
    | Seq (c1,c2)     
    | If (_,c1,c2) -> BatSet.union acc (BatSet.union (set_of_assign c1) (set_of_assign c2))
    | While (_,c) -> BatSet.union (set_of_assign c) acc
    | _ -> acc
  ) BatSet.empty cmdlist in
    begin
     match bexp with
      | Gt (Lv lv1,Lv lv2) -> BatSet.exists (fun cmd -> match cmd with | Assign (lv,_) when lv = lv1 || lv = lv2 -> true | _ -> false) assign_set 
      | Lt (Lv lv1,Lv lv2) -> BatSet.exists (fun cmd -> match cmd with | Assign (lv,_) when lv = lv1 || lv = lv2 -> true | _ -> false) assign_set
      | _ -> false
    end

and set_of_assign : cmd -> cmd BatSet.t 
= fun cmd ->
  match cmd with
  | Assign _ -> BatSet.singleton cmd
  | Seq (c1,c2) 
  | If (_,c1,c2) -> BatSet.union (set_of_assign c1) (set_of_assign c2)
  | While (_,c) -> set_of_assign c
  | _ -> BatSet.empty

and list_of_cmd : cmd -> cmd list 
= fun cmd -> 
  match cmd with
  | Seq (c1,c2) -> (list_of_cmd c1)@(list_of_cmd c2)
  | _ -> [cmd]*)


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
  | Not b -> Not (bhole())
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
  let a_comps = BatSet.map update_components_aexp a_comps in
  let b_comps = BatSet.map update_components_bexp b_comps in
  let c_comps = BatSet.map update_components_cmd c_comps in
  (a_comps, b_comps, c_comps)
    

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
  and aholes' : bexp -> aexp BatSet.t
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
    | Not b -> aholes' b
    | Or (b1, b2) | And (b1, b2) ->
      let t = aholes' b1 in
      if (BatSet.is_empty t) then aholes' b2 else t
    | _ -> BatSet.empty
  and aholes'' : aexp -> aexp BatSet.t
  = fun aexp ->
    match aexp with
    | Lv lv ->
      (match lv with
      | Var x -> BatSet.empty
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
  let rec choles : cmd -> cmd BatSet.t
  = fun cmd ->
    match cmd with
    | Seq (c1, c2) ->
      let t = choles c1 in
      if (BatSet.is_empty t) then choles c2 else t
    | If (_, c1, c2) ->
      let t = choles c1 in
      if (BatSet.is_empty t) then choles c2 else t
    | While (_, c) -> choles c
    | CHole _ -> BatSet.singleton cmd
    | _ -> BatSet.empty
  in choles cmd

let next : components -> lv list -> Workset.work -> Workset.work BatSet.t
= fun components lv_comps (rank, pgm) ->
  let (acomps, bcomps, ccomps) = components in
  let aholes = find_aholes pgm in
  let bholes = find_bholes pgm in
  let choles = find_choles pgm in
  let next_a = BatSet.fold (fun ah acc -> BatSet.union acc (nextof_a lv_comps ((rank, pgm), ah) acomps)) aholes BatSet.empty in
  let next_b = BatSet.fold (fun bh acc -> BatSet.union acc (nextof_b lv_comps ((rank,pgm), bh) bcomps)) bholes BatSet.empty in
  let next_c = BatSet.fold (fun ch acc -> BatSet.union acc (nextof_c lv_comps ((rank,pgm), ch) ccomps)) choles BatSet.empty in
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
= fun pgm -> BatSet.is_empty (find_aholes pgm) && BatSet.is_empty (find_bholes pgm) && BatSet.is_empty (find_choles pgm)

let rec work : components -> example list -> lv list -> Workset.t -> prog option
= fun exp_set examples lv_comps workset ->
(*
  iter := !iter + 1;
  if !iter mod 10000 = 0 && not !Options.simple
  then
    begin 
      print_string ("Iter : " ^ (string_of_int !iter) ^ " ");
      print_endline ((Workset.workset_info workset) ^ (" Total elapsed : " ^ (string_of_float (Sys.time () -. !start_time))))
    end;
  if Sys.time () -. !start_time > 3600.0 then None
  else*)
  match Workset.choose workset with
  | None -> None
  | Some ((rank,pgm), remaining_workset) ->
    print_endline (ts_pgm_onerow pgm) ;
    if is_closed pgm then
      if is_solution pgm examples then Some pgm(*(equivalence lv_comps pgm)*)
      else work exp_set examples lv_comps remaining_workset
    else 
      if Abs.hopeless pgm examples lv_comps then let _ = print_endline "Hopeless"; in  work exp_set examples lv_comps remaining_workset
      else 
        let exp_set = update_components exp_set in

        let nextstates = next exp_set lv_comps (rank,pgm) in
        (*let nextstates = BatSet.filter (fun ns -> not (infinite_possible ns)) nextstates in*)
        let nextstates = BatSet.map (fun ns -> equivalence lv_comps ns) nextstates in

        let new_workset = BatSet.fold Workset.add nextstates remaining_workset in 
          work exp_set examples lv_comps new_workset
   
let synthesize : components -> example list -> Workset.work BatSet.t  -> lv list -> prog option
= fun components examples pgm_set lv_comps ->
  let workset = BatSet.fold (fun t set-> Workset.add t set) pgm_set Workset.empty in
    work components examples lv_comps workset
