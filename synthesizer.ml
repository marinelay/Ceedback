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

let rec work : example list -> int list -> lv list -> Workset.t -> pgm option
= fun examples int_comps lv_comps workset ->
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
      if is_solution pgm examples then Some (equivalence lv_comps pgm)
      else work examples int_comps lv_comps remaining_workset
    else 
      if Abs.hopeless pgm examples lv_comps then work examples int_comps lv_comps remaining_workset
      else 
        let nextstates = next int_comps lv_comps pgm in
        let nextstates = BatSet.filter (fun ns -> not (infinite_possible ns)) nextstates in
        let nextstates = BatSet.map (fun ns -> equivalence lv_comps ns) nextstates in
        let new_workset = BatSet.fold Workset.add nextstates remaining_workset in 
          work examples int_comps lv_comps new_workset
   
let synthesize : example list -> pgm -> int list -> lv list -> pgm option
= fun examples pgm int_comps lv_comps ->
  let workset = Workset.add pgm Workset.empty in
    work examples int_comps lv_comps workset
