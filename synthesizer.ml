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