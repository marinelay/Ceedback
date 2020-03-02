open Imp

let rec find_variable : cmd -> var BatSet.t -> var BatSet.t -> (var BatSet.t * var BatSet.t)
= fun cmd var arr ->
  match cmd with
  | Assign (Var x, _) -> (BatSet.add x var, arr)
  | Assign (Arr (x, _), _) -> (var, BatSet.add x arr)
  | Seq (c1, c2) ->
    let (var, arr) = find_variable c1 var arr in
    find_variable c2 var arr
  | If (_, c1, c2) ->
    let (var, arr) = find_variable c1 var arr in
    find_variable c2 var arr
  | While (_, c) ->
    find_variable c var arr

let extract_assign_variables : prog -> (var BatSet.t * var BatSet.t) (* var arr *)
= fun (args, cmd, res) -> find_variable cmd BatSet.empty BatSet.empty

let extract_variables : prog -> var list -> var list -> (var BatSet.t * var BatSet.t) (* var arr *)
= fun sub var arr ->
  let (sub_var, sub_arr) = extract_assign_variables sub in
  let var = List.fold_left (fun lst x -> BatSet.add x lst) sub_var var in
  let arr = List.fold_left (fun lst x -> BatSet.add x lst) sub_arr arr in
  (var, arr)

let add_component_aexp : aexp -> components -> components
= fun aexp (a_comps, b_comps, c_comps) -> (BatSet.add aexp a_comps, b_comps, c_comps)

and add_component_bexp : bexp -> components -> components
= fun bexp (a_comps, b_comps, c_comps) -> (a_comps, BatSet.add bexp b_comps, c_comps)

and add_component_cmd : cmd -> components -> components
= fun cmd (a_comps, b_comps, c_comps) -> (a_comps, b_comps, BatSet.add cmd c_comps)


let rec find_component_aexp : aexp -> components -> components
= fun aexp comps ->
  match aexp with
  | Int n -> add_component_aexp aexp comps
  | Lv lv -> find_component_lv lv comps
  | BinOpLv (bop, e1, e2) ->
    let comps = find_component_aexp e1 comps in
    let comps = find_component_aexp e2 comps in
    add_component_aexp (BinOpLv (bop, AHole(0), AHole(0))) comps
  | _ -> comps

and find_component_lv : lv -> components -> components
= fun lv comps ->
  match lv with
  | Var x -> comps(*BatSet.add (Lv lv) comps*)
  | Arr (x, e) -> find_component_aexp e comps
    (*let comps = find_component_aexp e comps in
    BatSet.add (Lv (Arr (x, AHole(0)))) comps*)
  
and find_component_bexp : bexp -> components -> components
= fun bexp comps ->
  match bexp with
  | True | False -> add_component_bexp bexp comps
  | Gt (e1, e2) ->
    let comps = find_component_aexp e1 comps in
    let comps = find_component_aexp e2 comps in
    add_component_bexp (Gt (AHole (0), AHole(0))) comps
  | Lt (e1, e2) ->
    let comps = find_component_aexp e1 comps in
    let comps = find_component_aexp e2 comps in
    add_component_bexp (Lt (AHole (0), AHole(0))) comps
  | Eq (e1, e2) ->
    let comps = find_component_aexp e1 comps in
    let comps = find_component_aexp e2 comps in
    add_component_bexp (Eq (AHole (0), AHole(0))) comps
  | Not b ->
    let comps = find_component_bexp b comps in
    add_component_bexp (Not (BHole(0))) comps
  | Or (e1, e2) ->
    let comps = find_component_bexp e1 comps in
    let comps = find_component_bexp e2 comps in
    add_component_bexp (Or (BHole (0), BHole(0))) comps
  | And (e1, e2) ->
    let comps = find_component_bexp e1 comps in
    let comps = find_component_bexp e2 comps in
    add_component_bexp (And (BHole (0), BHole(0))) comps
  | _ -> comps

and find_component_cmd : cmd -> components -> components
= fun cmd comps ->
  match cmd with
  | Assign (x, e) ->
    let comps = find_component_lv x comps in
    let comps = find_component_aexp e comps in 
    add_component_cmd (Assign (x, AHole(0))) comps 
  | Seq (c1, c2) ->
    let comps = find_component_cmd c1 comps in
    let comps = find_component_cmd c2 comps in
    add_component_cmd (Seq (CHole (0), CHole(0))) comps
  | If (b, c1, c2) ->
    let comps = find_component_bexp b comps in
    let comps = find_component_cmd c1 comps in
    let comps = find_component_cmd c2 comps in
    add_component_cmd (If (BHole (0), CHole(0), CHole(0))) comps
  | While (b, c) ->
    let comps = find_component_bexp b comps in
    let comps = find_component_cmd c comps in
    add_component_cmd (While (BHole(0), CHole(0))) comps
  | _ -> comps

let extract_component : prog -> components
= fun (args, cmd, res) -> find_component_cmd cmd (BatSet.empty, BatSet.empty, BatSet.empty)