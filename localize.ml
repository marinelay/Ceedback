open Imp
open Labeling

(* set of execution traces *)
type trace_set = int BatSet.t

let empty_set = BatSet.empty
let extend_set = BatSet.add

let trace_set = ref empty_set
let init_set () = (trace_set := empty_set)

module labeled_Memory = struct
  type t = (var,labeled_value) BatMap.t
  let add = BatMap.add
  let mem = BatMap.mem
  let find x m = BatMap.find x m 
  let empty = BatMap.empty
end

(* Find counter exampels *)
let rec is_counter_example : prog -> example -> bool
= fun pgm (input, output) ->
  try
    let result = Imp.run pgm input in
    not (Eval.value_equality result output)
  with _ -> true

let rec find_counter_examples : prog -> examples -> examples * examples
= fun pgm examples -> List.partition (is_counter_example pgm) examples

exception BufferOverFlow

(*labeled eval *)
let rec eval_aexp : labeled_aexp -> labeled_Memory.t -> labeled_value
= fun aexp mem ->
  (trace_set := extend_set label !trace_set);
  match aexp with
  | Int n -> VInt n
  | Lv lv -> eval_lv lv mem
  | BinOpLv -> (bop, e1, e2) -> eval_bop bop (eval_aexp e1 mem) (eval_aexp e2 mem)
  | AHole _ -> raise (Failure "eval_aexp : hole encountered")

and eval_lv : labeled_lv -> labeled_Memory.t -> labeled_value
= fun lv mem ->
  (trace_set := extend_set label !trace_set);
  match lv with
  | Var x -> Memory.find x mem
  | Arr (x,e) ->
    (match Memory.find x mem, (eval_aexp e mem) with
    | VArr lst, VInt idx ->
      let size = List.length lst in
        if idx < 0 || idx >= size then raise BufferOverFlow
        else VInt (List.nth lst idx)
    | _ -> raise (Failure "imp.ml : eval_lv - variable type error")) 
    )

and value2int : labeled_value -> int
= fun value -> 
  match value with 
  | VInt n -> n 
  | _ -> raise (Failure "array value is not integer type")

and eval_bop : bop -> labeled_value -> labeled_value -> labeled_value
= fun bop v1 v2 ->
  match bop with
  | Plus  -> VInt ((value2int v1) + (value2int v2))
  | Minus -> VInt ((value2int v1) - (value2int v2))
  | Mult  -> VInt ((value2int v1) * (value2int v2))
  | Div   -> VInt ((value2int v1) / (value2int v2))
  | Mod   -> VInt ((value2int v1) mod (value2int v2))

and eval_bexp : labeled_bexp -> labeled_Memory.t -> bool
= fun bexp mem ->
  (trace_set := extend_set label !trace_set);
  match bexp with
  | True -> true 
  | False -> false
  | Gt (e1,e2) -> (value2int (eval_aexp e1 mem)) > (value2int (eval_aexp e2 mem))
  | Lt (e1,e2) -> (value2int (eval_aexp e1 mem)) < (value2int (eval_aexp e2 mem))
  | Eq (e1,e2) -> (value2int (eval_aexp e1 mem)) = (value2int (eval_aexp e2 mem)) 
  | Not b -> not (eval_bexp b mem) 
  | Or (b1,b2) -> (eval_bexp b1 mem) || (eval_bexp b2 mem)
  | And (b1,b2) -> (eval_bexp b1 mem) && (eval_bexp b2 mem)
  | BHole _ -> raise (Failure "eval_bexp: hole encountered")

and eval_cmd : labeled_cmd -> labeled_Memory.t -> labeled_Memory.t
= fun cmd mem ->
  (trace_set := extend_set label !trace_set);
  match cmd with
  | Assign (Var x, aexp) -> labeled_Memory.add x (eval_aexp aexp mem) mem
  | Assign (Arr (x, e), aexp) ->
    (match labeled_Memory.find x mem, (eval_aexp e mem) with
    | VArr lst, VInt idx ->
      let size = List.length lst ->
        if (idx < 0) || (idx >= size) then raise BufferOverFlow
        else
          labeled_Memory.add x (VArr (BatList.modify_at idx (fun v -> value2int (eval_aexp aexp mem)) lst)) mem
    | _ -> raise (Failure "imp.ml - eval_cmd : variable type error"))
    )
  | Seq (c1, c2) -> eval_cmd c2 (eval_cmd c1 mem)
  | If (b, c1, c2) ->
    if eval_bexp b mem then eval_cmd c1 mem else eval_cmd c2 mem
  | While (b,c) ->
    if eval_bexp b mem then eval_cmd (eval_cmd c mem) else mem
  | Skip -> mem
  | CHole _ -> raise (Failure "eval_cmd: hole encountered")

let run : labeled_prog -> labeled_value list -> labeled_value (* input = value list *)
= fun (args,cmd,res) input_params ->
  let init_mem = 
  List.fold_left2 (fun mem x v -> labeled_Memory.add x v mem) labeled_Memory.empty args input_params in
    let r = labeled_Memory.find res (eval_cmd cmd init_mem) in
      r

let rec collect_execution_trace : labeled_prog -> example -> trace_set
= fun pgm (input, output) ->
  try
    let _  = run pgm input in
    !trace_set
  with _ -> !trace_set

let gen_label_map (ex : examples) (pgm : labeled_prog) : (int, int) BatMap.t =
	List.fold_left (fun map example ->
		let label_set = collect_execution_trace pgm example in
		BatSet.fold (fun label m ->
			if BatMap.mem label m then BatMap.add label ((BatMap.find label m)+1) m
			else BatMap.add label 1 m
		) label_set map
	) BatMap.empty ex

let weight : labeled_prog -> examples -> examples -> (int, float) BatMap.t
= fun l_pgm pos neg ->
	let counter_map = gen_label_map neg l_pgm in
	let pass_map = gen_label_map pos l_pgm in
	let counter_num = List.length neg in
	let pass_num = List.length pos in
	let weight_function = BatMap.foldi (fun label n result ->
		if(BatMap.mem label pass_map) then 
			let w = (float_of_int (BatMap.find label pass_map)) /. float_of_int(counter_num+pass_num) in
			BatMap.add label w result
		else BatMap.add label 0.0 result
	) counter_map BatMap.empty in
  weight_function

let cost_avg : (int, float) BatMap.t -> labeled_prog -> float
= fun w l_pgm->
	let pgm_set = BatMap.foldi (fun l _ acc ->
		let hole_pgm = gen_hole_cmd l l_pgm in
		BatSet.add (unlabeling_prog hole_pgm) acc
	) w BatSet.empty in
	let sum = BatSet.fold (fun pgm acc->
		let rank = (cost (unlabeling_prog l_pgm)) - (cost pgm) in
		acc +. (float_of_int rank)
	) pgm_set 0.0 in
	sum /. (float_of_int (BatSet.cardinal pgm_set))

let localization : prog -> examples -> unit
= fun pgm examples ->
  let (counter_examples,pass_examples) = find_counter_examples pgm examples in
  let l_pgm = Labeling.labeling_prog pgm in 
  let weight_function = weight l_pgm pass_examples counter_examples in
  