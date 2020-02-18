open Imp

exception Arg_exception

let make_lv_list : var list -> var list -> int list -> lv list
= fun int_var_comps arr_var_comps int_comps -> 
  List.fold_left (fun acc x -> (Var x)::acc) [] int_var_comps @
  List.fold_left (fun acc1 x ->
    acc1@(List.fold_left (fun acc2 y -> (Arr (x,y))::acc2) [] int_var_comps)
  ) [] arr_var_comps 

let main () =
  let usageMsg = "./main.native -input filename" in
  let _ = Arg.parse options (fun s->()) usageMsg in
  let file_channel = open_in !inputfile in
  let lexbuf = Lexing.from_channel file_channel in
  let (examples, pgm, int_comps, int_var_comps, arr_var_comps) = Parser.resource Lexer.start lexbuf in
  let lv_comps = make_lv_list int_var_comps arr_var_comps int_comps in
  if not !Options.simple then
    match pgm with 
    | None -> print_endline "Fail to Synthesize"
    | Some pgm ->
      let ranked_prog_set = Localize.localization (lv_comps, pgm, )