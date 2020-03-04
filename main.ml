open Imp
open Options

exception Arg_exception

(*let make_lv_list : var list -> var list -> int list -> lv list
= fun int_var_comps arr_var_comps int_comps -> 
  List.fold_left (fun acc x -> (Var x)::acc) [] int_var_comps @
  List.fold_left (fun acc1 x ->
    acc1@(List.fold_left (fun acc2 y -> (Arr (x,y))::acc2) [] int_var_comps)
  ) [] arr_var_comps *)

let rec print_component : components -> unit
= fun (aexp, bexp, cmd) ->
  let _ = BatSet.map (fun x -> let _ = print_endline (ts_aexp x); in ()) aexp in
  let _ = BatSet.map (fun x -> let _ = print_endline (ts_bexp x); in ()) bexp in
  let _ = BatSet.map (fun x -> let _ = print_endline (ts_cmd_onerow x); in ()) cmd in ()

let make_lv_list : var list -> var list -> lv list
= fun int_var_comps arr_var_comps ->
  List.fold_left (fun acc x -> (Var x)::acc) [] int_var_comps @
  List.fold_left (fun acc x -> (Arr (x, AHole (0)))::acc) [] arr_var_comps

let main () =
  let usageMsg = "./main.native -input filename" in
  let _ = Arg.parse options (fun s->()) usageMsg in
  let file_channel = open_in !inputfile in
  let lexbuf = Lexing.from_channel file_channel in
  let (examples, pgm, int_comps, int_var_comps, arr_var_comps) = Parser.resource Lexer.start lexbuf in
  let file2 = open_in !solutionfile in
  let lexbuf = Lexing.from_channel file2 in
  let (a, solution, b, c, d) = Parser.resource Lexer.start lexbuf in
  (*let lv_comps = make_lv_list int_var_comps arr_var_comps int_comps in*)
  if not !Options.simple then
    (*match cmd with 
    | None -> print_endline "Fail to Synthesize"
    | Some cmd ->*)
      let ranked_prog_set = Localize.localization pgm examples in

      let _ = print_endline "After Ranking" in

      let components = Comp.extract_component solution in
      
      let _ = print_component components in 

    
      let lv_comps = make_lv_list int_var_comps arr_var_comps in
      let pgm = Synthesizer.synthesize components examples ranked_prog_set lv_comps in
      begin
        match pgm with
        | None -> print_endline "Fail to Synthesize"
        | Some pgm -> print_endline (ts_pgm_rows pgm);
      end
      
      
let _ = main ()