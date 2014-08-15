(**************************************************************************)
(*                                                                        *)
(*                              FLAX                                      *)
(*                                                                        *)
(*                       Copyright (C) 2014                               *)
(*                                                                        *)
(*                         Sylvain Conchon                                *)
(*                                                                        *)
(*                     Universite Paris-Sud 11                            *)
(*                                                                        *)
(*                                                                        *)
(*  This file is distributed under the terms of the Apache Software       *)
(*  License version 2.0                                                   *)
(*                                                                        *)
(**************************************************************************)

open Format
open Options
open Common
open Ast

module Make (M : BIN) = struct

  type info = {
    t_locs : S.t;
    t_protos : S.t;
    t_opcodes : S.t
  }
 

  let pp_S fmt = S.iter (fprintf fmt "%s, ")
  let pp_L sep fmt l = List.iter (fun x -> fprintf fmt "%s%s " x sep) l
  let pp_info fmt i = 
    let locs = S.elements i.t_locs in
    fprintf fmt "mtype = { %a }\n" (pp_L ",") locs;
    let last = match List.rev locs with x::_ -> x | [] -> assert false in
    List.iter (fun l -> fprintf fmt "#define _%s %s - %s\n" l l last) locs;
    fprintf fmt "\n";
    fprintf fmt "mtype = { %a }\n" pp_S i.t_protos;
    fprintf fmt "mtype = { %a }\n\n" pp_S i.t_opcodes

  let extract_protos r protos = 
    let protos = 
      List.fold_left 
	(fun protos g ->
	  match g with
	    | Can_add msg -> S.add msg.msg_bin_proto protos
	    | Can_rm pat -> S.add pat.msg_bin_pat_proto protos
	    | Other_literal -> protos) 
	protos r.rule_guard
    in
    List.fold_left 
      (fun protos g ->
	match g with
	  | Add msg -> S.add msg.msg_bin_proto protos
	  | Rm pat -> S.add pat.msg_bin_pat_proto protos
	  | Other_action -> protos) 
      protos r.rule_action

  let extract_opcodes r opcodes = 
    let opcodes = 
      List.fold_left 
	(fun opcodes g ->
	  match g with
	    | Can_add msg -> S.add msg.msg_bin_opcode opcodes
	    | Can_rm pat -> S.add pat.msg_bin_pat_opcode opcodes
	    | Other_literal -> opcodes) 
	opcodes r.rule_guard
    in
    List.fold_left 
      (fun opcodes g ->
	match g with
	  | Add msg -> S.add msg.msg_bin_opcode opcodes
	  | Rm pat -> S.add pat.msg_bin_pat_opcode opcodes
	  | Other_action -> opcodes) 
      opcodes r.rule_action

  let collect_info l = 
    List.fold_left 
      (fun info r -> 
	let rlocs = S.union r.rule_loc_in r.rule_loc_out in
	{ t_locs = S.union rlocs info.t_locs;
	  t_protos = extract_protos r info.t_protos;
	  t_opcodes =  extract_opcodes r info.t_opcodes; }) 
      { t_locs  = S.empty; 
	t_protos = S.empty;
	t_opcodes = S.empty; } l

  let pp_agents fmt l = 
    fprintf fmt "mtype = { %a }\n\n" (pp_L ",") l  

  let pp_state fmt info =
    fprintf fmt "\nbool L[%d]\n" (S.cardinal info.t_locs)

  let pp_init fmt prog info = 
    fprintf fmt "\n/* Let's go */\n\n";
    fprintf fmt "init \n{\n d_step{\n";
    S.iter (fprintf fmt " L[_%s] = true; \n") prog.init_locs;
    S.iter (fprintf fmt " L[_%s] = false; \n") 
      (S.diff info.t_locs prog.init_locs);
  (*  List.iter (fprintf fmt " run AGT_%s ();\n") prog.agents;*)
    fprintf fmt "}}\n\n"

  let gen_idents s = 
    let locs = S.elements s in
    let ids = ref [] in
    for i = 1 to List.length locs do
      ids := ("i"^string_of_int i) :: !ids
    done;
    List.combine (List.rev !ids) locs
  
  let pp_loc_in fmt s = 
    let l = S.elements s in
    match l with
      | [] -> ()
      | [x] ->  fprintf fmt " L[_%s] " x
      | x :: l ->
	fprintf fmt " L[_%s]" x;
	List.iter (fprintf fmt " && L[_%s]") l

  let is_a_message r = 
    List.exists 
      (fun lit -> match lit with 
	  Can_add _ | Can_rm _ -> true | _ -> false) r.rule_guard
   
  let pp_guard fmt r = 
    List.iter 
      (fun lit -> match lit with
	| Other_literal -> ()

	| Can_add msg ->
	  M.pp_can_add fmt r.rule_agent msg

	| Can_rm pat ->
	  M.pp_can_rm fmt r.rule_agent pat

      ) r.rule_guard


  let pp_loc_out fmt r = 
    S.iter 
      (fun l -> 
	if not (S.mem l r.rule_loc_out) then
	  fprintf fmt "\tL[_%s] = false;\n" l) r.rule_loc_in;
    S.iter 
      (fun l ->
	if not (S.mem l r.rule_loc_in) then 
	  fprintf fmt "\tL[_%s] = true;\n" l) r.rule_loc_out
    
  let pp_action fmt r = 
    List.iter
      (fun act -> match act with
	| Other_action -> ()
	| Add msg -> 
	  M.pp_add fmt r.rule_agent msg

	| Rm pat -> 
	  M.pp_rm fmt r.rule_agent pat

      ) r.rule_action

  let rule fmt r = 
    fprintf fmt "\n /* %s */\n" r.rule_name ;
    fprintf fmt "  :: atomic { %a" pp_loc_in r.rule_loc_in;
    if r.rule_guard <> [] && not (S.is_empty r.rule_loc_in) then
      fprintf fmt " && ";
    fprintf fmt "%a ->\n" pp_guard r;
    fprintf fmt "\n%a" pp_loc_out r;
    fprintf fmt "%a\n    }\n" pp_action r


  let rules fmt prog info = 
    List.iter 
      (fun a -> 
	let rl = List.filter (fun r -> r.rule_agent = a) prog.rules in
	fprintf fmt "\nactive proctype AGT_%s ()\n{\n" a;
	fprintf fmt " do\n\n";
	List.iter (rule fmt) rl;
	fprintf fmt "\n od\n}\n") prog.agents

  let compile fmt prog = 
    let info = collect_info prog.rules in
    pp_info fmt info;
    pp_agents fmt prog.agents;
    pp_state fmt info;
    M.pp fmt prog;
    pp_init fmt prog info ;
    rules fmt prog info;
    fprintf fmt "@."
end
  
