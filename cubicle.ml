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
 
  let pp_S fmt = S.iter (fprintf fmt "| %s ")
  let pp_info fmt i = 
    fprintf fmt "type proto = %a\n" pp_S i.t_protos;
    fprintf fmt "type opcode = %a\n" pp_S i.t_opcodes

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

  let pp_L sep fmt l = List.iter (fprintf fmt "%s %s " sep) l

  let pp_agents fmt l = 
    fprintf fmt "\ntype agent = %a\n\n" (pp_L "|") l

  let pp_state fmt info =
    fprintf fmt "(* Bit-blasting state *)\n\n";
    S.iter (fprintf fmt "var %s : bool\n") info.t_locs;
    fprintf fmt "var Deadlock : bool\n"

  let pp_fab0 fmt () =
    fprintf fmt "\n(* Fabric 0 :*)\n\n";
    fprintf fmt "array Q[proc] : bool\n";
    fprintf fmt "array Q_proto[proc] : proto\n";
    fprintf fmt "array Q_opcode[proc] : opcode\n";
    fprintf fmt "array Q_from[proc] : agent\n";
    fprintf fmt "array Q_to[proc] : agent\n"

  let pp_init fmt prog info = 
    fprintf fmt "\n(* Let's go *)\n\n";
    fprintf fmt "init (z) {\nQ[z] = False \n";
    S.iter (fprintf fmt "&& %s = True \n") prog.init_locs;
    S.iter (fprintf fmt "&& %s = False \n") (S.diff info.t_locs prog.init_locs);
    fprintf fmt "&& Deadlock = False";
    fprintf fmt "}\n\n"

  let pp_unsafe fmt () = 
    fprintf fmt "\n(* Don't know what property to check at the moment *)\n\n";
    fprintf fmt "unsafe (z) { Deadlock = True }\n\n"

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
      | [] -> fprintf fmt "\n"
      | [x] ->  fprintf fmt "%s = True" x
      | x :: l ->
	fprintf fmt "%s = True" x;
	List.iter (fprintf fmt " && %s = True") l

  let is_a_message r = 
    List.exists 
      (fun lit -> match lit with 
	  Can_add _ | Can_rm _ -> true | _ -> false) r.rule_guard
   
  let pp_guard fmt r = 
    List.iter 
      (fun lit -> match lit with
	| Other_literal -> ()
	| Can_add msg ->
	  fprintf fmt 
	    "\t  Q[i] = False && forall_other z. (i<=z || Q[z] = True) "
	| Can_rm pat ->
	  fprintf fmt 
	    "\t  Q[i] = True && forall_other z. (i<=z || Q[z] = False) &&\n";
	  fprintf fmt
	    "\t  Q_proto[i] = %s && Q_opcode[i] = %s && Q_to[i] = %s " 
	    pat.msg_bin_pat_proto
	    pat.msg_bin_pat_opcode
	    pat.msg_bin_pat_to;
	  match pat.msg_bin_pat_from with
	    | Pat_Const c -> fprintf fmt "&& Q_from[i]=%s " c
	    | Pat_Var _ -> ()
      ) r.rule_guard


  let pp_loc_out fmt r = 
    S.iter 
      (fun l -> 
	if not (S.mem l r.rule_loc_out) then
	  fprintf fmt " %s := False;\n" l) r.rule_loc_in;
    S.iter 
      (fun l ->
	if not (S.mem l r.rule_loc_in) then 
	  fprintf fmt " %s := True;\n" l) r.rule_loc_out
    
  let pp_action fmt r = 
    List.iter
      (fun act -> match act with
	| Other_action -> ()
	| Add msg -> 
	  fprintf fmt " Q[i] := True ;\n";
	  fprintf fmt " Q_proto[i] := %s ;\n" msg.msg_bin_proto;
	  fprintf fmt " Q_opcode[i] := %s ;\n" msg.msg_bin_opcode;
	  fprintf fmt " Q_from[i] := %s ;\n" msg.msg_bin_from;
	  fprintf fmt " Q_to[i] := %s ;\n" msg.msg_bin_to

	| Rm pat -> 
	  fprintf fmt " Q[i] := False ;\n"
      ) r.rule_action

  let rule fmt r = 
    fprintf fmt "\ntransition %s (%s)\n" 
      r.rule_name 
      (if is_a_message r then "i" else "");
    fprintf fmt "requires {%a" pp_loc_in r.rule_loc_in;
    if r.rule_guard <> [] && not (S.is_empty r.rule_loc_in) then
      fprintf fmt " && \n";
    fprintf fmt "%a}\n" pp_guard r;
    fprintf fmt "{\n";
    fprintf fmt "%a" pp_loc_out r;
    fprintf fmt "%a\n}\n" pp_action r


  let rules fmt l = 
    List.iter (rule fmt) l

  let compile fmt prog = 
    let info = collect_info prog.rules in
    pp_info fmt info;
    pp_agents fmt prog.agents;
    pp_state fmt info;
    pp_fab0 fmt ();
    pp_init fmt prog info ;
    pp_unsafe fmt ();
    rules fmt prog.rules
  
end
