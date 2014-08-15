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

(* Pretty print of GALM rules *)
  
let pp_S fmt s = 
  fprintf fmt "{ ";
  S.iter (fprintf fmt "%s ") s;
  fprintf fmt "} "

let pp_msg fmt m = 
  fprintf fmt "%s_%s(%s,%s)" 
    m.msg_bin_proto m.msg_bin_opcode m.msg_bin_from m.msg_bin_to

let pp_msg_pat fmt p = 
  let from = match p.msg_bin_pat_from with Pat_Const s | Pat_Var s -> s in
  fprintf fmt "%s_%s(%s,%s)" 
    p.msg_bin_pat_proto p.msg_bin_pat_opcode from p.msg_bin_pat_to

let pp_lit fmt lit = 
  match lit with 
    | Can_add m ->
      fprintf fmt "can_add %a" pp_msg m
    | Can_rm p -> 
      fprintf fmt "can_rm %a" pp_msg_pat p
    | Other_literal -> ()

let pp_act fmt act = 
  match act with
    | Rm pat ->
      fprintf fmt "rm %a" pp_msg_pat pat
    | Add m ->
      fprintf fmt "add %a" pp_msg m
    | Other_action -> ()

let rec pp_L f fmt = function
  | [] -> ()
  | [s] -> fprintf fmt "%a" f s
  | s :: l -> fprintf fmt "%a %a" f s (pp_L f) l

let pp_rule fmt r = 
  fprintf fmt "rule %s (%s): \n" r.rule_name r.rule_agent;
  fprintf fmt "\t loc_in:%a\n" pp_S r.rule_loc_in;
  fprintf fmt "\t guards:%a\n" (pp_L pp_lit) r.rule_guard;
  fprintf fmt "\t actions:%a\n" (pp_L pp_act) r.rule_action;
  fprintf fmt "\t loc_out:%a\n" pp_S r.rule_loc_out

let pp fmt p =
  fprintf fmt "Init locations : %a\n@." pp_S p.init_locs;
  List.iter (fprintf fmt "%a\n" pp_rule) p.rules


(* GALM Interpretor *)    

exception Do_not_Match

let pattern_matching pat msg = 
  if  pat.msg_bin_pat_proto <> msg.msg_bin_proto ||
     pat.msg_bin_pat_opcode <> msg.msg_bin_opcode ||
    pat.msg_bin_pat_to <> msg.msg_bin_to
  then
    raise Do_not_Match;
  match pat.msg_bin_pat_from with
    | Pat_Const c when c = msg.msg_bin_from -> []
    | Pat_Var x -> [x, msg.msg_bin_from]
    | _ -> raise Do_not_Match
      
module type BIN = sig

  val can_make_a_step : unit -> bool
  val step : unit -> unit

  val can_add : msg_bin -> bool
  val add : msg_bin -> unit
  val can_rm : msg_bin_pat -> bool
  val rm : msg_bin_pat -> (string * string) list

end

module type INTERP = sig
  val run : formatter -> prog -> unit
end

module Make ( M : BIN ) = struct

  type t = {
    vars : (string, int) Hashtbl.t;
    mutable locs : S.t;
  }
    
  let print_state fmt g = 
    fprintf fmt "\nState : (-, %a)@." pp_S g.locs

  (* XXX : no substitution mecanism yet *)

  let apply fmt g r = 
    g.locs <- S.union (S.diff g.locs r.rule_loc_in) r.rule_loc_out;
    List.iter 
      (function 
	| Rm pat -> let _ = M.rm pat in ()
	| Add m -> M.add m
	| Other_action -> () ) 
      r.rule_action;
    fprintf fmt "\t\t[ %s (%s) ]\n%a\n" r.rule_name r.rule_agent print_state g

  let is_a_model g r = 
    S.subset r.rule_loc_in g.locs && 
      List.fold_left
      (fun res lit ->
	match lit with
	  | Can_add m -> res && M.can_add m
	  | Can_rm pat -> res && M.can_rm pat
	  | Other_literal -> res
      ) 
      true r.rule_guard

  let filter g rules = 
    Array.of_list (List.filter (is_a_model g) rules)

  exception Deadlock

  let run fmt p = 
    fprintf fmt 
      "\n=-=-=-=-=-==-=-=-=-=-= SIMULATION TRACE =-=-=-=-=-==-=-=-=-=-=\n@.";
    let g = { vars = Hashtbl.create 10; locs = p.init_locs } in 
    fprintf fmt "%a@." print_state g;
    let step = ref steps in
    try
      while !step > 0 do
	if M.can_make_a_step () && Random.int 100 < fabric_priority then
	  begin
	    fprintf fmt "\t\t >> Fabric <<\n\n";
	    M.step ()
	  end
	else
	  begin
	    let sr = filter g p.rules in
	    if debug then 
	      begin
		fprintf fmt "{";
		Array.iter 
		  (fun r -> fprintf fmt " (%s) %s " r.rule_agent r.rule_name) 
		  sr;
		fprintf fmt " }\n@.";
	      end;
	    if Array.length sr = 0 then raise Deadlock;
	    let i = Random.int (Array.length sr) in
	    apply fmt g sr.(i)
	  end;
	decr step
      done;
      fprintf fmt "--> No deadlock detected after %d steps@." steps
    with Deadlock -> 
      fprintf fmt "\t       *** DEADLOCK ***@."
end

