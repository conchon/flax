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

(* First Galm machine : a single queue *)

module Fab1 = struct
  let q : msg_bin Queue.t = Queue.create ()

  let can_make_a_step () = false
  let step () = ()
    
  let can_add _ = (queue_capacity = 0) || (Queue.length q < queue_capacity)
    
  let add msg = Queue.add msg q
    
  let can_rm pat = 
    try
      ignore (pattern_matching pat (Queue.top q));
      true
    with Queue.Empty | Do_not_Match -> false
      
  let rm pat = 
    try
      pattern_matching pat (Queue.pop q)
    with Queue.Empty | Do_not_Match -> failwith "galm G0 : rm failed"

  let pp fmt prog = 
    let agents = prog.agents in
    fprintf fmt "\n/* Fabric 0 */\n\n";
    fprintf fmt "chan Q = [%d] of { mtype, mtype, mtype, mtype }\n" 
      queue_capacity;
    fprintf fmt "\n";
    List.iter 
      (fprintf fmt "#define can_add_%s(p,m,from,to) nfull(Q)\n") agents;
    List.iter 
      (fprintf fmt "#define can_rm_%s(p,m,from,to) Q?[p,m,from,to]\n") agents;
    List.iter 
      (fprintf fmt "#define add_%s(p,m,from,to) Q!p,m,from,to\n") agents;
    List.iter 
      (fprintf fmt "#define rm_%s(p,m,from,to) Q?p,m,from,to\n") agents

  let pp_add fmt agent msg = 
    fprintf fmt "\tadd_%s(%s, %s, %s, %s);\n"
      agent
      msg.msg_bin_proto
      msg.msg_bin_opcode
      msg.msg_bin_from
      msg.msg_bin_to

  let pp_rm fmt agent pat = 
    fprintf fmt "\trm_%s(%s, %s, %s, %s);\n"
      agent
      pat.msg_bin_pat_proto
      pat.msg_bin_pat_opcode
      (match pat.msg_bin_pat_from with
	| Pat_Const c -> c
	| Pat_Var x -> x)
      pat.msg_bin_pat_to

  let pp_can_add fmt agent msg = 
    fprintf fmt " can_add_%s(%s, %s, %s, %s) "
      agent
      msg.msg_bin_proto
      msg.msg_bin_opcode
      msg.msg_bin_from
      msg.msg_bin_to

  let pp_can_rm fmt agent pat = 
    fprintf fmt " can_rm_%s(%s,%s,%s,%s) "
      agent
      pat.msg_bin_pat_proto
      pat.msg_bin_pat_opcode
      (match pat.msg_bin_pat_from with
	| Pat_Const c -> c
	| Pat_Var x -> x)
      pat.msg_bin_pat_to

end


(* Second Galm machine : a queue per agent *)


module Fab2 = struct

  type t = msg_bin Queue.t
  let queues : (string, t) Hashtbl.t = Hashtbl.create 42

  let can_make_a_step () = false
  let step () = ()

  let find_q a = 
    try 
      Hashtbl.find queues a 
    with Not_found -> 
      let q = Queue.create () in
      Hashtbl.add queues a q;
      q

  let can_add msg = 
    let q = find_q msg.msg_bin_to in
    (queue_capacity = 0) || (Queue.length q < queue_capacity)

  let add msg = 
    let q = find_q msg.msg_bin_to in
    Queue.add msg q

  let can_rm pat = 
    try
      let q = find_q pat.msg_bin_pat_to in
      ignore (pattern_matching pat (Queue.top q));
      true
    with Queue.Empty | Do_not_Match -> false

  let rm pat = 
    try
      let q = find_q pat.msg_bin_pat_to in
      pattern_matching pat (Queue.pop q)
    with Queue.Empty | Do_not_Match -> failwith "galm G1 : rm failed"

  let pp fmt prog = 
    let agents = prog.agents in
    fprintf fmt "\n/* Fabric 1 */\n\n";
    List.iter
      (fun a ->
	fprintf fmt "chan Q_%s = [%d] of { mtype, mtype, mtype, mtype }\n" 
	  a queue_capacity) agents ;
    fprintf fmt "\n";
    List.iter 
      (fun a -> 
	fprintf fmt "#define can_add_%s(p,m,from,to) nfull(Q_%s)\n" a a)
      agents;
    List.iter 
      (fun a -> 
	fprintf fmt "#define can_rm_%s(p,m,from,to) Q_%s?[p,m,from,to]\n" a a)
      agents;
    List.iter 
      (fun a -> 
	fprintf fmt "#define add_%s(p,m,from,to) Q_%s!p,m,from,to\n" a a) 
      agents;
    List.iter 
      (fun a ->
	fprintf fmt "#define rm_%s(p,m,from,to) Q_%s?p,m,from,to\n" a a) 
      agents

  let pp_add fmt _ msg = 
    fprintf fmt "\tadd_%s(%s, %s, %s, %s);\n"
      msg.msg_bin_to
      msg.msg_bin_proto
      msg.msg_bin_opcode
      msg.msg_bin_from
      msg.msg_bin_to

  let pp_rm fmt agent pat = 
    fprintf fmt "\trm_%s(%s, %s, %s, %s);\n"
      agent
      pat.msg_bin_pat_proto
      pat.msg_bin_pat_opcode
      (match pat.msg_bin_pat_from with
	| Pat_Const c -> c
	| Pat_Var x -> x)
      pat.msg_bin_pat_to

  let pp_can_add fmt _ msg = 
    fprintf fmt " can_add_%s(%s, %s, %s, %s) "
      msg.msg_bin_to
      msg.msg_bin_proto
      msg.msg_bin_opcode
      msg.msg_bin_from
      msg.msg_bin_to

  let pp_can_rm fmt agent pat = 
    fprintf fmt " can_rm_%s(%s,%s,%s,%s) "
      agent
      pat.msg_bin_pat_proto
      pat.msg_bin_pat_opcode
      (match pat.msg_bin_pat_from with
	| Pat_Const c -> c
	| Pat_Var x -> x)
      pat.msg_bin_pat_to

end


(* Third Galm machine : two queues (C/P, and NP) per agent *)

module Fab3 = struct

  type t = { pc : msg_bin Queue.t; np : msg_bin Queue.t }

  let queues : (string, t) Hashtbl.t = Hashtbl.create 42

  let find_q a = 
    try 
      Hashtbl.find queues a 
    with Not_found -> 
      let pc = Queue.create () in
      let np = Queue.create () in
      let q = { pc = pc; np = np } in
      Hashtbl.add queues a q;
      q

  let can_add msg = 
    let { pc = pc; np = np } = find_q msg.msg_bin_to in
    (queue_capacity = 0) || 
      (if msg.msg_bin_opcode = "CfgRd" then Queue.length np < queue_capacity
       else Queue.length pc < queue_capacity)

  let add msg = 
    let { pc = pc; np = np } = find_q msg.msg_bin_to in
    if msg.msg_bin_opcode = "CfgRd" then
      Queue.add msg np
    else
      Queue.add msg pc

  let can_make_a_step () = false

  let step () = ()

  let can_rm pat = 
    try
      let { pc = pc; np = np } = find_q pat.msg_bin_pat_to in
      if pat.msg_bin_pat_opcode = "CfgRd" then
	(ignore (pattern_matching pat (Queue.top np)); true)
      else
	(ignore (pattern_matching pat (Queue.top pc)); true)
    with Queue.Empty | Do_not_Match -> false

  let rm pat = 
    try
      let { pc = pc; np = np } = find_q pat.msg_bin_pat_to in
      if pat.msg_bin_pat_opcode = "CfgRd" then
	pattern_matching pat (Queue.top np)
      else
	pattern_matching pat (Queue.top pc)
    with Queue.Empty | Do_not_Match -> failwith "galm G3 : rm failed"

  let pp fmt prog = 
    let agents = prog.agents in
    fprintf fmt "\n/* Fabric 2 */\n\n";
    List.iter
      (fun a ->
	fprintf fmt "chan PC_%s = [%d] of { mtype, mtype, mtype, mtype }\n" 
	  a queue_capacity;
	fprintf fmt "chan NP_%s = [%d] of { mtype, mtype, mtype, mtype }\n" 
	  a queue_capacity) 
      agents ;
    fprintf fmt "\n";
    List.iter 
      (fun a -> 
	fprintf fmt "#define can_add_pc_%s(p,m,from,to) nfull(PC_%s)\n" a a;
	fprintf fmt "#define can_add_np_%s(p,m,from,to) nfull(NP_%s)\n" a a;
      )
      agents;
    List.iter 
      (fun a -> 
	fprintf fmt 
	  "#define can_rm_pc_%s(p,m,from,to) PC_%s?[p,m,from,to]\n" a a;
	fprintf fmt 
	  "#define can_rm_np_%s(p,m,from,to) NP_%s?[p,m,from,to]\n" a a;)
      agents;
    List.iter 
      (fun a -> 
	fprintf fmt "#define add_pc_%s(p,m,from,to) PC_%s!p,m,from,to\n" a a;
	fprintf fmt "#define add_np_%s(p,m,from,to) NP_%s!p,m,from,to\n" a a
      ) 
      agents;
    List.iter 
      (fun a ->
	fprintf fmt "#define rm_pc_%s(p,m,from,to) PC_%s?p,m,from,to\n" a a;
	fprintf fmt "#define rm_np_%s(p,m,from,to) NP_%s?p,m,from,to\n" a a) 
      agents

  let pp_add fmt agent msg = 
    if msg.msg_bin_opcode = "CfgRd" then
      fprintf fmt "\tadd_np_%s(%s, %s, %s, %s);\n"
	msg.msg_bin_to
	msg.msg_bin_proto
	msg.msg_bin_opcode
	msg.msg_bin_from
	msg.msg_bin_to
    else
      fprintf fmt "\tadd_pc_%s(%s, %s, %s, %s);\n"
	msg.msg_bin_to
	msg.msg_bin_proto
	msg.msg_bin_opcode
	msg.msg_bin_from
	msg.msg_bin_to

  let pp_rm fmt agent pat = 
    if pat.msg_bin_pat_opcode = "CfgRd" then
      fprintf fmt "\trm_np_%s(%s, %s, %s, %s);\n"
	agent
	pat.msg_bin_pat_proto
	pat.msg_bin_pat_opcode
	(match pat.msg_bin_pat_from with
	  | Pat_Const c -> c
	  | Pat_Var x -> x)
	pat.msg_bin_pat_to
    else
      fprintf fmt "\trm_pc_%s(%s, %s, %s, %s);\n"
	agent
	pat.msg_bin_pat_proto
	pat.msg_bin_pat_opcode
	(match pat.msg_bin_pat_from with
	  | Pat_Const c -> c
	  | Pat_Var x -> x)
	pat.msg_bin_pat_to

  let pp_can_add fmt agent msg = 
    if msg.msg_bin_opcode = "CfgRd" then
      fprintf fmt " can_add_np_%s(%s, %s, %s, %s) "
	msg.msg_bin_to
	msg.msg_bin_proto
	msg.msg_bin_opcode
	msg.msg_bin_from
	msg.msg_bin_to
    else
      fprintf fmt " can_add_pc_%s(%s, %s, %s, %s) "
	msg.msg_bin_to
	msg.msg_bin_proto
	msg.msg_bin_opcode
	msg.msg_bin_from
	msg.msg_bin_to

  let pp_can_rm fmt agent pat = 
    if pat.msg_bin_pat_opcode = "CfgRd" then
      fprintf fmt " can_rm_np_%s(%s,%s,%s,%s) "
	agent
	pat.msg_bin_pat_proto
	pat.msg_bin_pat_opcode
	(match pat.msg_bin_pat_from with
	  | Pat_Const c -> c
	  | Pat_Var x -> x)
	pat.msg_bin_pat_to
    else
      fprintf fmt " can_rm_pc_%s(%s,%s,%s,%s) "
	agent
	pat.msg_bin_pat_proto
	pat.msg_bin_pat_opcode
	(match pat.msg_bin_pat_from with
	  | Pat_Const c -> c
	  | Pat_Var x -> x)
	pat.msg_bin_pat_to

end


(* Fourth Galm machine : ingress and egress queues per agent *)

module Fab4 = struct

  type t = { igress : msg_bin Queue.t; egress : msg_bin Queue.t }

  let queues : (string, t) Hashtbl.t = Hashtbl.create 42

  let find_q a = 
    try 
      Hashtbl.find queues a 
    with Not_found -> 
      let iq = Queue.create () in
      let eq = Queue.create () in
      let q = { igress = iq; egress = eq } in
      Hashtbl.add queues a q;
      q

  exception Pending of msg_bin Queue.t
  let can_make_a_step () = 
    try
      Hashtbl.iter 
	(fun _ { egress = eq } ->
	  if not (Queue.is_empty eq) then
	    let msg = Queue.top eq in
	    let { igress = iq} = find_q msg.msg_bin_to in
	    if (queue_capacity = 0) || (Queue.length iq < queue_capacity) 
	    then raise (Pending iq)) queues;
      false
    with Pending _ -> true

  let step () = 
    try
      Hashtbl.iter 
	(fun _ { egress = eq } -> 
	  if not (Queue.is_empty eq) then raise (Pending eq)) queues;
      failwith "(fabric) fails to make a step!"
    with Pending eq -> 
      let msg = Queue.pop eq in
      let { igress = iq} = find_q msg.msg_bin_to in
      Queue.add msg iq

  let can_add msg = 
    let { egress = eq} = find_q msg.msg_bin_from in
    (queue_capacity = 0) || (Queue.length eq < queue_capacity)


  let add msg = 
    let { egress = eq} = find_q msg.msg_bin_from in
    Queue.add msg eq

  let can_rm pat = 
    try
      let { igress = iq} = find_q pat.msg_bin_pat_to in
      ignore (pattern_matching pat (Queue.top iq));
      true
    with Queue.Empty | Do_not_Match -> false

  let rm pat = 
    try
      let { igress = iq} = find_q pat.msg_bin_pat_to in
      pattern_matching pat (Queue.top iq)
    with Queue.Empty | Do_not_Match -> failwith "galm G2 : rm failed"

  let pp fmt prog = 
    let agents = prog.agents in
    fprintf fmt "\n/* Fabric 2 */\n\n";
    List.iter
      (fun a ->
	fprintf fmt "chan EQ_%s = [%d] of { mtype, mtype, mtype, mtype }\n" 
	  a queue_capacity;
	fprintf fmt "chan IQ_%s = [%d] of { mtype, mtype, mtype, mtype }\n" 
	  a queue_capacity) 
      agents ;
    fprintf fmt "\n";
    List.iter 
      (fun a -> 
	fprintf fmt "#define can_add_%s(p,m,from,to) nfull(EQ_%s)\n" a a)
      agents;
    List.iter 
      (fun a -> 
	fprintf fmt "#define can_rm_%s(p,m,from,to) IQ_%s?[p,m,from,to]\n" a a)
      agents;
    List.iter 
      (fun a -> 
	fprintf fmt "#define add_%s(p,m,from,to) EQ_%s!p,m,from,to\n" a a) 
      agents;
    List.iter 
      (fun a ->
	fprintf fmt "#define rm_%s(p,m,from,to) IQ_%s?p,m,from,to\n" a a) 
      agents;
    fprintf fmt "\nactive proctype Fabric ()\n{\n";
    fprintf fmt " mtype p;\n mtype m;\n mtype to;\n\n do\n\n";
    List.iter
      (fun from_ ->
	List.iter
	  (fun to_ ->
	    if from_ <> to_ then
	      begin
		fprintf fmt 
		  " :: atomic { EQ_%s?[p,m,_,%s] && nfull(IQ_%s) -> \n" 
		  from_ to_ to_;
		fprintf fmt "\n\t     EQ_%s?p,m,%s,%s;\n" from_ from_ to_;
		fprintf fmt "\t     IQ_%s!p,m,%s,%s\n    }\n\n" to_ from_ to_
	      end) 
	  agents) 
      agents;
    fprintf fmt " od\n}"

  let pp_add fmt agent msg = 
    fprintf fmt "\tadd_%s(%s, %s, %s, %s);\n"
      agent
      msg.msg_bin_proto
      msg.msg_bin_opcode
      msg.msg_bin_from
      msg.msg_bin_to

  let pp_rm fmt agent pat = 
    fprintf fmt "\trm_%s(%s, %s, %s, %s);\n"
      agent
      pat.msg_bin_pat_proto
      pat.msg_bin_pat_opcode
      (match pat.msg_bin_pat_from with
	| Pat_Const c -> c
	| Pat_Var x -> x)
      pat.msg_bin_pat_to

  let pp_can_add fmt agent msg = 
    fprintf fmt " can_add_%s(%s, %s, %s, %s) "
      agent
      msg.msg_bin_proto
      msg.msg_bin_opcode
      msg.msg_bin_from
      msg.msg_bin_to


  let pp_can_rm fmt agent pat = 
    fprintf fmt " can_rm_%s(%s,%s,%s,%s) "
      agent
      pat.msg_bin_pat_proto
      pat.msg_bin_pat_opcode
      (match pat.msg_bin_pat_from with
	| Pat_Const c -> c
	| Pat_Var x -> x)
      pat.msg_bin_pat_to

end


(* Fifth Galm machine : ingress and egress queues per agent, but
   can_rm waits for the egress queue to be empty *)

module Fab5 = struct

  type t = { igress : msg_bin Queue.t; egress : msg_bin Queue.t }

  let queues : (string, t) Hashtbl.t = Hashtbl.create 42

  let find_q a = 
    try 
      Hashtbl.find queues a 
    with Not_found -> 
      let iq = Queue.create () in
      let eq = Queue.create () in
      let q = { igress = iq; egress = eq } in
      Hashtbl.add queues a q;
      q

  exception Pending of msg_bin Queue.t
  let can_make_a_step () = 
    try
      Hashtbl.iter 
	(fun _ { egress = eq } ->
	  if not (Queue.is_empty eq) then
	    let msg = Queue.top eq in
	    let { igress = iq} = find_q msg.msg_bin_to in
	    if (queue_capacity = 0) || (Queue.length iq < queue_capacity) 
	    then raise (Pending iq)) queues;
      false
    with Pending _ -> true

  let step () = 
    try
      Hashtbl.iter 
	(fun _ { egress = eq } -> 
	  if not (Queue.is_empty eq) then raise (Pending eq)) queues;
      failwith "(fabric) fails to make a step!"
    with Pending eq -> 
      let msg = Queue.pop eq in
      let { igress = iq} = find_q msg.msg_bin_to in
      Queue.add msg iq

  let can_add msg = 
    let { egress = eq} = find_q msg.msg_bin_from in
    (queue_capacity = 0) || (Queue.length eq < queue_capacity)


  let add msg = 
    let { egress = eq} = find_q msg.msg_bin_from in
    Queue.add msg eq

  let can_rm pat = 
    try
      let { igress = iq; egress = eq} = find_q pat.msg_bin_pat_to in
      if not (Queue.is_empty eq) then false
      else
	begin
	  ignore (pattern_matching pat (Queue.top iq));
	  true
	end
    with Queue.Empty | Do_not_Match -> false

  let rm pat = 
    try
      let { igress = iq} = find_q pat.msg_bin_pat_to in
      pattern_matching pat (Queue.top iq)
    with Queue.Empty | Do_not_Match -> failwith "galm G2 : rm failed"

  let pp fmt prog = 
    let agents = prog.agents in
    fprintf fmt "\n/* Fabric 2 */\n\n";
    List.iter
      (fun a ->
	fprintf fmt "chan EQ_%s = [%d] of { mtype, mtype, mtype, mtype }\n" 
	  a queue_capacity;
	fprintf fmt "chan IQ_%s = [%d] of { mtype, mtype, mtype, mtype }\n" 
	  a queue_capacity) 
      agents ;
    fprintf fmt "\n";
    List.iter 
      (fun a -> 
	fprintf fmt "#define can_add_%s(p,m,from,to) nfull(EQ_%s)\n" a a)
      agents;
    List.iter 
      (fun a -> 
	fprintf fmt "#define can_rm_%s(p,m,from,to) empty(EQ_%s) && IQ_%s?[p,m,from,to]\n" a a a)
      agents;
    List.iter 
      (fun a -> 
	fprintf fmt "#define add_%s(p,m,from,to) EQ_%s!p,m,from,to\n" a a) 
      agents;
    List.iter 
      (fun a ->
	fprintf fmt "#define rm_%s(p,m,from,to) IQ_%s?p,m,from,to\n" a a) 
      agents;
    fprintf fmt "\nactive proctype Fabric ()\n{\n";
    fprintf fmt " mtype p;\n mtype m;\n mtype to;\n\n do\n\n";
    List.iter
      (fun from_ ->
	List.iter
	  (fun to_ ->
	    if from_ <> to_ then
	      begin
		fprintf fmt 
		  " :: atomic { EQ_%s?[p,m,_,%s] && nfull(IQ_%s) -> \n" 
		  from_ to_ to_;
		fprintf fmt "\n\t     EQ_%s?p,m,%s,%s;\n" from_ from_ to_;
		fprintf fmt "\t     IQ_%s!p,m,%s,%s\n    }\n\n" to_ from_ to_
	      end) 
	  agents) 
      agents;
    fprintf fmt " od\n}"

  let pp_add fmt agent msg = 
    fprintf fmt "\tadd_%s(%s, %s, %s, %s);\n"
      agent
      msg.msg_bin_proto
      msg.msg_bin_opcode
      msg.msg_bin_from
      msg.msg_bin_to

  let pp_rm fmt agent pat = 
    fprintf fmt "\trm_%s(%s, %s, %s, %s);\n"
      agent
      pat.msg_bin_pat_proto
      pat.msg_bin_pat_opcode
      (match pat.msg_bin_pat_from with
	| Pat_Const c -> c
	| Pat_Var x -> x)
      pat.msg_bin_pat_to
      
  let pp_can_add fmt agent msg = 
    fprintf fmt " can_add_%s(%s, %s, %s, %s) "
      agent
      msg.msg_bin_proto
      msg.msg_bin_opcode
      msg.msg_bin_from
      msg.msg_bin_to

  let pp_can_rm fmt agent pat = 
    fprintf fmt " can_rm_%s(%s,%s,%s,%s) "
      agent
      pat.msg_bin_pat_proto
      pat.msg_bin_pat_opcode
      (match pat.msg_bin_pat_from with
	| Pat_Const c -> c
	| Pat_Var x -> x)
      pat.msg_bin_pat_to

end
