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

open Common

(* =-=-=-=-= FLAX AST =-=-=-=-= 

   Entry point : iflow

   Improvements : 
     - replace "strings" by algebraic types.
     - design a basic type checking algorithm
   
   
*)

type agent = string

type global = {
  glob_name : string; 
  glob_type : string option;
  glob_init : string option;
}

type repository = { 
  rep_name : string;
  rep_owner : string;
  rep_type : string option;
  rep_init : string option;
  rep_wac : string option;
  rep_rac : string option;
}

type local = {
  local_name : string;
  local_type : string option;
  local_owner : string option;
  local_init : string option;
}

type msg_type = {
  msg_proto : string;
  msg_from : string;
  msg_to : string;
  msg_data_type : string;
  msg_repo : string;
  msg_data : string;
  msg_opcode : string;
}

type node_shape =
  | Start | Task | PSync | NPSync | Branch | InLink | OutLink

type action = Create of msg_type | Consume of msg_type | Other of string

type node = {
  node_name : string;
  node_agent : string;
  node_shape : node_shape;
  node_label : string;
  node_connect : string;
  mutable node_guard : string;
  mutable node_action : action;
}

type arc_shape = Sequence | Message

type arc = {
  arc_name : string;
  arc_from : string;
  arc_to : string;
  arc_shape : arc_shape;
  arc_label : string;
  arc_cond : bool option;

  arc_msg : msg_type option;
}

type diagram = {
  diag_name : string; 
  diag_locals : local list;
  diag_nodes : node list;
  diag_arcs : arc list;
}

type iflow = {
  agents : agent list;
  globals : global list;
  repositories : repository list;
  diagrams : diagram list;
}


(* =-=-=-=-= GALM AST =-=-=-=-= *)

type msg_bin = { 
  msg_bin_proto : string; 
  msg_bin_opcode : string;
  msg_bin_from : string;
  msg_bin_to : string;
}

type pat = Pat_Const of string | Pat_Var of string

type msg_bin_pat = { 
  msg_bin_pat_proto : string; 
  msg_bin_pat_opcode : string;
  msg_bin_pat_from : pat;
  msg_bin_pat_to : string;
}

type lit = Can_add of msg_bin | Can_rm of msg_bin_pat | Other_literal
type act = Rm of msg_bin_pat | Add of msg_bin | Other_action

type rule = {
  rule_name : string;
  rule_agent : string;
  rule_loc_in : S.t;
  rule_guard : lit list;
  rule_action : act list;
  rule_loc_out : S.t;
}

type prog = {
  rules : rule list;
  init_locs : S.t;
  agents : string list;
}


module type BIN = sig

  val can_make_a_step : unit -> bool
  val step : unit -> unit

  val can_add : msg_bin -> bool
  val add :  msg_bin -> unit
  val can_rm : msg_bin_pat -> bool
  val rm : msg_bin_pat -> (string * string) list

  val pp : Format.formatter -> prog -> unit
  val pp_add : Format.formatter -> string -> msg_bin -> unit
  val pp_rm : Format.formatter -> string -> msg_bin_pat -> unit
  val pp_can_add : Format.formatter -> string -> msg_bin -> unit
  val pp_can_rm : Format.formatter -> string -> msg_bin_pat -> unit
end
