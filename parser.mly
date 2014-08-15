/**************************************************************************/
/*                                                                        */
/*                              FLAX                                      */
/*                                                                        */
/*                       Copyright (C) 2014                               */
/*                                                                        */
/*                         Sylvain Conchon                                */
/*                                                                        */
/*                     Universite Paris-Sud 11                            */
/*                                                                        */
/*                                                                        */
/*  This file is distributed under the terms of the Apache Software       */
/*  License version 2.0                                                   */
/*                                                                        */
/**************************************************************************/

%{

  open Ast
  open Parsing
  
  let error_attribute s a = 
    let a = match a with
      | OWNER -> "owner"
      | TYPE -> "type"
      | INIT -> "init"
      | PROTO  -> "proto"
      | DATATYPE -> "datatype"
      | REPO -> "repo"
      | DATA -> "data"
      | AGT -> "agt"
      | GUARD -> "guard"
      | ACTION -> "action"
      | SHAPE -> "shape"
      | LABEL -> "label"
      | PAUSE -> "pause"
      | MSG -> "msg"
      | NAME -> "name"
      | COND -> "cond"
      | WAC -> "wac"
      | RAC -> "rac"
      | OPCODE -> "opcode"
      | _ -> assert false
    in
    let msg = 
      Printf.sprintf "Attribute %s does not supported in %s category" a s in
    failwith msg

  let make_global n l = 
    let g = {
      glob_name = n; 
      glob_type = None;
      glob_init = None;
    }
    in
    List.fold_left 
      (fun g (a, v) ->
	match a with
	  | TYPE -> { g with glob_type = Some v }
	  | INIT -> { g with glob_init = Some v }
	  | _ -> error_attribute "gloval" a
      ) g l

  let make_local n l = 
    let x = {
      local_name = n;
      local_type = None;
      local_owner = None;
      local_init = None;
    }
    in
    List.fold_left 
      (fun x (a, v) ->
	match a with
	  | TYPE -> { x with local_type = Some v }
	  | INIT -> { x with local_init = Some v }
	  | OWNER -> { x with local_owner = Some v }
	  | _ -> error_attribute "local" a
      ) x l

  let make_repository n l = 
    let r = { 
      rep_name = n; 
      rep_owner = ""; 
      rep_type = None; 
      rep_init = None;
      rep_wac = None;
      rep_rac = None;
    } 
    in
    List.fold_left 
      (fun r (a, v) -> 
	match a with
	  | OWNER -> { r with rep_owner = v} 
	  | TYPE ->  { r with rep_type = Some v} 
	  | INIT ->  { r with rep_init = Some v}
	  | WAC ->   { r with rep_wac = Some v}
	  | RAC ->   { r with rep_rac = Some v}
	  | _ -> error_attribute "repository" a
      ) r l

  let node_shape_of = function
    | "START" -> Start
    | "TASK" -> Task
    | "NPSYNC" -> NPSync
    | "PSYNC" -> PSync
    | "BRANCH" -> Branch
    | _ -> assert false

  let make_node n l = 
    let n = {
      node_name = n;
      node_agent = "";
      node_shape = Start;
      node_label = "";
      node_guard = "";
      node_action = Other "";
    }
    in 
    List.fold_left
      (fun n (a, v) ->
	match a with
	   | AGT -> { n with node_agent = v }
	   | SHAPE -> { n with node_shape = node_shape_of v }
	   | LABEL -> { n with node_label = v }
	   | GUARD -> { n with node_guard = v }
	   | ACTION -> { n with node_action = Other v }
	   | _ -> assert false
      ) n l

  let update_arc_cond e v = 
    match v with 
      | "Yes" -> 
	{ e with arc_cond = Some true }
      | "No" -> 
	{ e with arc_cond = Some false }
      | _ -> 
	{ e with arc_cond = None }


  let arc_shape_of = function
    | "SEQUENCE" -> Sequence
    | "MESSAGE" -> Message
    | _ -> assert false

  let update_arc_msg e a v = 
    let msg = 
      match e.arc_msg with
	| Some msg -> msg
	|  None ->
	  {  msg_proto = "";
	     msg_from = ""; 
	     msg_to = ""; 
	     msg_data_type = "";
	     msg_repo = "";
	     msg_data = "";
	     msg_opcode = "Other";}
    in
    let msg = 
      match a with
	| PROTO -> { msg with msg_proto = v }
	| DATATYPE -> { msg with msg_data_type = v }
	| REPO -> { msg with msg_repo = v }
	| DATA -> { msg with msg_data = v }
	| OPCODE -> { msg with msg_opcode = v }
	| _ -> assert false
    in 
    { e with arc_msg = Some msg }

  let make_arc n l = 
    let e = {
      arc_name = n;
      arc_from = "";
      arc_to = "";
      arc_shape = Sequence;
      arc_label = "";
      arc_cond = None;
      arc_msg = None;
    }
    in 
    List.fold_left 
      (fun e (a, v) ->
	match a with
	  | FROM  -> { e with arc_from = v }
	  | TO  -> { e with arc_to = v }
	  | SHAPE -> { e with arc_shape = arc_shape_of v }
	  | LABEL -> { e with arc_label = v }
	  | COND -> update_arc_cond e v
	  | PROTO  | DATATYPE | REPO | DATA | OPCODE ->
	    update_arc_msg e a v
	  | _ -> assert false
      ) e l


%}

/* keywords */
%token GLOBAL AGENT REPOSITORY DIAGRAM LOCAL NODE ARC

/* attributes */
%token OWNER TYPE INIT WAC RAC
%token PROTO DATATYPE REPO DATA 
%token SHAPE LABEL
%token AGT PAUSE GUARD ACTION OPCODE
%token NAME MSG COND FROM TO

/* shapes */
%token TASK START NPSYNC PSYNC BRANCH
%token SEQUENCE MESSAGE

/* constructors */
%token CFGWR CFGRD COMPL
%token YES NO
%token GENERIC IOSFSB

/* identifiers */
%token <string> IDENT
%token <string> MIDENT

/* numbers */
%token <string> INT

/* other tokens */

%token LPAR RPAR EQ LSQ RSQ LBR RBR COMMA
%token <string> STRING
%token EOF

%type <Ast.iflow> iflow
%start iflow
%%

iflow:
| agents globals repositories diagrams
  { { agents = $1; 
      globals = $2;
      repositories = $3;
      diagrams = $4;
    } }
;

/* Agents */

agents:
| agent { [$1] }
| agent agents { $1::$2 }
;

agent:
AGENT MIDENT { $2 }
;

/* Globals */

globals:
| { [] }
| global globals { $1 :: $2 }
;

global:
GLOBAL MIDENT LSQ attr_list RSQ { make_global $2 $4 }
;

/* Repositories */

repositories:
|   { [] }
| repository repositories { $1 :: $2 }
;

repository:
REPOSITORY MIDENT LSQ attr_list RSQ { make_repository $2 $4 }
;


/* Diagrams */

diagrams:
| { [] }
| diagram diagrams { $1 :: $2 }
;

diagram:
| DIAGRAM MIDENT LBR locals nodes arcs RBR 
    { {diag_name = $2; 
       diag_locals = $4; 
       diag_nodes = $5; 
       diag_arcs = $6 } }
;

locals:
| { [] }
| local locals { $1 :: $2 }
;

local:
| LOCAL MIDENT LSQ attr_list RSQ 
    { make_local $2 $4 }
;

nodes:
| { [] }
| node nodes { $1 :: $2}
;

node:
| NODE MIDENT LSQ attr_list RSQ 
    { make_node $2 $4 }
;

arcs:
| { [] }
| arc arcs { $1 :: $2}
;

arc:
| ARC MIDENT LSQ attr_list RSQ 
    { make_arc $2 $4 }
;


/* Atributes */

attr_list :
| { [] }
| attr { [$1] }
| attr COMMA attr_list { $1 :: $3 }
;

attr:
| OWNER EQ MIDENT { OWNER, $3 }
| TYPE EQ IDENT   { TYPE, $3 }
| INIT EQ IDENT   { INIT, $3 }
| INIT EQ INT     { INIT, $3 }
| PROTO  EQ MIDENT { PROTO, $3 }
| DATATYPE  EQ IDENT { DATATYPE, $3 }
| REPO  EQ MIDENT { REPO, $3 }
| DATA  EQ MIDENT { DATA, $3 }
| AGT EQ MIDENT { AGT, $3 }
| GUARD EQ STRING { GUARD, $3 }
| ACTION EQ STRING { ACTION, $3 }
| SHAPE EQ shape { SHAPE, $3 }
| LABEL EQ STRING { LABEL, $3 }
| PAUSE EQ constructors { PAUSE, $3 }
| MSG EQ MIDENT { MSG, $3 }
| NAME EQ MIDENT { NAME, $3 }
| COND EQ constructors { COND, $3 }
| WAC EQ INT { WAC, $3 }
| RAC EQ INT { RAC, $3 }
| OPCODE EQ constructors { OPCODE, $3 }
| FROM EQ MIDENT { FROM, $3 }
| TO EQ MIDENT { TO, $3 }
;

constructors:
| CFGWR { "CfgWr" }
| CFGRD { "CfgRd" }
| COMPL { "Compl" }
| YES { "Yes" }
| NO { "No" }
| {""}
;

shape:
| START { "START" }
| TASK { "TASK" }
| NPSYNC { "NPSYNC" }
| PSYNC { "PSYNC" }
| BRANCH { "BRANCH" }
| SEQUENCE { "SEQUENCE" }
| MESSAGE { "MESSAGE" }
;
