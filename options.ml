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

let usage = "usage: flaxc file.flax"
let debug = ref false
let show_graph = ref false
let simu = ref false
let file_in = ref "_stdin"
let file_out = ref ""
let seed = ref 42
let fabric_priority = ref 80
let single = ref false
let verbose = ref 0
let fabric = ref 1
let queue_capacity = ref 1
let steps = ref 200
let mc = ref ""

let set_mc s = 
  if s = "cubicle" || s = "promela" then mc := s
  else  raise (Arg.Bad (s^" model checker not supported"))
 
let show_version () = Format.printf "%s@." Version.version; exit 0
let verbose_level () = incr verbose

let set_file_out s =
  if Filename.check_suffix s ".galm" then file_out := s
  else raise (Arg.Bad "no .galm extension")


let specs = 
  [ "-version", Arg.Unit show_version, " prints the version number";
    "-debug", Arg.Set debug, " debug mode";
    "-v", Arg.Unit verbose_level, 
    " increase de verbosity level of the debug mode";
    "-showgraph", Arg.Set show_graph, " show iflow and message subsystem graphs\n";
    "-o",  Arg.String set_file_out, "<file.galm> set output galm file\n";

    "-simu", Arg.Set simu, " simulation mode";
    "-steps",  Arg.Set_int steps, " <int> numbers of simulation steps (10 by default)";
    "-seed",  Arg.Set_int seed, " <int> set the random seed for simulation";
    "-single", Arg.Set single, " single active service per agent";
    "-fabric", Arg.Set_int fabric, " <int> fabric subsystem\n\t\t\t  1 = one single queue\n\t\t\t  2 = a single queue per agent\n\t\t\t  3 = in-and-egress queues per agent\n\t\t\t 4 = in-and-egress queues per agent\n\t\t\t  5 = in-and-egress queues per agent, \n\t\t\t      but remove from igress only when egress is empty\n";
    "-queuecapacity", Arg.Set_int queue_capacity, " <int> set the queues capacities (10 by default, 0 = infinite)";
    "-fabpriority", Arg.Set_int fabric_priority, 
                    " <int> set the fabric priority w.r.t agents\n";
    "-mc", Arg.String set_mc, " < promela > model checking mode\n";
  ]

let alspecs = Arg.align specs

let cin =
  let ifile = ref None in
  let set_file s =
    if Filename.check_suffix s ".flax" then ifile := Some s
    else if Filename.check_suffix s ".xml" then 
      begin
	Format.eprintf "converting to flax...@.";
	let f = s^".flax" in
	let com = Printf.sprintf "%s python/iflow2flax.py %s" Version.runpy s in
	ignore (Sys.command com);
	ifile := Some f
      end
    else raise (Arg.Bad "no .flax o .xml extension");
  in
  Arg.parse alspecs set_file usage;
  match !ifile with 
  | Some f -> file_in := f ; open_in f 
  | None -> stdin

let file_in = !file_in
let debug = !debug
let show_graph = !show_graph
let simu = !simu
let file_out = 
  if !file_out = "" then   
    (Filename.chop_suffix file_in ".flax")^".galm"
  else !file_out
let cout = open_out file_out
let () = Random.init !seed
let fabric_priority = !fabric_priority
let single = !single
let verbose = !verbose
let fabric = !fabric
let steps = !steps
let mc = !mc
let file_mc = 
  let f = Filename.chop_suffix file_out ".galm" in
  match mc with
    | "cubicle" -> f^".cub"
    | "promela" -> f^".pml"
    | _ -> ""

let file_simu = file_out ^".simu"
let cmc = if file_mc="" then stdout else open_out file_mc
let csimu = open_out file_simu
let queue_capacity = !queue_capacity
