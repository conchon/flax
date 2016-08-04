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

open Version
open Options
open Format
open Common
open Ast
open Graph
open Galm

(* Graph representation of a .flax file. We first create the modules
   for representing nodes and arcs.

   ** Nodes **

   Given a entry :

   node N [ agt = A, shape = TASK,  label = "...P", ... ]

   we will simply represent the node N by a string "N" (the name of
   the node). The attributes of N are saved in a separate hashtable
   mapping nodes'name to values of type Ast.node.


   ** Arcs **

   Similarly, given a entry : 

   arc A [ from = N1, to = N4, shape = MESSAGE, ...]

   we will simply represent the arc A by a string "A" and save its
   attributes in a separate hashtable which maps arcs'names to values
   of type Ast.arc.

*)

module Node = struct
  type t = string
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let print fmt n = fprintf fmt "%s" n
end

module Arc = struct
  type t = string
  let compare = compare
  let default = ""
end

(* Data structures for representing iFlows.

   An iFlow will be represented by *two* graphs : 

     - g_seq : the control flow graph (Sequence arcs)
     - g_msg : the message passing graph (Message arcs)

   The graph structure of those two graphs is an imperative DAG with
   (concrete) labels (module G). Here the labels are just the types of
   the nodes and the arcs defined above, i.e strings.

   The node and arc attributes are respectively saved in the
   hashtables m_nodes and m_arcs. We need more information for
   compiling that what is contained in Ast.node and Ast.arc.

   For arcs (type arc_t), we add the locations attached to an arc
   (field locs of arc_t). Currently, an arc can contains a *list* of
   locations (who knows, we will may be need more than one location
   per node in the futur).

   For nodes (type node_t), we add information about services. We need
   to know if a node : 
      
       - start a service
       - end a service
       - is both the start and the end of a service
       - is inside a service

   Except for an *inservice* node, the information of type
   service_locs associated to a service are about locations :
   
   - locs_in : locations for starting a sercice
   - locs_out : locations produced when the service in started
   - locs_eps : locations produced when the node is the end of a service

*)

module G = Imperative.Digraph.ConcreteLabeled(Node)(Arc)

type location = string
type service_name = string

type service_locs = {
  service_name : service_name ;
  locs_in : location list;
  locs_out : location list;
  locs_eps : location list;
}

type service_t = 
  | InService of service_name
  | StartService of service_locs
  | EndService of service_locs
  | StartEndService of service_locs

type node_t = { node_id : Ast.node; mutable node_service : service_t }

type arc_t = { arc : Ast.arc; mutable locs : string list }
type graph = {
  m_nodes : (string, node_t ) Hashtbl.t;
  m_arcs : (string, arc_t) Hashtbl.t;
  g_seq : G.t;
  g_msg : G.t;
}

(* To perfom DFS graph traversal on graphs created with G *)
module GDfs = Traverse.Dfs(G)

(* Name generators:

   We distinguish between "standard" locations (starting with 'L')
   and epilon locations (starting with 'EL').

*)

let gen_standard_loc = 
  let i = ref 0 in
  fun () -> incr i; "L"^(string_of_int !i)

let gen_epsilon_loc = 
  let i = ref 0 in
  fun () -> incr i; "EL"^(string_of_int !i)

let gen_link = 
  let i = ref 0 in
  fun () -> incr i; "LINK"^(string_of_int !i)
			   
(* Auxilary functions mainly on the graph structure *)

let set_of_list l = List.fold_right S.add l S.empty

let filter g p = 
  G.fold_edges_e (fun e l -> if p e then e::l else l) g []

let find_n graph n = 
  try Hashtbl.find graph.m_nodes n with Not_found -> failwith ("find_n : "^n)

let find_e graph e = 
  try Hashtbl.find graph.m_arcs e with Not_found -> failwith ("find_e : "^e)
      
let owner_of_node graph e = 
  try (Hashtbl.find graph.m_nodes e).node_id.node_agent
  with Not_found -> failwith "owner_of_node"

let replace_edge graph n1 n2 e n'1 n'2 =
  G.remove_edge_e graph.g_msg (n1, e, n2);
  G.add_edge_e graph.g_msg (n'1, e, n'2);
  let ee = Hashtbl.find graph.m_arcs e in
  Hashtbl.replace graph.m_arcs e 
    { ee with arc = { ee.arc with arc_from = n'1; arc_to = n'2 } }

let remove_vertex graph n1 n2 = 
  G.remove_vertex graph.g_msg n1;
  G.remove_vertex graph.g_msg n2

let service_of graph n = 
  try match (find_n graph n).node_service with
    | StartService { service_name = s }
    | StartEndService { service_name = s }
    | EndService { service_name = s } 
    | InService s -> s
  with Not_found -> failwith "service_of"

(* =-=-=-=-=-=-= Pretty printing =-=-=-=-=-=-= *)

let pp_nodes fmt = List.iter (fprintf fmt "%a " Node.print)

let pp_graph graph = 
  (* output sequence graph *)
  let module GSeqDot = Graphviz.Dot(struct
    include G 
    let edge_attributes (_,e,_) = 
      let v = ref "" in
      List.iter (fun x -> v:=!v^x) (find_e graph e).locs;
      [`Style `Solid; `Label !v]
    let default_edge_attributes _ = [`Style `Solid ]
    let get_subgraph _ = None
    let vertex_attributes v = 
      match (find_n graph v).node_service with
	| StartService { service_name = s; locs_in = il; locs_out = ol} -> 
	  let m = 
	    fprintf str_formatter "%s\n Starts %s\n (%a -> %a)" v s 
	      pp_nodes il pp_nodes ol;
	    flush_str_formatter ()
	  in
	  [`Label m; `Shape `Doublecircle]
	| EndService { service_name = s; locs_eps = el} -> 
	  let m = 
	    fprintf str_formatter "%s\n Ends %s\n Loc %a" v s pp_nodes el;
	    flush_str_formatter ()
	  in
	  [`Label m; `Shape (`Polygon (6,0.))]
	| StartEndService 
	    { service_name = s; locs_in = il; locs_out = ol; locs_eps = el } ->
	  let m = 
	    fprintf str_formatter 
	      "%s\n StartEnds %s\n (%a -> %a)\n Loc %a" v s pp_nodes il 
	      pp_nodes ol  pp_nodes el;
	    flush_str_formatter ()
	  in
	  [`Label m; `Shape (`Polygon (12,0.))]
	  
	| InService _ -> [`Label v; `Fillcolor 123]
    let vertex_name v = v
    let default_vertex_attributes _  = [`Shape `Circle]
    let graph_attributes _ =  []
  end)
  in
  let f_seq = Filename.temp_file "graph" "-seq.dot" in
  let f_seq_pdf = (Filename.chop_suffix f_seq ".dot")^".pdf" in
  let cout_seq = open_out f_seq in
  GSeqDot.output_graph cout_seq graph.g_seq;
  close_out cout_seq;
  let com = Printf.sprintf "%s -Tpdf %s -o %s" rundot f_seq f_seq_pdf in
  ignore (Sys.command com);
  let com = Printf.sprintf "%s %s" runpdf f_seq_pdf in
  ignore (Sys.command com);

  (* output message graph *)

  let module GMsgDot = Graphviz.Dot(struct
    include G 
    let edge_attributes (_,e,_) = 
      let v = ref "" in
      List.iter (fun x -> v:=!v^x) (find_e graph e).locs;
      [`Style `Dotted; `Label !v]
    let default_edge_attributes _ = [`Style `Solid ]
    let get_subgraph _ = None
    let vertex_attributes v = 
      match (find_n graph v).node_service with
	| StartService { service_name = s; locs_in = il; locs_out = ol } -> 
	  let m = 
	    fprintf str_formatter "%s\n Starts %s\n (%a -> %a)" 
	      v s pp_nodes il pp_nodes ol ;
	    flush_str_formatter ()
	  in
	  [`Label m; `Shape `Doublecircle]
	| EndService { service_name = s; locs_eps = el} -> 
	  let m = 
	    fprintf str_formatter "%s\n Ends %s\n Loc %a" v s pp_nodes el;
	    flush_str_formatter ()
	  in
	  [`Label m; `Shape (`Polygon (6,0.))]
	| StartEndService 
	    { service_name = s; locs_in = il; locs_out = ol; locs_eps = el } ->
	  let m = 
	    fprintf str_formatter 
	      "%s\n StartEnds %s\n (%a -> %a)\n Loc %a" v s pp_nodes il 
	      pp_nodes ol pp_nodes el;
	    flush_str_formatter ()
	  in
	  [`Label m; `Shape (`Polygon (12,0.))]

	| InService _ -> [`Label v; `Fillcolor 123]
    let vertex_name v = v
    let default_vertex_attributes _  = [`Shape `Circle]
    let graph_attributes _ =  []
  end)
  in
  let f_msg = Filename.temp_file "graph" "-msg.dot" in
  let f_msg_pdf = (Filename.chop_suffix f_msg ".dot")^".pdf" in
  let cout_msg = open_out f_msg in
  GMsgDot.output_graph cout_msg graph.g_msg;
  close_out cout_msg; 
  let com = Printf.sprintf "%s -Tpdf %s -o %s" rundot f_msg f_msg_pdf in
  ignore (Sys.command com);
  let com = Printf.sprintf "%s %s" runpdf f_msg_pdf in
  ignore (Sys.command com)

let pp_services fmt graph = 
  Hashtbl.iter 
    (fun n nd -> 
      match nd.node_service with
	| StartService { service_name = o } ->
	  fprintf fmt "\n %s starts a service (%s)\n" n o
	| EndService { service_name = s } ->
	  fprintf fmt "\n %s ends the service %s \n"  n s
	| _ -> () ) graph.m_nodes


(* Initialisation of the graph data structure by extracting
   information from an iFlow.
   
   The entry point (init_graph) iterates on the list of nodes and arcs
   to:

      1. add an arc (or node) into the graph structure
      2. fill the hashtables with the information found in the AST

   Note: 

   1. Initially, each node n is considered as an *InService n*
   node. We perform a second pass to discover the exact nature of a
   node (Start, End, etc.) as well as the name of the service it
   belongs to.

   2. An arc N1 -- A --> N2 is just represented by a triple of strings
   ("N1", "A", "N2")

*)

let add_node graph n = 
  Hashtbl.add graph.m_nodes 
    n.node_name { node_id = n; node_service = InService n.node_name };
  G.add_vertex graph.g_seq n.node_name

let add_edge graph a = 
  Hashtbl.add graph.m_arcs a.arc_name { arc = a; locs = [] };
  if a.arc_shape = Sequence then 
    G.add_edge_e graph.g_seq (a.arc_from, a.arc_name, a.arc_to)
  else
    G.add_edge_e graph.g_msg (a.arc_from, a.arc_name, a.arc_to)

let init_graph graph iflow =
  List.iter 
    (fun d -> 
      List.iter (add_node graph) d.diag_nodes;
      List.iter (add_edge graph) d.diag_arcs)
    iflow.diagrams


(* Find the services of an iFlow. The name of a service is the name of
   the node that starts the service (we could add the name of the
   agent ?).

   We first look for the nodes that start a service. By definition, a
   node v stars a service is it has no incoming "SEQUENCE" arcs. 

   Then, we perform a DFS from v to find the nodes that end its
   service (i.e. nodes that have no outgoing "SEQUENCE" arcs). We also
   use this traversal to fix the service name of each reachable node.

   We generate a new epsilon location for each ending node. Those
   locations will be located after and save in the loc_in field of the
   starting node.

*)

let is_starting graph v = 
  G.in_degree graph.g_seq v = 0

let is_ending graph v = 
  G.out_degree graph.g_seq v = 0

let find_services graph = 
  G.iter_vertex 
    (fun v -> 
      if is_starting graph v then 
	begin
	  let node_v = find_n graph v in
	  let start = 
	    { service_name = v; 
	      locs_in = []; 
	      locs_out = [];
	      locs_eps = [] }
	  in
	  if is_ending graph v then
	    node_v.node_service <- 
	      StartEndService { start with locs_eps = [ gen_epsilon_loc () ] }
	  else
	    begin
	      node_v.node_service <- StartService start;
	      GDfs.prefix_component 
		(fun n -> 
		  let node_n = find_n graph n in
		  match node_n.node_service with
		    | InService _ -> 
		      if is_ending graph n then 
			let end_s =
			  { service_name = v; 
			    locs_in = []; 
			    locs_out = [];
			    locs_eps = [ gen_epsilon_loc ()]; }
			in
			node_n.node_service <- EndService end_s
		      else 
			node_n.node_service <- InService v
		    | StartService _ | StartEndService _ | EndService _ -> ()
		) 
		graph.g_seq v;
	    end
	end
    )
    graph.g_seq
 

(* Surgery : expand the "SEQUENCE" graph with create and consume
   nodes. 

   We have to be very carefull with the status of a node (Start, End,
   ...) which may change when adding create or consume nodes.

   Surgery would have been simpler if done before find_services, but
   find_services would have been then much complicated to design. I'm
   not particularly happy with the code bellow :-(

   For each message arc  N1 --- e ---> N2, we have four situations : 

   1. N1 is a StartService node. The surgery is :

   (N1 + can_add + add ) --- e ---> N2_consume  ====> N2

   and the node (N1 + can_add + add ) is still a StartService node

   2. N1 is a StartEndService node. The surgery is : 

   (N1 + can_add + add) --- e ---> N2_consume  ====> N2

   and the node (N1 + can_add + add ) is still a StartEndService node


   3. N1 is an End node. The surgery is :

   N1 ====> N1_create --- e ---> N2_consume  ====> N2

   and N1 is now an *InService* node

   4. N1 is an InService node. The surgery is :

   N1 ====> N1_create --- e ---> N2_consume  ====> N2

   and N1 is still an InService node

   Remark : for each surgery, we have to fix the service status of
   each node N2 and N2_consume.

   1. N2 becomes a *InService* if it was a StartService, and N2_consume
   is a StartService
   
   2. N2 becomes an *EndService* if it was a *StartService*, and N2_consume
   is a StartService

   3. N2 stays as an EndService if it was an EndService, and N2_consume
   is an InService

   4. N2 stays as an InService if it was an InService, and N2_consume
   is an InService

*)


let new_arc_sequence n1 n2 = 
  { arc_name = n1^"_"^n2;
    arc_from = n1;
    arc_to = n2;
    arc_shape = Sequence;
    arc_label = "";
    arc_cond = None;
    arc_msg = None;  }

(* Create a new N_consume node, and fix its status according to the
   remark above *)

let new_consume_node graph e n = 
  let node_n = find_n graph n in
  let a = (find_e graph e).arc in
  let msg = match a.arc_msg with Some m -> m | None -> assert false in
  let agt_from = owner_of_node graph a.arc_from in
  let agt_to = owner_of_node graph a.arc_to in
  let msg = { msg with msg_from = agt_from ; msg_to = agt_to } in
  let node = 
    { node_n with 
      node_id = 
	{ node_n.node_id with
	  node_name = node_n.node_id.node_name^"_consume";
	  node_shape = Task;
	  node_label = "consume "^node_n.node_id.node_agent;
	  node_action = Consume msg   };
    }
  in
  (match node.node_service with
    | StartService { service_name = s} -> 
      node_n.node_service <- InService s

    | StartEndService ser -> 
      node_n.node_service <- EndService { ser with locs_in = []; locs_out = []};
      node.node_service <- StartService { ser with locs_eps = [] }
    
    | EndService { service_name = s} -> 
      node.node_service <- InService s
      
    | InService _ -> ()
    );
  node


let surgery graph = 

  (* We have to be carefull when adding or removing nodes/edges "on
     the fly" to an imperative graph when we iterate on that
     graph. For safety, I keep in these two lists what must be done
     *after* the iteration. *)

  let remove_list = ref [] in (* arcs to remove *)
  let add_list = ref [] in (* arcs to add *)

  G.iter_edges_e 
    (fun (n1, e, n2) ->

      (* first, we create the n2_consume node *)
      let node_n2_consume = new_consume_node graph e n2 in
      let nn2 = node_n2_consume.node_id.node_name in
      Hashtbl.add graph.m_nodes nn2 node_n2_consume;
      let a_consume = new_arc_sequence nn2 n2 in
      add_edge graph a_consume;

      let a = (find_e graph e).arc in
      let msg = match a.arc_msg with Some m -> m | None -> assert false in
      let agt_from = owner_of_node graph a.arc_from in
      let agt_to = owner_of_node graph a.arc_to in
      let msg = { msg with msg_from = agt_from ; msg_to = agt_to } in
      
      let node_n1 = find_n graph n1 in begin
	match node_n1.node_service with
	  | StartService _ | StartEndService _ ->
	    (* In thes cases, we don't create a new node, just inline
	       the can_add predicate in n1 *)

	    node_n1.node_id.node_action <- Create msg;
	    add_list := (n1, e, nn2) :: !add_list
	  | EndService _ | InService _ ->
	    (* In these two cases, we have to create a new n1_create
	       node *)
	    
	    let end_s = 
	      { service_name = service_of graph n1;
		locs_in = []; locs_out = [];
		locs_eps = [gen_epsilon_loc()]  }
	    in
	    let node_n1_create =  
	      { node_id = 
		  { node_n1.node_id with 
		    node_name = node_n1.node_id.node_name^"_create";
		    node_shape = Task;
		    node_label = "create";
		    node_action = Create msg };
		node_service = EndService end_s }
	    in
	    (match node_n1.node_service with
	      | EndService { service_name = s; locs_eps = el} -> 
		node_n1.node_service <- InService s;
		node_n1_create.node_service <- 
		  EndService { service_name = s; 
			       locs_in = [] ; locs_out = []; locs_eps = el }
		  
	      |  _ -> ()  );
	    let nn1 = node_n1_create.node_id.node_name in
	    Hashtbl.add graph.m_nodes nn1 node_n1_create; 
	    let a_create = new_arc_sequence n1 nn1 in
	    add_edge graph a_create;
	    add_list := (nn1, e, nn2) :: !add_list;
      end;
      remove_list := (n1, e, n2) :: !remove_list
    ) graph.g_msg;
  (* Now we iterate on add_list and remove_list *)
  List.iter 
    (fun (n'1, e, n'2) -> 
      G.add_edge_e graph.g_msg (n'1, e, n'2);
      let ee = try Hashtbl.find graph.m_arcs e with Not_found -> 
	failwith "iter"
      in
      Hashtbl.replace graph.m_arcs e 
	{ ee with arc = { ee.arc with arc_from = n'1; arc_to = n'2 } }
    ) !add_list;
  List.iter (G.remove_edge_e graph.g_msg) !remove_list


(* We compute the epsilon locations of services after surgery. The
   computation depends on the 'single' service assumption.

   We first collect in two separate tables (epsilons_by_agent and
   epsilons_by_service) the epsilon locations associated to an agent
   and to a service.

   For each StartService or StartEndService node v.

   1. *single* mode : 

   All the starting nodes of an agent A are initialized with the same
   set of *input* epsilon locations associated to A in the
   epsilons_agent table.

   The set of *output* locations for each starting node v is equal to
   eps_all_services \ eps_of_service, where eps_of_service is the set
   of epsilon locations of the service of v.

   2. *not single* mode : we simply update the value StartService or
   StartEndService with the set of epsilon locations associated in the
   epsilons_by_service table.

*)

let epsilon_tables graph = 
  let epsilons_by_agent = Hashtbl.create 42 in
  let epsilons_by_service = Hashtbl.create 42 in
  GDfs.prefix 
    (fun v ->
      let node_v = find_n graph v in
      match node_v.node_service with      
	| EndService { service_name = s; locs_eps = el } 
	| StartEndService { service_name = s; locs_eps = el } -> 
	  let sl = set_of_list el in
	  let o = owner_of_node graph v in

	  let locs = 
	    try Hashtbl.find epsilons_by_agent o with Not_found -> S.empty in
	  Hashtbl.replace epsilons_by_agent o (S.union sl locs);

	  let locs = 
	    try Hashtbl.find epsilons_by_service s with Not_found -> S.empty in
	  Hashtbl.replace epsilons_by_service s (S.union sl locs)

	| _ -> ()
    )
    graph.g_seq;
 epsilons_by_agent, epsilons_by_service

let epsilon_locations graph = 
  let epsilons_by_agent, epsilons_by_service = epsilon_tables graph in
  G.iter_vertex 
    (fun v ->
      let n = find_n graph v in
      match n.node_service with
	| StartService ({ service_name = s } as sr) 
	| StartEndService ({ service_name = s } as sr) ->
	  if single then 
	    let o = owner_of_node graph v in
	    let eps_all_services = 
	      try Hashtbl.find epsilons_by_agent o 
	      with Not_found -> failwith "unknown agent"
	    in
	    let eps_of_service = 
	      try Hashtbl.find epsilons_by_service s 
	      with Not_found -> failwith "unknown service"
	    in
	    let s_out = S.diff eps_all_services eps_of_service in
	    match n.node_service with
	      | StartService _ ->
		n.node_service <- 
		  StartService 
		  { sr with 
		    locs_in = S.elements eps_all_services;
		    locs_out = S.elements s_out }
	      | StartEndService _ ->
		n.node_service <- 
		  StartEndService 
		  { sr with 
		    locs_in = S.elements eps_all_services;
		    locs_out = S.elements s_out }
	      | _ -> assert false
	  else
	    let sin = 
	      try Hashtbl.find epsilons_by_service s
	      with Not_found -> 
		eprintf "fix_start (normal mode) node %s unknown@." v; 
		exit 2
	    in
	    let sin = S.elements sin in
	    (match n.node_service with
	      | StartService _ ->
		n.node_service <- StartService { sr with locs_in = sin }
	      | StartEndService _ ->
		n.node_service <- StartEndService { sr with locs_in = sin }
	      | _ -> assert false)
	| _ -> () )
    graph.g_seq


(* Associate a "standard" location to each arc *)

let standard_locations graph = 
  Hashtbl.iter 
    (fun _ ({locs = l} as a) -> a.locs <- gen_standard_loc() :: l ) 
    graph.m_arcs


(* =-=-=-=-=-=-=  Translation to GAML =-=-=-=-=-=-= 

   Since the language of guards and actions is very limited, the
   translation to GALM mainly consists in computing the sets of input
   and ouput locations for each node.


   Input locations : The set of the input locations S of a node v
   depends on the "service" nature of v :
   
   - If v is an InService or EndService node, then S is the union of
   the locations of the arcs going *into* v

   - If v is an StartService or StartEndService, then S is the union
   of the epsilon locations of v


   Output locations : The set of the output locations S of a node v
   depends on the "service" nature of v :

   - If v is an InService node, then S is the set of the union of the
   *outgoing* arcs of v

   - If v is an EndService node, the S is the set of epsilon locations
   contained in v

   - If v is a StartEndService node, then S is the union of the output
   locations and epsilon locations contained in v

   - If v is a StartService node, then S is the union of the ouput
   locations contained in v and the locations of the *outgoing* arcs
   of v

*)

let collect_input_locations graph il n = 
  match (find_n graph n).node_service with
    | InService _ | EndService _ -> 
      let rin = 
	G.fold_pred_e 
	  (fun (_,e,_) r -> S.union (set_of_list (find_e graph e).locs) r) 
	  graph.g_seq n S.empty      
      in
      rin, il

    | StartService { locs_in = locs } | StartEndService { locs_in = locs } ->
      let l = set_of_list locs in
      l, S.union l il

let collect_output_locations graph n = 
  match (find_n graph n).node_service with
    | InService _ -> 
      G.fold_succ_e 
	(fun (_,e,_) r -> S.union (set_of_list (find_e graph e).locs) r) 
	graph.g_seq n S.empty      

    | EndService { locs_eps = l } ->
      set_of_list l

    | StartEndService { locs_out = lo; locs_eps = le } -> 
      S.union (set_of_list lo) (set_of_list le)

    | StartService { locs_out = l } ->
      G.fold_succ_e 
	(fun (_,e,_) r -> S.union (set_of_list (find_e graph e).locs) r) 
	graph.g_seq n (set_of_list l)      

  
let guard_of n = if n.node_guard = "" then [] else [ Other_literal ]
  
let action_of n = 
  match n.node_action with
    | Create m -> 
      let m = 
	{ msg_bin_proto = m.msg_proto; 
	  msg_bin_opcode = m.msg_opcode;
	  msg_bin_from = m.msg_from;
	  msg_bin_to = m.msg_to;     }
      in
      [ Can_add m ], [ Add m ]

    | Consume m ->
      let pat = 
	{ msg_bin_pat_proto = m.msg_proto;
	  msg_bin_pat_opcode = m.msg_opcode;
	  msg_bin_pat_from = Pat_Const m.msg_from;
	  msg_bin_pat_to = m.msg_to;    }
      in
      [ Can_rm pat ], [ Rm pat ]

    | Other _ -> 
      [], [ Other_action ]

let translate_to_galm graph = 
  G.fold_vertex 
    (fun n prog -> 
      let rin, init_locs = collect_input_locations graph prog.init_locs n in
      let rout = collect_output_locations graph n in
      let nid = (find_n graph n).node_id in
      let g = guard_of nid in
      let g', a = action_of nid in
      let r = 
	{ rule_name = nid.node_name;
	  rule_agent = nid.node_agent;
	  rule_loc_in = rin;
	  rule_guard = g@g';
	  rule_loc_out = rout;
	  rule_action = a 
	} 
      in 
      { prog with 
	rules = r :: prog.rules; 
	init_locs = init_locs }
    ) 
    graph.g_seq { rules = []; init_locs = S.empty; agents = []}


let inline_links g =
  let inlinks = Hashtbl.create 107 in 
  let outlinks = Hashtbl.create 107 in  
  G.iter_edges_e
    (fun e ->
     let v1 = G.E.src e in
     let v2 = G.E.dst e in
     let n1 = (find_n g v1).node_id in
     let n2 = (find_n g v2).node_id in
     if n1.node_shape = InLink then
       let l = try e :: (Hashtbl.find inlinks n1.node_name)
	       with Not_found -> [e] in
       Hashtbl.add inlinks n1.node_name l
     else if n2.node_shape = OutLink then
       let l = try e :: (Hashtbl.find outlinks v2)
	       with Not_found -> [e] in
       Hashtbl.add outlinks v2 l
    ) g.g_seq;
  
  Hashtbl.iter
    (fun v out_edges ->
     let in_edges = Hashtbl.find inlinks (find_n g v).node_id.node_connect in
     match in_edges with
     | [] | _ :: _ :: _ -> assert false
     | [ie] ->
	List.iter
	  (fun oe ->
	   let from = G.E.src oe in
	   let dst = G.E.dst ie in
	   let arc_name = gen_link () in
	   Format.eprintf "link from : %s to %s@." from dst;
	   G.add_edge_e g.g_seq (from, arc_name, dst);
	   let a = {
	       arc_name = arc_name;
	       arc_from = from;
	       arc_to = dst;
	       arc_shape = Sequence;
	       arc_label = "";
	       arc_cond = None;
	       arc_msg = None;
	     }
	   in
	   Hashtbl.add g.m_arcs arc_name { arc = a; locs = [] };
	  )
	  out_edges	
    ) outlinks;

  (* cleaning *)
  Hashtbl.iter
    (fun v out_edges ->
     List.iter (G.remove_edge_e g.g_seq) out_edges;
     G.remove_vertex g.g_seq v) outlinks;

  Hashtbl.iter
    (fun _ in_edges ->
     match in_edges with
     | [] | _ :: _ :: _ -> assert false
     | [ie] ->
	G.remove_edge_e g.g_seq ie;
	G.remove_vertex g.g_seq (G.E.src ie)) inlinks

     
    
  
(* Entry point *)

let compile iflow = 
  let m_nodes = Hashtbl.create 2007 in
  let m_arcs = Hashtbl.create 2007 in
  let g_seq = G.create ~size:2007 () in
  let g_msg = G.create ~size:2007 () in
  let graph = { 
    m_nodes = m_nodes; 
    m_arcs = m_arcs; 
    g_seq = g_seq; 
    g_msg = g_msg } 
  in
  init_graph graph iflow;
  inline_links graph;

  if debug && verbose = 2 then pp_graph graph;
  
  find_services graph;
  surgery graph;
  epsilon_locations graph;
  standard_locations graph;
  
  if show_graph then pp_graph graph;
 
  let g = translate_to_galm graph in
  { g with agents = iflow.agents }


    

