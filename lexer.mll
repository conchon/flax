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

{
  open Lexing
  open Parser

  let keywords = Hashtbl.create 97
  let () = 
    List.iter 
      (fun (x,y) -> Hashtbl.add keywords x y)
      [ 
	"agent", AGENT;
	"global", GLOBAL;
	"repository", REPOSITORY;
	"diagram", DIAGRAM;
	"local", LOCAL;
	"node", NODE;
	"arc", ARC;
      ]

  let attributes = Hashtbl.create 97
  let () = 
    List.iter 
      (fun (x,y) -> Hashtbl.add attributes x y)
      [ 
	"owner", OWNER;
	"type", TYPE;
	"init", INIT;
	"guard", GUARD;
	"action", ACTION;
	"proto", PROTO;
	"data_type", DATATYPE;
	"repo", REPO;
	"data", DATA;
	"agt", AGT;
	"name", NAME;
	"shape", SHAPE;
	"label", LABEL;
	"pause", PAUSE;
	"msg", MSG;
	"cond", COND;
	"wac", WAC;
	"rac", RAC;
	"from", FROM;
	"to", TO;
	"opcode", OPCODE;
	"connect", CONNECT;
      ]
      
  let shapes = Hashtbl.create 97
  let () = 
    List.iter 
      (fun (x,y) -> Hashtbl.add shapes x y)
      [ 
	"TASK", TASK ;
	"PSYNC", PSYNC;
	"NPSYNC", NPSYNC;
	"START", START;
	"BRANCH", BRANCH;
	"INLINK", INLINK;
	"OUTLINK", OUTLINK;
	"SEQUENCE", SEQUENCE;
	"MESSAGE", MESSAGE 
      ]

  let constructors = Hashtbl.create 97
  let () = 
    List.iter 
      (fun (x,y) -> Hashtbl.add constructors x y)
      [ 
	"CfgWr", CFGWR;
	"CfgRd", CFGRD;
	"Compl", COMPL;
	"Yes", YES;
	"No", NO;
      ]
	       
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let string_buffer = Buffer.create 1024

  exception Lexical_error of string
}

let newline = '\n'
let space = [' ' '\t' '\r']
let integer = ['0' - '9'] ['0' - '9']*
let mident = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let ident = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | newline 
      { newline lexbuf; token lexbuf }
  | space+  
      { token lexbuf }
  | ident as id
      { try Hashtbl.find keywords id
	with Not_found -> 
	  try Hashtbl.find attributes id
	  with Not_found -> IDENT id }

  | mident as id 
      { try Hashtbl.find shapes id
	with Not_found -> 
	  try Hashtbl.find constructors id
	  with Not_found -> MIDENT id }
      
  | integer as i { INT i }

  | "("
      { LPAR }
  | ")"
      { RPAR }
  | "="
      { EQ }
  | "["
      { LSQ }
  | "]"
      { RSQ }
  | "{"
      { LBR }
  | "}"
      { RBR }
  | ","
      { COMMA }
  | "(*"
      { comment lexbuf; token lexbuf }
  | '"'
      { STRING (string lexbuf) }
  | eof 
      { EOF }
  | _ as c
      { raise (Lexical_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*)" 
      { () }
  | "(*" 
      { comment lexbuf; comment lexbuf }
  | eof
      { raise (Lexical_error "unterminated comment") }
  | newline 
      { newline lexbuf; comment lexbuf }
  | _ 
      { comment lexbuf }

and string = parse
  | '"'
      { let s = Buffer.contents string_buffer in
	Buffer.reset string_buffer;
	s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
	string lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"';
	string lexbuf }
  | _ as c
      { Buffer.add_char string_buffer c;
	string lexbuf }
  | eof
      { raise (Lexical_error "unterminated string") }
