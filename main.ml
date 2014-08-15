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
open Lexing
open Format
open Options
open Ast

(** Entry point of Flax *)

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  printf "File \"%s\", line %d, characters %d-%d:" file_in l fc lc

let _ = 
  let lb = from_channel cin in 
  try
    let s = Parser.iflow Lexer.token lb in
    let r = Compile.compile s in
    let fmt = formatter_of_out_channel cout in
    let simu_fmt = formatter_of_out_channel csimu in
    Galm.pp fmt r;
    if simu then 
      begin
	(match fabric with
	  | 1 -> 
	    let module G = Galm.Make(Fabrics.Fab1) in 
	    G.run simu_fmt r
	  | 2 -> 
	    let module G = Galm.Make(Fabrics.Fab2) in 
	    G.run simu_fmt r
	  | 3 -> 
	    let module G = Galm.Make(Fabrics.Fab3) in 
	    G.run simu_fmt r
	  | 4 -> 
	    let module G = Galm.Make(Fabrics.Fab4) in 
	    G.run simu_fmt r
	  | 5 -> 
	    let module G = Galm.Make(Fabrics.Fab5) in 
	    G.run simu_fmt r
	  | _ -> failwith "unkown fabric number");
	close_out csimu;       
	let com = Printf.sprintf "%s %s" runemacs file_simu in
	ignore (Sys.command com)
      end;

    (match mc with
      | "" -> ()
      | "promela" -> 
	let fmt = formatter_of_out_channel cmc in
	(match fabric with
	  | 1 -> 
	    let module MC = Promela.Make(Fabrics.Fab1) in
	    MC.compile fmt r
	  | 2 -> 
	    let module MC = Promela.Make(Fabrics.Fab2) in
	    MC.compile fmt r
	  | 3 -> 
	    let module MC = Promela.Make(Fabrics.Fab3) in
	    MC.compile fmt r
	  | 4 -> 
	    let module MC = Promela.Make(Fabrics.Fab4) in
	    MC.compile fmt r
	  | 5 -> 
	    let module MC = Promela.Make(Fabrics.Fab5) in
	    MC.compile fmt r
	  | _ -> failwith "unkown fabric number");
	close_out cmc;
	let com = Printf.sprintf "%s %s" runispin file_mc in
	ignore (Sys.command com)
	  
      | "cubicle" -> 
	let fmt = formatter_of_out_channel cmc in
	(match fabric with
	  | 1 -> 
	    let module MC = Cubicle.Make(Fabrics.Fab1) in
	    MC.compile fmt r
	  | _ -> failwith "fabric not currenlty supported by cubicle");
	close_out cmc;
	let com = Printf.sprintf "%s %s" runcubicle file_mc in
	ignore (Sys.command com)

      | _ -> failwith (mc^" not supported yet!"))
  with
  | Lexer.Lexical_error s -> 
     report (lexeme_start_p lb, lexeme_end_p lb);
     printf "lexical error: %s\n@." s;
     exit 2

  | Parsing.Parse_error ->
     let  loc = (lexeme_start_p lb, lexeme_end_p lb) in
     report loc;
     printf "\nsyntax error\n@.";
     exit 2

  | Failure s ->
     let  loc = (lexeme_start_p lb, lexeme_end_p lb) in
     report loc;
     printf "\nError : %s \n@." s;
     exit 2

  | _  ->
     eprintf "\n@{<u>Internal failure:@}.";
     exit 2
