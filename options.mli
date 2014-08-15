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

val file_in : string
val file_out : string
val file_mc : string
val file_simu : string

val cin : in_channel
val cout : out_channel
val cmc : out_channel
val csimu : out_channel

val debug : bool
val show_graph : bool
val simu : bool
val fabric : int
val fabric_priority : int
val queue_capacity : int
val single : bool
val verbose : int
val steps : int
val mc : string
