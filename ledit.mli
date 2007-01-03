(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*                Daniel de Rauglaudre, INRIA Rocquencourt             *)
(*                                                                     *)
(*  Copyright 1997-2007 Institut National de Recherche en Informatique *)
(*  et Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

value input_char : in_channel -> char;
value set_prompt : string -> unit;
value get_prompt : unit -> string;
value open_histfile : bool -> string -> unit;
value close_histfile : unit -> unit;
value set_max_len : int -> unit;
value set_son : int -> unit;
