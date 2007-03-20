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

type a_char = 'abstract;

value input_char : in_channel -> a_char;
value print_a_char : a_char -> unit;

value set_prompt : string -> unit;
value get_prompt : unit -> string;
value open_histfile : bool -> string -> unit;
value close_histfile : unit -> unit;
value set_max_len : int -> unit;
value set_son : int -> unit;

value unset_meta_as_escape : unit -> unit;
value set_utf8 : unit -> unit;
