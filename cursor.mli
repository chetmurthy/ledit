(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*       Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type t 'a = 'x;

exception Failure;

value create : unit -> t 'a;
value before : t 'a -> unit;
value after : t 'a -> unit;
value insert : t 'a -> 'a -> unit;
value insert_last : t 'a -> 'a -> unit;
value peek : t 'a -> 'a;
value peek_last : t 'a -> 'a;
value goto_first : t 'a -> unit;
value goto_last : t 'a -> unit;
value get_all : t 'a -> list 'a;
value is_last_line : t 'a -> bool;
