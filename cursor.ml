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

type t 'a =
  { before : mutable list 'a;
    current : mutable (option 'a);
    after : mutable list 'a }
;

exception Failure;

value create () = {before = []; current = None; after = []};

value before c =
  match c.before with
  [ [] -> raise Failure
  | [x :: l] ->
      do {
        match c.current with
        [ Some y -> c.after := [y :: c.after]
        | _ -> () ];
        c.current := Some x;
        c.before := l;
      } ]
;

value after c =
  match c.current with
  [ None -> raise Failure
  | Some y ->
      do {
        c.before := [y :: c.before];
        match c.after with
        [ [] -> c.current := None
        | [x :: l] ->
            do {
              c.current := Some x;
              c.after := l;
            } ]
      } ]
;

value is_last_line c = c.current = None;

value insert c x =
  do {
    match c.current with
    [ Some y -> c.before := [y :: c.before]
    | None -> () ];
    c.current := Some x;
  }
;

value insert_last c x =
  match c.current with
  [ Some _ -> c.after := c.after @ [x]
  | None -> c.current := Some x ]
;

value peek c =
  match c.current with
  [ Some y -> y
  | None -> raise Failure ]
;

value peek_last c =
  let rec peek_rec =
    fun
    [ [] -> raise Failure
    | [x] -> x
    | [_ :: l] -> peek_rec l ]
  in
  peek_rec c.after
;

value rec goto_first c =
  try while True do { before c } with
  [ Failure -> () ]
;

value rec goto_last c =
  try while True do { after c } with
  [ Failure -> () ]
;

value get_all c =
  let end_list =
    match c.current with
    [ Some y -> [y :: c.after]
    | None -> c.after ]
  in
  List.rev_append c.before end_list
;
