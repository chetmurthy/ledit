(* $Id$ *)

type t 'a = { before : mutable list 'a; after : mutable list 'a };

exception Failure;

value create () = {before = []; after = []};

value before c =
  match c.before with
  [ [] -> raise Failure
  | [x :: l] -> do c.after := [x :: c.after]; c.before := l; return () ]
;

value after c =
  match c.after with
  [ [] -> raise Failure
  | [x :: l] -> do c.after := l; c.before := [x :: c.before]; return () ]
;

value insert c x = c.before := [x :: c.before];

value insert_last c x = c.after := c.after @ [x];

value normalize c =
  match c.after with
  [ [_ :: _] -> ()
  | [] -> before c ]
;

value peek c =
  do normalize c; return
  match c.after with
  [ [x :: _] -> x
  | [] -> raise Failure ]
;

value peek_last c =
  let rec peek_rec =
    fun
    [ [] -> raise Failure
    | [x] -> x
    | [_ :: l] -> peek_rec l ]
  in
  do normalize c; return peek_rec c.after
;

value rec goto_first c =
  match c.before with
  [ [] -> ()
  | [x :: l] ->
      do c.after := [x :: c.after]; c.before := l; goto_first c; return () ]
;

value rec goto_last c =
  match c.after with
  [ [] -> ()
  | [x :: l] ->
      do c.before := [x :: c.before]; c.after := l; goto_last c; return () ]
;

