(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*               Daniel de Rauglaudre, INRIA Rocquencourt              *)
(*                                                                     *)
(*  Copyright 2001-2007 Institut National de Recherche en Informatique *)
(*  et Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";

open Pcaml;

value expr_of_patt p =
  let loc = MLast.loc_of_patt p in
  match p with
  [ <:patt< $lid:x$ >> -> <:expr< $lid:x$ >>
  | _ -> Stdpp.raise_with_loc loc (Stream.Error "identifier expected") ]
;

value fst3 (a,b,c) = a ;

EXTEND
  str_item:
    [ [ "local"; rf = [ "rec" -> True | -> False ];
        lb = LIST1 let_binding SEP "and"; "in"; "value";
        rf1 = [ "rec" -> True | -> False ];
        lb1 = LIST1 let_binding SEP "and" ->
          let pl = List.map fst3 lb1 in
          let el = List.map expr_of_patt pl in
          <:str_item<
           value ($list:pl$) =
             let $opt:rf$ $list:lb$ in
             let $opt:rf1$ $list:lb1$ in
             ($list:el$) >> ] ]
  ;
END;
