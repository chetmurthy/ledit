(* $Id$ *)
(* Copyright (c) 2001 INRIA *)

#load "q_MLast.cmo";
#load "pa_extfun.cmo";

open Pcaml;
open Pretty;

value loc = (0, 0);

value expr e dg k = pr_expr.pr_fun "top" e dg k;
value patt e dg k = pr_patt.pr_fun "top" e dg k;

value is_local_def p pel1 pel2 e =
  try
    let dl1 =
      let pl =
        match p with
        [ <:patt< ($list:pl$) >> -> pl
        | p -> [p] ]
      in
      List.map (fun [ <:patt< $lid:s$ >> -> s | _ -> raise Not_found ]) pl
    in
    let (dl2, el) =
      let (pl, el) = List.split pel2 in
      let dl2 =
        List.map (fun [ <:patt< $lid:s$ >> -> s | _ -> raise Not_found ]) pl
      in
      (dl2, el)
    in
    let dl3 =
      let el =
        match e with
        [ <:expr< ($list:el$) >> -> el
        | e -> [e] ]
      in
      List.map (fun [ <:expr< $lid:s$ >> -> s | _ -> raise Not_found ]) el
    in
    dl1 = dl2 && dl1 = dl3
  with
  [ Not_found -> False ]
;

let lev = find_pr_level "top" pr_str_item.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:str_item< value $p$ = let $list:pel1$ in let $list:pel2$ in $e$ >>
    when is_local_def p pel1 pel2 e ->
      fun curr next _ k ->
        [: curr <:str_item< let $list:pel1$ in let $list:pel2$ in () >> ""
             k :] ];
