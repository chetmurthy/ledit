(* $Id$ *)
(* Copyright (c) 2001 INRIA *)

#load "q_MLast.cmo";
#load "pa_extfun.cmo";

open Pcaml;
open Spretty;

value loc = (0, 0);

value expr e dg k = pr_expr.pr_fun "top" e dg k;
value patt e dg k = pr_patt.pr_fun "top" e dg k;
value expr_fun_args ge = Extfun.apply pr_expr_fun_args.val ge;

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

value rec list elem =
  fun
  [ [] -> fun _ k -> k
  | [x] -> fun dg k -> [: `elem x dg k :]
  | [x :: l] -> fun dg k -> [: `elem x "" [: :]; list elem l dg k :] ]
;

value rec listwbws elem b sep el k =
  match el with
  [ [] -> [: b; k :]
  | [x] -> [: `elem b x k :]
  | [x :: l] -> [: `elem b x [: :]; listwbws elem [: `sep :] sep l k :] ]
;

value rec bind_list b pel k =
  match pel with
  [ [pe] -> let_binding b pe k
  | pel ->
      Vbox [: `HVbox [: :]; listwbws let_binding b (S LR "and") pel k :] ]
and let_binding b (p, e) k =
  BEbox [: let_binding0 [: b; `patt p "" [: :] :] e [: :]; k :]
and let_binding0 b e k =
  let (pl, e) = expr_fun_args e in
  [: `HVbox [: `HVbox b; `HOVbox (list patt pl "" [: `S LR "=" :]) :];
     `expr e "" k :]
;

let lev = find_pr_level "top" pr_str_item.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:str_item< value $p$ = let $p1$ = $e1$ in let $list:pel2$ in $e$ >>
    when is_local_def p [(p1, e1)] pel2 e ->
      fun curr next _ k ->
        let pel1 = [(p1, e1)] in
        let r = [: :] in
        [: `Vbox
             [: `HVbox [: :];
                `bind_list [: `S LR "local"; r :] pel1 [: `S LR "in" :];
                curr <:str_item< value $list:pel2$ >> "" k :] :] ];
