(* $Id$ *)

open Pcaml;

value expr_of_patt p =
  let loc = MLast.loc_of_patt p in
  match p with
  [ <:patt< $lid:x$ >> -> <:expr< $lid:x$ >>
  | _ -> Stdpp.raise_with_loc loc (Stream.Error "identifier expected") ]
;

EXTEND
  str_item:
    [[ "local"; rf = [ "rec" -> True | -> False ];
       lb = LIST1 let_binding SEP "and"; "in";
       "value"; rf1 = [ "rec" -> True | -> False ];
       lb1 = LIST1 let_binding SEP "and" ->
         let pl = List.map fst lb1 in
         let el = List.map expr_of_patt pl in
         <:str_item<
           value ($list:pl$) =
             let $rec:rf$ $list:lb$ in
             let $rec:rf1$ $list:lb1$ in
             ($list:el$) >> ]]
  ;
END;
