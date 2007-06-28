(* $Id$ *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
#load "pa_macro.cmo";

open Pcaml;

Pcaml.no_constructors_arity.val := False;

do {
  let odfa = Plexer.dollar_for_antiquotation.val in
  Plexer.dollar_for_antiquotation.val := False;
  let lexer = Plexer.gmake () in
  Grammar.Unsafe.gram_reinit gram lexer;
  Plexer.dollar_for_antiquotation.val := odfa;
  Grammar.Unsafe.clear_entry interf;
  Grammar.Unsafe.clear_entry implem;
  Grammar.Unsafe.clear_entry top_phrase;
  Grammar.Unsafe.clear_entry use_file;
  Grammar.Unsafe.clear_entry module_type;
  Grammar.Unsafe.clear_entry module_expr;
  Grammar.Unsafe.clear_entry sig_item;
  Grammar.Unsafe.clear_entry str_item;
  Grammar.Unsafe.clear_entry expr;
  Grammar.Unsafe.clear_entry patt;
  Grammar.Unsafe.clear_entry ctyp;
  Grammar.Unsafe.clear_entry let_binding;
  Grammar.Unsafe.clear_entry type_declaration;
};

Pcaml.parse_interf.val := Grammar.Entry.parse interf;
Pcaml.parse_implem.val := Grammar.Entry.parse implem;

value o2b =
  fun
  [ Some _ -> True
  | None -> False ]
;

value mksequence loc =
  fun
  [ [e] -> e
  | [] -> Stdpp.raise_with_loc loc (Failure "empty sequence")
  | el -> <:expr< do { $list:el$ } >> ]
;
      
value neg_string n =
  let len = String.length n in
  if len > 0 && n.[0] = '-' then String.sub n 1 (len - 1) else "-" ^ n
;

value mkumin loc f arg =
  match arg with
  [ <:expr< $int:n$ >> -> <:expr< $int:neg_string n$ >>
  | <:expr< $flo:n$ >> -> <:expr< $flo:neg_string n$ >>
  | _ ->
      let f = "~" ^ f in
      <:expr< $lid:f$ $arg$ >> ]
;

value mkuminpat loc arg =
  match arg with
  [ <:patt< $int:n$ >> -> <:patt< $int:neg_string n$ >>
  | <:patt< $flo:n$ >> -> <:patt< $flo:neg_string n$ >>
  | _ -> invalid_arg "mkuminpat" ]
;

IFDEF CAMLP4S THEN declare
value merge_couple loc1 loc2 =
  Stdpp.sub_loc loc1 0 (Stdpp.last_pos loc2 - Stdpp.first_pos loc1)
;

value loc_merge (x : MLast.loc) (y : MLast.loc) : MLast.loc =
  Stdpp.encl_loc x y
;
end ELSE declare
value merge_couple (bp, _) (_, ep) = (bp, ep);
value loc_merge (bp, _) (_, ep) = (bp, ep);
end END;

value mklistexp loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some e -> e
        | None -> <:expr< [] >> ]
    | [e1 :: el] ->
        let loc =
          if top then loc else loc_merge (MLast.loc_of_expr e1) loc
        in
        <:expr< [$e1$ :: $loop False el$] >> ]
;

value mklistpat loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some p -> p
        | None -> <:patt< [] >> ]
    | [p1 :: pl] ->
        let loc =
          if top then loc else loc_merge (MLast.loc_of_patt p1) loc
        in
        <:patt< [$p1$ :: $loop False pl$] >> ]
;

value mkexprident loc i j =
  let rec loop m =
    fun
    [ <:expr< $x$ . $y$ >> -> loop <:expr< $m$ . $x$ >> y
    | e -> <:expr< $m$ . $e$ >> ]
  in
  loop <:expr< $uid:i$ >> j
;

value mkassert loc e =
  match e with
  [ <:expr< False >> -> <:expr< assert False >>
  | _ -> <:expr< assert $e$ >> ]
;

value ipatt = Grammar.Entry.create gram "ipatt";
value with_constr = Grammar.Entry.create gram "with_constr";

EXTEND
  GLOBAL: sig_item str_item ctyp patt expr module_type module_expr
    let_binding type_declaration ipatt with_constr;
  module_expr:
    [ [ "functor"; "("; i = UIDENT; ":"; t = module_type; ")"; "->";
        me = SELF ->
          <:module_expr< functor ( $i$ : $t$ ) -> $me$ >>
      | "struct"; st = LIST0 [ s = str_item; ";" -> s ]; "end" ->
          <:module_expr< struct $list:st$ end >> ]
    | [ me1 = SELF; me2 = SELF -> <:module_expr< $me1$ $me2$ >> ]
    | [ me1 = SELF; "."; me2 = SELF -> <:module_expr< $me1$ . $me2$ >> ]
    | "simple"
      [ i = UIDENT -> <:module_expr< $uid:i$ >>
      | "("; me = SELF; ":"; mt = module_type; ")" ->
          <:module_expr< ( $me$ : $mt$ ) >>
      | "("; me = SELF; ")" -> <:module_expr< $me$ >> ] ]
  ;
  str_item:
    [ "top"
      [ "declare"; st = LIST0 [ s = str_item; ";" -> s ]; "end" ->
          <:str_item< declare $list:st$ end >>
      | "exception"; (_, c, tl) = constructor_declaration; b = rebind_exn ->
          <:str_item< exception $c$ of $list:tl$ = $b$ >>
      | "external"; i = LIDENT; ":"; t = ctyp; "="; pd = LIST1 STRING ->
          <:str_item< external $i$ : $t$ = $list:pd$ >>
      | "include"; me = module_expr -> <:str_item< include $me$ >>
      | "module"; i = UIDENT; mb = module_binding ->
          <:str_item< module $i$ = $mb$ >>
      | "module"; "type"; i = UIDENT; "="; mt = module_type ->
          <:str_item< module type $i$ = $mt$ >>
      | "open"; i = mod_ident -> <:str_item< open $i$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          <:str_item< type $list:tdl$ >>
      | "value"; r = OPT "rec"; l = LIST1 let_binding SEP "and" ->
          <:str_item< value $opt:o2b r$ $list:l$ >>
      | e = expr -> <:str_item< $exp:e$ >> ] ]
  ;
  rebind_exn:
    [ [ "="; sl = mod_ident -> sl
      | -> [] ] ]
  ;
  module_binding:
    [ RIGHTA
      [ "("; m = UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
          <:module_expr< functor ( $m$ : $mt$ ) -> $mb$ >>
      | ":"; mt = module_type; "="; me = module_expr ->
          <:module_expr< ( $me$ : $mt$ ) >>
      | "="; me = module_expr -> <:module_expr< $me$ >> ] ]
  ;
  module_type:
    [ [ "functor"; "("; i = UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
          <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ]
    | [ mt = SELF; "with"; wcl = LIST1 with_constr SEP "and" ->
          <:module_type< $mt$ with $list:wcl$ >> ]
    | [ "sig"; sg = LIST0 [ s = sig_item; ";" -> s ]; "end" ->
          <:module_type< sig $list:sg$ end >> ]
    | [ m1 = SELF; m2 = SELF -> <:module_type< $m1$ $m2$ >> ]
    | [ m1 = SELF; "."; m2 = SELF -> <:module_type< $m1$ . $m2$ >> ]
    | "simple"
      [ i = UIDENT -> <:module_type< $uid:i$ >>
      | i = LIDENT -> <:module_type< $lid:i$ >>
      | "'"; i = ident -> <:module_type< ' $i$ >>
      | "("; mt = SELF; ")" -> <:module_type< $mt$ >> ] ]
  ;
  sig_item:
    [ "top"
      [ "declare"; st = LIST0 [ s = sig_item; ";" -> s ]; "end" ->
          <:sig_item< declare $list:st$ end >>
      | "exception"; (_, c, tl) = constructor_declaration ->
          <:sig_item< exception $c$ of $list:tl$ >>
      | "external"; i = LIDENT; ":"; t = ctyp; "="; pd = LIST1 STRING ->
          <:sig_item< external $i$ : $t$ = $list:pd$ >>
      | "include"; mt = module_type -> <:sig_item< include $mt$ >>
      | "module"; i = UIDENT; mt = module_declaration ->
          <:sig_item< module $i$ : $mt$ >>
      | "module"; "rec"; mds = LIST1 module_rec_declaration SEP "and" ->
          <:sig_item< module rec $list:mds$ >>
      | "module"; "type"; i = UIDENT; "="; mt = module_type ->
          <:sig_item< module type $i$ = $mt$ >>
      | "open"; i = mod_ident -> <:sig_item< open $i$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          <:sig_item< type $list:tdl$ >>
      | "value"; i = LIDENT; ":"; t = ctyp ->
          <:sig_item< value $i$ : $t$ >> ] ]
  ;
  module_declaration:
    [ RIGHTA
      [ ":"; mt = module_type -> <:module_type< $mt$ >>
      | "("; i = UIDENT; ":"; t = module_type; ")"; mt = SELF ->
          <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ] ]
  ;
  module_rec_declaration:
    [ [ m = UIDENT; ":"; mt = module_type -> (m, mt) ] ]
  ;
  with_constr:
    [ [ "type"; i = mod_ident; tpl = LIST0 type_parameter; "="; t = ctyp ->
          <:with_constr< type $i$ $list:tpl$ = $t$ >>
      | "module"; i = mod_ident; "="; me = module_expr ->
          <:with_constr< module $i$ = $me$ >> ] ]
  ;
  expr:
    [ "top" RIGHTA
      [ "let"; r = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        x = SELF ->
          <:expr< let $opt:o2b r$ $list:l$ in $x$ >>
      | "let"; "module"; m = UIDENT; mb = module_binding; "in"; e = SELF ->
          <:expr< let module $m$ = $mb$ in $e$ >>
      | "fun"; "["; l = LIST0 match_case SEP "|"; "]" ->
          <:expr< fun [ $list:l$ ] >>
      | "fun"; p = ipatt; e = fun_def -> <:expr< fun $p$ -> $e$ >>
      | "match"; e = SELF; "with"; "["; l = LIST0 match_case SEP "|"; "]" ->
          <:expr< match $e$ with [ $list:l$ ] >>
      | "match"; e = SELF; "with"; p1 = ipatt; "->"; e1 = SELF ->
          <:expr< match $e$ with $p1$ -> $e1$ >>
      | "try"; e = SELF; "with"; "["; l = LIST0 match_case SEP "|"; "]" ->
          <:expr< try $e$ with [ $list:l$ ] >>
      | "try"; e = SELF; "with"; p1 = ipatt; "->"; e1 = SELF ->
          <:expr< try $e$ with $p1$ -> $e1$ >>
      | "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
          <:expr< if $e1$ then $e2$ else $e3$ >>
      | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; seq = sequence; "done" ->
          <:expr< for $i$ = $e1$ $to:df$ $e2$ do { $list:seq$ } >>
      | "while"; e = SELF; "do"; seq = sequence; "done" ->
          <:expr< while $e$ do { $list:seq$ } >> ]
    | "where"
      [ e = SELF; "where"; lb = let_binding ->
          <:expr< let rec $list:[lb]$ in $e$ >> ]
    | ":=" NONA
      [ e1 = SELF; ":="; e2 = SELF; [ -> () ] -> <:expr< $e1$ := $e2$ >> ]
    | "||" RIGHTA
      [ e1 = SELF; "||"; e2 = SELF -> <:expr< $e1$ || $e2$ >> ]
    | "&&" RIGHTA
      [ e1 = SELF; "&&"; e2 = SELF -> <:expr< $e1$ && $e2$ >> ]
    | "<" LEFTA
      [ e1 = SELF; "<"; e2 = SELF -> <:expr< $e1$ < $e2$ >>
      | e1 = SELF; ">"; e2 = SELF -> <:expr< $e1$ > $e2$ >>
      | e1 = SELF; "<="; e2 = SELF -> <:expr< $e1$ <= $e2$ >>
      | e1 = SELF; ">="; e2 = SELF -> <:expr< $e1$ >= $e2$ >>
      | e1 = SELF; "="; e2 = SELF -> <:expr< $e1$ = $e2$ >>
      | e1 = SELF; "<>"; e2 = SELF -> <:expr< $e1$ <> $e2$ >>
      | e1 = SELF; "=="; e2 = SELF -> <:expr< $e1$ == $e2$ >>
      | e1 = SELF; "!="; e2 = SELF -> <:expr< $e1$ != $e2$ >> ]
    | "^" RIGHTA
      [ e1 = SELF; "^"; e2 = SELF -> <:expr< $e1$ ^ $e2$ >>
      | e1 = SELF; "@"; e2 = SELF -> <:expr< $e1$ @ $e2$ >> ]
    | "+" LEFTA
      [ e1 = SELF; "+"; e2 = SELF -> <:expr< $e1$ + $e2$ >>
      | e1 = SELF; "-"; e2 = SELF -> <:expr< $e1$ - $e2$ >>
      | e1 = SELF; "+."; e2 = SELF -> <:expr< $e1$ +. $e2$ >>
      | e1 = SELF; "-."; e2 = SELF -> <:expr< $e1$ -. $e2$ >> ]
    | "*" LEFTA
      [ e1 = SELF; "*"; e2 = SELF -> <:expr< $e1$ * $e2$ >>
      | e1 = SELF; "/"; e2 = SELF -> <:expr< $e1$ / $e2$ >>
      | e1 = SELF; "*."; e2 = SELF -> <:expr< $e1$ *. $e2$ >>
      | e1 = SELF; "/."; e2 = SELF -> <:expr< $e1$ /. $e2$ >>
      | e1 = SELF; "land"; e2 = SELF -> <:expr< $e1$ land $e2$ >>
      | e1 = SELF; "lor"; e2 = SELF -> <:expr< $e1$ lor $e2$ >>
      | e1 = SELF; "lxor"; e2 = SELF -> <:expr< $e1$ lxor $e2$ >>
      | e1 = SELF; "mod"; e2 = SELF -> <:expr< $e1$ mod $e2$ >> ]
    | "**" RIGHTA
      [ e1 = SELF; "**"; e2 = SELF -> <:expr< $e1$ ** $e2$ >>
      | e1 = SELF; "asr"; e2 = SELF -> <:expr< $e1$ asr $e2$ >>
      | e1 = SELF; "lsl"; e2 = SELF -> <:expr< $e1$ lsl $e2$ >>
      | e1 = SELF; "lsr"; e2 = SELF -> <:expr< $e1$ lsr $e2$ >> ]
    | "unary minus" NONA
      [ "-"; e = SELF -> mkumin loc "-" e
      | "-."; e = SELF -> mkumin loc "-." e ]
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF -> <:expr< $e1$ $e2$ >>
      | "assert"; e = SELF -> mkassert loc e
      | "lazy"; e = SELF -> <:expr< lazy ($e$) >> ]
    | "." LEFTA
      [ e1 = SELF; "."; "("; e2 = SELF; ")" -> <:expr< $e1$ .( $e2$ ) >>
      | e1 = SELF; "."; "["; e2 = SELF; "]" -> <:expr< $e1$ .[ $e2$ ] >>
      | e1 = SELF; "."; e2 = SELF -> <:expr< $e1$ . $e2$ >> ]
    | "~-" NONA
      [ "~-"; e = SELF -> <:expr< ~- $e$ >>
      | "~-."; e = SELF -> <:expr< ~-. $e$ >> ]
    | "simple"
      [ s = INT -> <:expr< $int:s$ >>
      | s = FLOAT -> <:expr< $flo:s$ >>
      | s = STRING -> <:expr< $str:s$ >>
      | s = CHAR -> <:expr< $chr:s$ >>
      | i = expr_ident -> i
      | "["; "]" -> <:expr< [] >>
      | "["; el = LIST1 expr SEP ";"; last = cons_expr_opt; "]" ->
          mklistexp loc last el
      | "[|"; el = LIST0 expr SEP ";"; "|]" -> <:expr< [| $list:el$ |] >>
      | "{"; lel = LIST1 label_expr SEP ";"; "}" -> <:expr< { $list:lel$ } >>
      | "{"; "("; e = SELF; ")"; "with"; lel = LIST1 label_expr SEP ";";
        "}" ->
          <:expr< { ($e$) with $list:lel$ } >>
      | "("; ")" -> <:expr< () >>
      | "("; lloc = [ "let" -> loc ]; rf = OPT "rec";
        l = LIST1 let_binding SEP "and"; "in"; el = sequence; ")" ->
          let loc = merge_couple lloc loc in
          <:expr< let $opt:o2b rf$ $list:l$ in $mksequence loc el$ >>
      | "("; e = SELF; ";"; el = sequence; ")" -> mksequence loc [e :: el]
      | "("; e = SELF; ":"; t = ctyp; ")" -> <:expr< ($e$ : $t$) >>
      | "("; e = SELF; ","; el = LIST1 expr SEP ","; ")" ->
          <:expr< ( $list:[e::el]$) >>
      | "("; e = SELF; ")" -> <:expr< $e$ >> ] ]
  ;
  cons_expr_opt:
    [ [ "::"; e = expr -> Some e
      | -> None ] ]
  ;
  sequence:
    [ [ "let"; rf = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        el = SELF ->
          [<:expr< let $opt:o2b rf$ $list:l$ in $mksequence loc el$ >>]
      | e = expr; ";"; el = SELF -> [e :: el]
      | e = expr -> [e]
      | -> [] ] ]
  ;
  let_binding:
    [ [ p = ipatt; e = fun_binding -> (p, e) ] ]
  ;
  fun_binding:
    [ RIGHTA
      [ p = ipatt; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "="; e = expr -> <:expr< $e$ >>
      | ":"; t = ctyp; "="; e = expr -> <:expr< ($e$ : $t$) >>
      | ":>"; t = ctyp; "="; e = expr -> <:expr< ($e$ :> $t$) >> ] ]
  ;
  match_case:
    [ [ p = patt_as; w = when_expr_opt; "->"; e = expr -> (p, w, e) ] ]
  ;
  patt_as:
    [ [ p = patt; "as"; p2 = patt -> <:patt< ($p$ as $p2$) >>
      | p = patt -> p ] ]
  ;
  when_expr_opt:
    [ [ "when"; e = expr -> Some e
      | -> None ] ]
  ;
  label_expr:
    [ [ i = patt_label_ident; e = fun_binding -> (i, e) ] ]
  ;
  expr_ident:
    [ RIGHTA
      [ i = LIDENT -> <:expr< $lid:i$ >>
      | i = UIDENT -> <:expr< $uid:i$ >>
      | i = UIDENT; "."; j = SELF -> mkexprident loc i j ] ]
  ;
  fun_def:
    [ RIGHTA
      [ p = ipatt; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "->"; e = expr -> e ] ]
  ;
  patt:
    [ LEFTA
      [ p1 = SELF; "|"; p2 = SELF -> <:patt< $p1$ | $p2$ >> ]
    | NONA
      [ p1 = SELF; ".."; p2 = SELF -> <:patt< $p1$ .. $p2$ >> ]
    | LEFTA
      [ p1 = SELF; p2 = SELF -> <:patt< $p1$ $p2$ >> ]
    | LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | "simple"
      [ s = LIDENT -> <:patt< $lid:s$ >>
      | s = UIDENT -> <:patt< $uid:s$ >>
      | s = INT -> <:patt< $int:s$ >>
      | s = FLOAT -> <:patt< $flo:s$ >>
      | s = STRING -> <:patt< $str:s$ >>
      | s = CHAR -> <:patt< $chr:s$ >>
      | "-"; s = INT -> mkuminpat loc <:patt< $int:s$ >>
      | "-"; s = FLOAT -> mkuminpat loc <:patt< $flo:s$ >>
      | "["; "]" -> <:patt< [] >>
      | "["; pl = LIST1 patt SEP ";"; last = cons_patt_opt; "]" ->
          mklistpat loc last pl
      | "[|"; pl = LIST0 patt SEP ";"; "|]" -> <:patt< [| $list:pl$ |] >>
      | "{"; lpl = LIST1 label_patt SEP ";"; "}" -> <:patt< { $list:lpl$ } >>
      | "("; ")" -> <:patt< () >>
      | "("; p = SELF; ")" -> <:patt< $p$ >>
      | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
      | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p$ as $p2$) >>
      | "("; p = SELF; ","; pl = LIST1 patt SEP ","; ")" ->
          <:patt< ( $list:[p::pl]$) >>
      | "_" -> <:patt< _ >> ] ]
  ;
  cons_patt_opt:
    [ [ "::"; p = patt -> Some p
      | -> None ] ]
  ;
  label_patt:
    [ [ i = patt_label_ident; "="; p = patt -> (i, p) ] ]
  ;
  patt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | "simple" RIGHTA
      [ i = UIDENT -> <:patt< $uid:i$ >>
      | i = LIDENT -> <:patt< $lid:i$ >> ] ]
  ;
  ipatt:
    [ [ "{"; lpl = LIST1 label_ipatt SEP ";"; "}" ->
          <:patt< { $list:lpl$ } >>
      | "("; ")" -> <:patt< () >>
      | "("; p = SELF; ")" -> <:patt< $p$ >>
      | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
      | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p$ as $p2$) >>
      | "("; p = SELF; ","; pl = LIST1 ipatt SEP ","; ")" ->
          <:patt< ( $list:[p::pl]$) >>
      | s = LIDENT -> <:patt< $lid:s$ >>
      | "_" -> <:patt< _ >> ] ]
  ;
  label_ipatt:
    [ [ i = patt_label_ident; "="; p = ipatt -> (i, p) ] ]
  ;
  type_declaration:
    [ [ n = type_patt; tpl = LIST0 type_parameter; "="; tk = ctyp;
        cl = LIST0 constrain ->
          IFDEF CAMLP4S_4_02 THEN
            {MLast.tdNam = n; MLast.tdPrm = tpl; MLast.tdPrv = False;
             MLast.tdDef = tk; MLast.tdCon = cl}
          ELSE (n, tpl, tk, cl) END ] ]
  ;
  type_patt:
    [ [ n = LIDENT -> (loc, n) ] ]
  ;
  constrain:
    [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> (t1, t2) ] ]
  ;
  type_parameter:
    [ [ "'"; i = ident -> (i, (False, False))
      | "+"; "'"; i = ident -> (i, (True, False))
      | "-"; "'"; i = ident -> (i, (False, True)) ] ]
  ;
  ctyp:
    [ LEFTA
      [ t1 = SELF; "=="; t2 = SELF -> <:ctyp< $t1$ == $t2$ >> ]
    | "alias" LEFTA
      [ t1 = SELF; "as"; t2 = SELF -> <:ctyp< $t1$ as $t2$ >> ]
    | "arrow" RIGHTA
      [ t1 = SELF; "->"; t2 = SELF -> <:ctyp< $t1$ -> $t2$ >> ]
    | LEFTA
      [ t1 = SELF; t2 = SELF -> <:ctyp< $t1$ $t2$ >> ]
    | LEFTA
      [ t1 = SELF; "."; t2 = SELF -> <:ctyp< $t1$ . $t2$ >> ]
    | "simple"
      [ "'"; i = ident -> <:ctyp< '$i$ >>
      | "_" -> <:ctyp< _ >>
      | i = LIDENT -> <:ctyp< $lid:i$ >>
      | i = UIDENT -> <:ctyp< $uid:i$ >>
      | "("; t = SELF; "*"; tl = LIST1 ctyp SEP "*"; ")" ->
          <:ctyp< ( $list:[t::tl]$ ) >>
      | "("; t = SELF; ")" -> <:ctyp< $t$ >>
      | "["; cdl = LIST0 constructor_declaration SEP "|"; "]" ->
          <:ctyp< [ $list:cdl$ ] >>
      | "{"; ldl = LIST1 label_declaration SEP ";"; "}" ->
          <:ctyp< { $list:ldl$ } >> ] ]
  ;
  constructor_declaration:
    [ [ ci = UIDENT; "of"; cal = LIST1 ctyp SEP "and" -> (loc, ci, cal)
      | ci = UIDENT -> (loc, ci, []) ] ]
  ;
  label_declaration:
    [ [ i = LIDENT; ":"; mf = OPT "mutable"; t = ctyp ->
          (loc, i, o2b mf, t) ] ]
  ;
  ident:
    [ [ i = LIDENT -> i
      | i = UIDENT -> i ] ]
  ;
  mod_ident:
    [ RIGHTA
      [ i = UIDENT -> [i]
      | i = LIDENT -> [i]
      | i = UIDENT; "."; j = SELF -> [i :: j] ] ]
  ;
  direction_flag:
    [ [ "to" -> True
      | "downto" -> False ] ]
  ;
END;

EXTEND
  GLOBAL: interf implem use_file top_phrase expr patt;
  interf:
    [ [ "#"; n = LIDENT; dp = OPT expr; ";" ->
          ([(<:sig_item< # $n$ $opt:dp$ >>, loc)], True)
      | si = sig_item_semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | EOI -> ([], False) ] ]
  ;
  sig_item_semi:
    [ [ si = sig_item; ";" -> (si, loc) ] ]
  ;
  implem:
    [ [ "#"; n = LIDENT; dp = OPT expr; ";" ->
          ([(<:str_item< # $n$ $opt:dp$ >>, loc)], True)
      | si = str_item_semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | EOI -> ([], False) ] ]
  ;
  str_item_semi:
    [ [ si = str_item; ";" -> (si, loc) ] ]
  ;
  top_phrase:
    [ [ ph = phrase -> Some ph
      | EOI -> None ] ]
  ;
  use_file:
    [ [ "#"; n = LIDENT; dp = OPT expr; ";" ->
          ([<:str_item< # $n$ $opt:dp$ >>], True)
      | si = str_item; ";"; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | EOI -> ([], False) ] ]
  ;
  phrase:
    [ [ "#"; n = LIDENT; dp = OPT expr; ";" -> <:str_item< # $n$ $opt:dp$ >>
      | sti = str_item; ";" -> sti ] ]
  ;
  expr: LEVEL "simple"
    [ [ x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_expr_quotation loc x ] ]
  ;
  patt: LEVEL "simple"
    [ [ x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_patt_quotation loc x ] ]
  ;
END;
