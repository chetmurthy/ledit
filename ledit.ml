(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*       Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Sys;

value max_len = ref 70;
value set_max_len x =
  max_len.val := if x > 3 then x else failwith "set_max_len"
;

value son = ref None;
value set_son pid = son.val := Some pid;

type command =
  [ Abort
  | Accept_line
  | Backward_char
  | Backward_delete_char
  | Backward_kill_word
  | Backward_word
  | Beginning_of_history
  | Beginning_of_line
  | Capitalize_word
  | Delete_char
  | Downcase_word
  | End_of_history
  | End_of_line
  | Expand_abbrev
  | Forward_char
  | Forward_word
  | Interrupt
  | Kill_line
  | Kill_word
  | Next_history
  | Operate_and_get_next
  | Previous_history
  | Quit
  | Quoted_insert
  | Refresh_line
  | Reverse_search_history
  | Self_insert
  | Start_csi_sequence
  | Start_escape_sequence
  | Suspend
  | Transpose_chars
  | Unix_line_discard
  | Upcase_word
  | Yank ]
;

type istate = [ Normal | Quote | Escape | CSI ];

value (command_of_char, set_char_command) =
  let command_vect = Array.create 256 Self_insert in
  (fun c -> command_vect.(Char.code c),
   fun c comm -> command_vect.(Char.code c) := comm)
;

value (escape_command_of_char, set_escape_command) =
  let command_vect = Array.create 256 Abort in
  (fun c -> command_vect.(Char.code c),
   fun c comm -> command_vect.(Char.code c) := comm)
;

value (csi_command_of_char, set_csi_command) =
  let command_vect = Array.create 256 Abort in
  (fun c -> command_vect.(Char.code c),
   fun c comm -> command_vect.(Char.code c) := comm)
;

do set_char_command '\001' (* ^a *)    Beginning_of_line;
   set_char_command '\005' (* ^e *)    End_of_line;
   set_char_command '\006' (* ^f *)    Forward_char;
   set_char_command '\002' (* ^b *)    Backward_char;
   set_char_command '\230' (* M-f *)   Forward_word;
   set_char_command '\226' (* M-b *)   Backward_word;
   set_char_command '\016' (* ^p *)    Previous_history;
   set_char_command '\014' (* ^n *)    Next_history;
   set_char_command '\188' (* M-< *)   Beginning_of_history;
   set_char_command '\190' (* M-> *)   End_of_history;
   set_char_command '\018' (* ^r *)    Reverse_search_history;
   set_char_command '\004' (* ^d *)    Delete_char;
   set_char_command '\008' (* ^h *)    Backward_delete_char;
   set_char_command '\127' (* del *)   Backward_delete_char;
   set_char_command '\020' (* ^t *)    Transpose_chars;
   set_char_command '\227' (* M-c *)   Capitalize_word;
   set_char_command '\245' (* M-u *)   Upcase_word;
   set_char_command '\236' (* M-l *)   Downcase_word;
   set_char_command '\228' (* M-d *)   Kill_word;
   set_char_command '\136' (* M-^h *)  Backward_kill_word;
   set_char_command '\255' (* M-del *) Backward_kill_word;
   set_char_command '\017' (* ^q *)    Quoted_insert;
   set_char_command '\011' (* ^k *)    Kill_line;
   set_char_command '\025' (* ^y *)    Yank;
   set_char_command '\021' (* ^u *)    Unix_line_discard;
   set_char_command '\012' (* ^l *)    Refresh_line;
   set_char_command '\007' (* ^g *)    Abort;
   set_char_command '\003' (* ^c *)    Interrupt;
   set_char_command '\026' (* ^z *)    Suspend;
   set_char_command '\028' (* ^\ *)    Quit;
   set_char_command '\n'               Accept_line;
   set_char_command '\024' (* ^x *)    Operate_and_get_next;
   set_char_command '\027' (* ^x *)    Start_escape_sequence;
   set_char_command '\175' (* M-/ *)   Expand_abbrev;
   set_escape_command 'f'              Forward_word;
   set_escape_command 'b'              Backward_word;
   set_escape_command 'c'              Capitalize_word;
   set_escape_command 'u'              Upcase_word;
   set_escape_command 'l'              Downcase_word;
   set_escape_command '<'              Beginning_of_history;
   set_escape_command '>'              End_of_history;
   set_escape_command 'd'              Kill_word;
   set_escape_command '\008' (* ^h *)  Backward_kill_word;
   set_escape_command '\127' (* del *) Backward_kill_word;
   set_escape_command '['              Start_csi_sequence;
   set_escape_command '/'              Expand_abbrev;
   set_csi_command 'A' Previous_history;
   set_csi_command 'B' Next_history;
   set_csi_command 'C' Forward_char;
   set_csi_command 'D' Backward_char;
return ()
;

type line = { buf : mutable string; cur : mutable int; len : mutable int };
type abbrev_data =
  { hist : list string;
    rpos : int;
    clen : int;
    abbr : string;
    found : list string }
;

type state =
  { od : line;
    nd : line;
    line : line;
    iso_8859_1 : bool;
    istate : mutable istate;
    shift : mutable int;
    cut : mutable string;
    last_comm : mutable command;
    histfile : mutable option out_channel;
    history : mutable Cursor.t string;
    abbrev : mutable option abbrev_data }
;

value bs = '\b';

value put_char st c = output_char stderr c;
value put_newline st = prerr_endline "";
value flush_out st = flush stderr;
value bell () = do prerr_string "\007"; flush stderr; return ();

open Unix;

value saved_tcio = tcgetattr stdin;
value edit_tcio = ref None;

value set_edit () =
  let tcio =
    match edit_tcio.val with
    [ Some e -> e
    | None ->
        let tcio = tcgetattr stdin in
        do tcio.c_echo := False;
           tcio.c_icanon := False;
           tcio.c_vmin := 1;
           tcio.c_isig := False;
           tcio.c_ixon := False;
           edit_tcio.val := Some tcio;
        return tcio ]
  in
  tcsetattr stdin TCSADRAIN tcio
and unset_edit () = tcsetattr stdin TCSADRAIN saved_tcio;

value line_set_nth_char line i c =
  if i == String.length line.buf then line.buf := line.buf ^ String.make 1 c
  else line.buf.[i] := c
;

value line_to_nd st =
  let rec line_rec i =
    do if i == st.line.cur then st.nd.cur := st.nd.len else (); return
    if i < st.line.len then
      let c = st.line.buf.[i] in
      let ic = Char.code c in
      do if c = '\t' then
           for i = st.nd.len + 1 to (st.nd.len + 8) / 8 * 8 do
             line_set_nth_char st.nd st.nd.len ' ';
             st.nd.len := st.nd.len + 1;
           done
         else if ic < 32 || ic == 127 then
           do line_set_nth_char st.nd st.nd.len '^';
              line_set_nth_char st.nd (st.nd.len + 1)
                (Char.chr (127 land (ic + 64)));
              st.nd.len := st.nd.len + 2;
           return ()
         else if ic >= 128 && not st.iso_8859_1 then
           do line_set_nth_char st.nd st.nd.len '\\';
              line_set_nth_char st.nd (st.nd.len + 1)
                (Char.chr (ic / 100 + Char.code '0'));
              line_set_nth_char st.nd (st.nd.len + 2)
                (Char.chr (ic mod 100 / 10 + Char.code '0'));
              line_set_nth_char st.nd (st.nd.len + 3)
                (Char.chr (ic mod 10 + Char.code '0'));
              st.nd.len := st.nd.len + 4;
           return ()
         else if ic >= 128 && ic < 160 then
           do line_set_nth_char st.nd st.nd.len 'M';
              line_set_nth_char st.nd (st.nd.len + 1) '-';
              line_set_nth_char st.nd (st.nd.len + 2) '^';
              line_set_nth_char st.nd (st.nd.len + 3)
                (Char.chr (127 land (ic + 64)));
              st.nd.len := st.nd.len + 4;
           return ()
         else
           do line_set_nth_char st.nd st.nd.len c; st.nd.len := st.nd.len + 1;
           return ();
         line_rec (i + 1);
      return ()
    else if st.nd.len > max_len.val then
      let shift =
        if st.nd.cur - st.shift >= 0 &&
           st.nd.cur - st.shift < max_len.val - 2 then
          st.shift
        else if st.nd.cur < max_len.val - 3 then 0
        else st.nd.cur - max_len.val / 2
      in
      do for i = 0 to max_len.val - 3 do
           let ni = i + shift in
           st.nd.buf.[i] := if ni < st.nd.len then st.nd.buf.[ni] else ' ';
         done;
         st.nd.buf.[max_len.val - 2] := ' ';
         st.nd.buf.[max_len.val - 1] :=
           if shift = 0 then '>'
           else if st.nd.len - shift < max_len.val - 2 then '<'
           else '*';
         st.nd.cur := st.nd.cur - shift;
         st.nd.len := max_len.val;
         st.shift := shift;
      return ()
    else st.shift := 0
  in
  do st.nd.len := 0; line_rec 0; return ()
;

value display st =
  disp_rec 0 where rec disp_rec i =
    if i < st.nd.len then
      do if i >= st.od.len || st.od.buf.[i] <> st.nd.buf.[i] then
           do while i < st.od.cur do
                st.od.cur := st.od.cur - 1; put_char st bs;
              done;
              while st.od.cur < i do
                let c = st.nd.buf.[st.od.cur] in
                do st.od.cur := st.od.cur + 1; put_char st c; return ();
              done;
           return
           let c = st.nd.buf.[i] in
           do line_set_nth_char st.od i c;
              st.od.cur := st.od.cur + 1;
              put_char st c;
           return ()
         else ();
         disp_rec (i + 1);
      return ()
    else
      do if st.od.len > st.nd.len then
           do while st.od.cur < st.od.len do
                let c =
                  if st.od.cur < st.nd.len then st.nd.buf.[st.od.cur] else ' '
                in
                do put_char st c; st.od.cur := st.od.cur + 1; return ();
              done;
              while st.od.cur > st.nd.len do
                put_char st bs;
                put_char st ' ';
                put_char st bs;
                st.od.cur := st.od.cur - 1;
              done;
           return ()
         else ();
         st.od.len := st.nd.len;
         while st.od.cur < st.nd.cur do
           put_char st st.nd.buf.[st.od.cur]; st.od.cur := st.od.cur + 1;
         done;
         while st.od.cur > st.nd.cur do
           put_char st bs; st.od.cur := st.od.cur - 1;
         done;
         flush_out st;
      return ()
;

value update_output st = do line_to_nd st; display st; return ();

value balance_paren st =
  fun
  [ (')' | ']' | '}' as c) ->
      let i =
        find_lparen c (st.line.cur - 2) where rec find_lparen r i =
          if i < 0 then i
          else
            match st.line.buf.[i] with
            [ (')' | ']' | '}' as c) ->
                find_lparen r (find_lparen c (i - 1) - 1)
            | '(' -> if r == ')' then i else -1
            | '[' -> if r == ']' then i else -1
            | '{' -> if r == '}' then i else -1
            | '"' ->
                let rec skip_string i =
                  if i < 0 then i
                  else if st.line.buf.[i] == '"' then i - 1
                  else skip_string (i - 1)
                in
                find_lparen r (skip_string (i - 1))
            | _ -> find_lparen r (i - 1) ]
      in
      if i >= 0 then
        let c = st.line.cur in
        do st.line.cur := i;
           update_output st;
           st.line.cur := c;
           let _ = Unix.select [Unix.stdin] [] [] 1.0 in ();
        return ()
      else ()
  | _ -> () ]
;

value delete_char st =
  do st.line.len := st.line.len - 1;
     for i = st.line.cur to st.line.len - 1 do
       st.line.buf.[i] := st.line.buf.[i + 1];
     done;
  return ()
;

value insert_char st x =
  do for i = st.line.len downto st.line.cur + 1 do
       line_set_nth_char st.line i st.line.buf.[i - 1];
     done;
     st.line.len := st.line.len + 1;
     line_set_nth_char st.line st.line.cur x;
  return ()
;

value move_in_word buf e f g =
  move_rec where rec move_rec i =
    if e i then i
    else
      match buf.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> f move_rec i
      | x -> if Char.code x >= 160 then f move_rec i else g move_rec i ]
;

value forward_move line = move_in_word line.buf (fun i -> i == line.len);
value backward_move line = move_in_word line.buf (fun i -> i == -1);

value forward_word line =
  let i = line.cur in
  let i = forward_move line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move line (fun mv i -> mv (i + 1)) (fun _ i -> i) i
;

value backward_word line =
  let i = line.cur - 1 in
  let i = backward_move line (fun _ i -> i) (fun mv i -> mv (i - 1)) i in
  backward_move line (fun mv i -> mv (i - 1)) (fun _ i -> i) i + 1
;

value get_word_len st =
  let i = st.line.cur - 1 in
  i - backward_move st.line (fun mv i -> mv (i - 1)) (fun _ i -> i) i
;

value kill_word st =
  let i = st.line.cur in
  let i =
    forward_move st.line (fun _ i -> i)
      (fun mv i -> do delete_char st; return mv i) i
  in
  forward_move st.line (fun mv i -> do delete_char st; return mv i)
    (fun _ i -> i) i
;

value backward_kill_word st =
  let k = backward_word st.line in
  let sh = st.line.cur - k in
  do st.line.len := st.line.len - sh;
     for i = k to st.line.len - 1 do
       st.line.buf.[i] := st.line.buf.[i + sh];
     done;
  return k
;

value capitalize_word st =
  let i = st.line.cur in
  let i0 = forward_move st.line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move st.line
    (fun mv i ->
       let f = if i == i0 then Char.uppercase else Char.lowercase in
       do st.line.buf.[i] := f st.line.buf.[i]; return mv (i + 1))
    (fun _ i -> i) i0
;

value upcase_word st =
  let i = st.line.cur in
  let i = forward_move st.line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move st.line
    (fun mv i ->
       let f = Char.uppercase in
       do st.line.buf.[i] := f st.line.buf.[i]; return mv (i + 1))
    (fun _ i -> i) i
;

value downcase_word st =
  let i = st.line.cur in
  let i = forward_move st.line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move st.line
    (fun mv i ->
       let f = Char.lowercase in
       do st.line.buf.[i] := f st.line.buf.[i]; return mv (i + 1))
    (fun _ i -> i) i
;

value set_line st str =
  do st.line.len := 0;
     st.line.cur := 0;
     for i = 0 to String.length str - 1 do
       insert_char st str.[i]; st.line.cur := st.line.len;
     done;
  return ()
;

value previous_history st =
  try
    do Cursor.before st.history; set_line st (Cursor.peek st.history); return
    ()
  with
  [ Cursor.Failure -> bell () ]
;

value next_history st =
  try
    do Cursor.after st.history; set_line st (Cursor.peek st.history); return
    ()
  with
  [ Cursor.Failure -> bell () ]
;

value read_char =
  let buff = " " in
  fun () ->
    let len = Unix.read Unix.stdin buff 0 1 in
    if len == 0 then raise End_of_file
    else buff.[0]
;

value reverse_search_history st =
  let question str = "(reverse-i-search)'" ^ str ^ "': " in
  let make_line str fstr =
    do st.line.cur := 0; st.line.len := 0; return
    let len = String.length str in
    do for i = 0 to len - 1 do
         insert_char st str.[i]; st.line.cur := st.line.cur + 1;
       done;
    return
    let len = String.length fstr in
    for i = 0 to len - 1 do
      insert_char st fstr.[i]; st.line.cur := st.line.cur + 1;
    done
  in
  let initial_str = String.sub st.line.buf 0 st.line.len in
  let rec find_line (cnt, fstr) str =
    find_rec 0 0 where rec find_rec ifstr istr =
      if istr == String.length str then (cnt, fstr)
      else if ifstr == String.length fstr then
        if try do Cursor.before st.history; return True with
           [ Cursor.Failure -> False ] then
          find_line (cnt + 1, Cursor.peek st.history) str
        else do bell (); return (cnt, fstr)
      else if str.[istr] != fstr.[ifstr] then find_rec (ifstr + 1) 0
      else find_rec (ifstr + 1) (istr + 1)
  in
  let rec incr_search (cnt, fstr) str =
    let q = question str in
    do make_line q fstr; st.line.cur := String.length q - 3; update_output st;
    return
    let c = read_char () in
    match command_of_char c with
    [ Start_escape_sequence -> fstr
    | Self_insert ->
        let str = str ^ String.make 1 c in
        incr_search (find_line (cnt, fstr) str) str
    | Backward_delete_char ->
        if String.length str == 0 then incr_search (cnt, fstr) str
        else
          let str = String.sub str 0 (String.length str - 1) in
          do for i = 1 to cnt do Cursor.after st.history; done; return
          incr_search (find_line (0, initial_str) str) str
    | Abort ->
        do for i = 1 to cnt do Cursor.after st.history; done; bell (); return
        initial_str
    | Reverse_search_history ->
        let (cnt, fstr) =
          try
            do Cursor.before st.history; return
            find_line (cnt + 1, Cursor.peek st.history) str
          with
          [ Cursor.Failure -> do bell (); return (cnt, initial_str) ]
        in
        incr_search (cnt, fstr) str
    | _ -> fstr ]
  in
  let fstr = incr_search (0, initial_str) "" in make_line "" fstr
;

value rec beginning_of_history st =
  do Cursor.goto_first st.history; return
  try set_line st (Cursor.peek st.history) with [ Cursor.Failure -> bell () ]
;

value rec end_of_history st =
  do Cursor.goto_last st.history; return
  try set_line st (Cursor.peek st.history) with [ Cursor.Failure -> bell () ]
;

value rec back_search st ad hist rpos =
  match hist with
  [ [] ->
      do for i = 0 to String.length ad.abbr - 1 do
           insert_char st ad.abbr.[i]; st.line.cur := st.line.cur + 1;
         done;
      return bell ()
  | [l :: ll] ->
      let i = String.length l - rpos in
      if i <= 0 then back_search st ad ll 0
      else
        let i = backward_word {buf = l; cur = i; len = String.length l} in
        if String.length l - i < String.length ad.abbr then
          back_search st ad [l :: ll] (String.length l - i)
        else if String.sub l i (String.length ad.abbr) = ad.abbr then
          let i1 = forward_word {buf = l; cur = i; len = String.length l} in
          let f = String.sub l i (i1 - i) in
          if List.mem f ad.found then
            back_search st ad [l :: ll] (String.length l - i)
          else
            let ad =
              {hist = [l :: ll]; rpos = String.length l - i1; clen = i1 - i;
               abbr = ad.abbr; found = [f :: ad.found]}
            in
            do st.abbrev := Some ad;
               for i = 0 to String.length f - 1 do
                 insert_char st f.[i]; st.line.cur := st.line.cur + 1;
               done;
            return ()
        else back_search st ad [l :: ll] (String.length l - i) ]
;

value expand_abbrev st abbrev =
  let ad =
    match abbrev with
    [ Some x -> x
    | None ->
        let len = get_word_len st in
        let abbr = String.sub st.line.buf (st.line.cur - len) len in
        let line_beg = String.sub st.line.buf 0 (st.line.cur - len) in
        let line_end =
          String.sub st.line.buf st.line.cur
            (st.line.len - st.line.cur)
        in
        {hist = [line_beg :: Cursor.get_all st.history @ [line_end]];
         rpos = 0; clen = len; abbr = abbr; found = [abbr]} ]
  in
  do for i = 1 to ad.clen do
       st.line.cur := st.line.cur -  1;
       delete_char st;
     done;
     back_search st ad ad.hist ad.rpos;
     update_output st;
  return ()
;

value rec update_line st comm c =
  let abbrev = st.abbrev in
  do st.abbrev := None; return
  match comm with
  [ Beginning_of_line ->
      if st.line.cur > 0 then do st.line.cur := 0; update_output st; return ()
      else ()
  | End_of_line ->
      if st.line.cur < st.line.len then
        do st.line.cur := st.line.len; update_output st; return ()
      else ()
  | Forward_char ->
      if st.line.cur < st.line.len then
        do st.line.cur := st.line.cur + 1; update_output st; return ()
      else ()
  | Backward_char ->
      if st.line.cur > 0 then
        do st.line.cur := st.line.cur - 1; update_output st; return ()
      else ()
  | Forward_word ->
      if st.line.cur < st.line.len then
        do st.line.cur := forward_word st.line; update_output st; return ()
      else ()
  | Backward_word ->
      if st.line.cur > 0 then
        do st.line.cur := backward_word st.line; update_output st; return ()
      else ()
  | Capitalize_word ->
      if st.line.cur < st.line.len then
        do st.line.cur := capitalize_word st; update_output st; return ()
      else ()
  | Upcase_word ->
      if st.line.cur < st.line.len then
        do st.line.cur := upcase_word st; update_output st; return ()
      else ()
  | Downcase_word ->
      if st.line.cur < st.line.len then
        do st.line.cur := downcase_word st; update_output st; return ()
      else ()
  | Previous_history -> do previous_history st; update_output st; return ()
  | Next_history -> do next_history st; update_output st; return ()
  | Beginning_of_history ->
      do beginning_of_history st; update_output st; return ()
  | End_of_history -> do end_of_history st; update_output st; return ()
  | Reverse_search_history ->
      do reverse_search_history st; update_output st; return ()
  | Delete_char ->
      do if st.line.len = 0 then raise End_of_file else (); return
      if st.line.cur < st.line.len then
        do delete_char st; update_output st; return ()
      else ()
  | Backward_delete_char ->
      if st.line.cur > 0 then
        do st.line.cur := st.line.cur - 1; delete_char st; update_output st;
        return ()
      else ()
  | Transpose_chars ->
      if st.line.cur > 1 then
        let c = st.line.buf.[st.line.cur-1] in
        do st.line.buf.[st.line.cur-1] := st.line.buf.[st.line.cur-2];
           st.line.buf.[st.line.cur-2] := c;
           update_output st;
        return ()
      else ()
  | Kill_word ->
      if st.line.cur < st.line.len then
        do st.line.cur := kill_word st; update_output st; return ()
      else ()
  | Backward_kill_word ->
      if st.line.cur > 0 then
        do st.line.cur := backward_kill_word st; update_output st; return ()
      else ()
  | Quoted_insert -> st.istate := Quote
  | Start_escape_sequence -> st.istate := Escape
  | Start_csi_sequence -> st.istate := CSI
  | Self_insert ->
      do insert_char st c;
         st.line.cur := st.line.cur + 1;
         balance_paren st c;
         update_output st;
      return ()
  | Expand_abbrev -> expand_abbrev st abbrev
  | Refresh_line ->
      do put_newline st;
         st.od.cur := 0;
         st.od.len := 0;
         update_output st;
      return ()
  | Kill_line ->
      do st.cut :=
           String.sub st.line.buf st.line.cur (st.line.len - st.line.cur);
      return
      if st.line.len > st.line.cur then
        do st.line.len := st.line.cur; update_output st; return ()
      else ()
  | Unix_line_discard ->
      if st.line.cur > 0 then
        do st.line.cur := 0; st.line.len := 0; update_output st; return ()
      else ()
  | Yank ->
      if String.length st.cut > 0 then
        do for i = 0 to String.length st.cut - 1 do
             insert_char st st.cut.[i]; st.line.cur := st.line.cur + 1;
           done;
           update_output st;
        return ()
      else ()
  | Abort -> bell ()
  | Interrupt ->
      do if st.line.cur > 0 then
           do st.line.cur := 0; st.line.len := 0; update_output st; return ()
         else ();
         match son.val with
         [ Some pid -> Unix.kill pid Sys.sigint
         | _ -> () ];
      return ()
  | Suspend ->
      do unset_edit ();
         Unix.kill (Unix.getpid ()) Sys.sigtstp;
         set_edit ();
         st.od.cur := 0;
         st.od.len := 0;
         update_output st;
      return ()
  | Quit ->
      match son.val with
      [ Some pid -> Unix.kill pid Sys.sigquit
      | _ -> () ]
  | _ -> () ]
;

value save_history st line =
  let last_line =
    try Cursor.peek_last st.history with [ Cursor.Failure -> "" ]
  in
  if line <> last_line && line <> "" then
    do Cursor.insert_last st.history line; return
    match st.histfile with
    [ Some fdo ->
        do output_string fdo line; output_char fdo '\n'; flush fdo; return ()
    | None -> () ]
  else ()
;

local st =
  {od = {buf = ""; cur = 0; len = 0}; nd = {buf = ""; cur = 0; len = 0};
   line = {buf = ""; cur = 0; len = 0};
   iso_8859_1 =
     try Sys.getenv "LC_CTYPE" <> "" with
     [ Not_found -> False ];
   istate = Normal; shift = 0; cut = ""; last_comm = Accept_line;
   histfile = None; history = Cursor.create (); abbrev = None}
in
value edit_line () =
  let rec edit_loop () =
    let c = read_char () in
    let comm =
      match st.istate with
      [ Quote -> Self_insert
      | Normal -> command_of_char c
      | Escape -> escape_command_of_char c
      | CSI -> csi_command_of_char c ]
    in
    do st.istate := Normal; st.last_comm := comm; return
    match comm with
    [ Accept_line | Operate_and_get_next ->
        let v_max_len = max_len.val in
        do max_len.val := 10000;
           update_output st;
           max_len.val := v_max_len;
           put_newline st;
        return
        let line = String.sub st.line.buf 0 st.line.len in
        do st.abbrev := None; save_history st line; return line
    | _ ->
        do update_line st comm c; return edit_loop () ]
  in
  do st.od.len := 0;
     st.od.cur := 0;
     st.line.len := 0; st.line.cur := 0;
     if st.last_comm == Operate_and_get_next then
       try
         do Cursor.after st.history;
            set_line st (Cursor.peek st.history);
            update_output st;
         return ()
       with
       [ Cursor.Failure -> () ]
     else Cursor.goto_last st.history;
  return edit_loop ()
and open_histfile trunc file =
  do if not trunc then
       match try Some (open_in file) with _ -> None with
       [ Some fi ->
           do try
                while True do
                  Cursor.insert st.history (input_line fi);
                done
              with
              [ End_of_file -> () ];
              close_in fi;
           return ()
       | _ -> () ]
     else ();
  return
  let fd =
    openfile file ([O_WRONLY; O_CREAT] @ (if trunc then [O_TRUNC] else []))
      0o666
  in
  let fdo = out_channel_of_descr fd in
  do if not trunc then seek_out fdo (out_channel_length fdo) else ();
     st.histfile := Some fdo;
  return ()
and close_histfile () =
  match st.histfile with
  [ Some fdo -> close_out fdo
  | None -> () ]
;

value (set_prompt, get_prompt, input_char) =
  let prompt = ref ""
  and buff = ref ""
  and ind = ref 1 in
  let set_prompt x = prompt.val := x
  and get_prompt () = prompt.val
  and input_char ic =
    if ic != Pervasives.stdin then input_char ic
    else
      do if ind.val > String.length buff.val then
           do prerr_string prompt.val;
              flush Pervasives.stderr;
              try
                do set_edit ();
                   buff.val := edit_line ();
                   unset_edit ();
                return ()
              with e -> do unset_edit (); return raise e;
              ind.val := 0;
           return ()
         else ();
      return
      let c =
        if ind.val == String.length buff.val then '\n' else buff.val.[ind.val]
      in
      do ind.val := ind.val + 1; return c
  in
  (set_prompt, get_prompt, input_char)
;
