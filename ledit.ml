(***********************************************************************)
(*                                                                     *)
(*                               Ledit                                 *)
(*                                                                     *)
(*                Daniel de Rauglaudre, INRIA Rocquencourt             *)
(*                                                                     *)
(*  Copyright 2001-2007 Institut National de Recherche en Informatique *)
(*  et Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

#load "pa_local.cmo";
#load "pa_def.cmo";

type encoding = [ Ascii | Iso_8859 | Utf_8 ];

module A :
  sig
    value encoding : ref encoding;
    module Char :
      sig
        type t = 'abstract;
        value of_ascii : char -> t;
        value to_ascii : t -> option char;
        value is_word_char : t -> bool;
        value ctrl_val : t -> option t;
        value meta_ctrl_val : t -> option t;
        value not_ascii_val : t -> option (t * t * t);
        value uppercase : t -> t;
        value lowercase : t -> t;
        value input : in_channel -> t;
        value read : unit -> t;
        value print : t -> unit;
        value prerr : t -> unit;
        value prerr_backsp : t -> unit;
      end;
    module String :
      sig
        type t = 'abstract;
        value empty : t;
        value of_char : Char.t -> t;
        value of_ascii : string -> t;
        value length : t -> int;
        value set : t -> int -> Char.t -> unit;
        value get : t -> int -> Char.t;
        value sub : t -> int -> int -> t;
        value concat : t -> t -> t;
        value input_line : in_channel -> t;
        value output : out_channel -> t -> unit;
      end;
  end =
  struct
    value encoding = ref Iso_8859;
    value char_code = Char.code;
    value nbc c =
      if Char.code c < 0b10000000 then 1
      else if Char.code c < 0b11000000 then -1
      else if Char.code c < 0b11100000 then 2
      else if Char.code c < 0b11110000 then 3
      else if Char.code c < 0b11111000 then 4
      else if Char.code c < 0b11111100 then 5
      else if Char.code c < 0b11111110 then 6
      else -1
    ;
    module Char =
      struct
        type t = string;
       value of_ascii c = String.make 1 c;
        value to_ascii c =
           if String.length c = 1 && Char.code c.[0] < 128 then Some c.[0]
           else None
        ;
        value is_word_char c =
           if String.length c = 1 then
             match c.[0] with
             [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> True
             | x ->
                 if Char.code x < 128 then False
                 else
                   match encoding.val with
                   [ Ascii -> False
                   | Iso_8859 -> Char.code x >= 160
                   | Utf_8 -> assert False ] ]
           else True
        ;
        value ctrl_val c =
          if String.length c = 1 then
            let c = c.[0] in
            if Char.code c < 32 || Char.code c == 127 then
              Some (String.make 1 (Char.chr (127 land (Char.code c + 64))))
            else None
          else None
        ;
        value meta_ctrl_val c =
          if String.length c = 1 then
            let c = c.[0] in
            if Char.code c >= 128 && Char.code c < 160 then
              Some (String.make 1 (Char.chr (127 land (Char.code c + 64))))
            else None
          else None
        ;
        value not_ascii_val c =
          if String.length c = 1 then
            let c = c.[0] in
            if Char.code c >= 128 then
              let c1 = Char.chr (Char.code c / 100 + Char.code '0') in
              let c2 = Char.chr (Char.code c mod 100 / 10 + Char.code '0') in
              let c3 = Char.chr (Char.code c mod 10 + Char.code '0') in
              Some (String.make 1 c1, String.make 1 c2, String.make 1 c3)
            else None
          else None
        ;
        value uppercase c =
          match encoding.val with
          [ Ascii | Iso_8859 -> String.uppercase c
          | Utf_8 ->
              if String.length c = 1 then
                if Char.code c.[0] < 128 then String.uppercase c else c
              else c ]
        ;
        value lowercase c =
          match encoding.val with
          [ Ascii | Iso_8859 -> String.lowercase c
          | Utf_8 ->
              if String.length c = 1 then
                if Char.code c.[0] < 128 then String.lowercase c else c
              else c ]
        ;
        value get_char f =
          match encoding.val with
          [ Ascii | Iso_8859 -> String.make 1 (f ())
          | Utf_8 ->
              let c = f () in
              let nbc = nbc c in
              if nbc < 0 then "?"
              else if nbc = 1 then String.make 1 c
              else
                loop (String.make 1 c) (nbc - 1) where loop s n =
                  if n = 0 then s
                  else
                    let c = f () in
                    if Char.code c < 0b10000000 then "?"
                    else if Char.code c > 0b10111111 then "?"
                    else loop (s ^ String.make 1 c) (n - 1) ]
        ;
        value input ic = get_char (fun () -> input_char ic);
        value read =
          let buff = " " in
          fun () ->
            get_char
              (fun () ->
                 let len = Unix.read Unix.stdin buff 0 1 in
                 if len == 0 then raise End_of_file else buff.[0])
        ;
        value print c =
          if String.length c = 1 && c.[0] = '\n' then print_newline ()
          else print_string c
        ;
        value prerr c = output_string stderr c;
        value prerr_backsp c = output_char stderr '\b';
      end;
    module String =
      struct
        type t = array string;
        value empty = [| |];
        value of_char c = [| c |];
        value of_ascii s =
          Array.init (String.length s)
            (fun i ->
               if char_code s.[i] < 128 then String.make 1 s.[i]
               else invalid_arg "A.String.of_ascii")
        ;
        value length = Array.length;
        value set = Array.set;
        value get = Array.get;
        value sub = Array.sub;
        value concat = Array.append;
        value input_line ic =
          let s = input_line ic in
          match encoding.val with
          [ Ascii | Iso_8859 -> of_ascii s
          | Utf_8 ->
              loop [] 0 where loop list i =
                if i >= String.length s then Array.of_list (List.rev list)
                else
                  let n = nbc s.[i] in
                  if n < 0 then loop ["?" :: list] (i + 1)
                  else loop [String.sub s i n :: list] (i + n) ]
        ;
        value output oc s = Array.iter (output_string oc) s;
      end;
  end
;

type a_char = A.Char.t;
value print_a_char = A.Char.print;

DEFINE CTRL(x) = EVAL (Char.chr (Char.code x - (Char.code 'a' - 1)));
DEFINE META(x) = EVAL (Char.chr (Char.code x + 128));
DEFINE DEL = '\127';
DEFINE ESC = '\027';

open Sys;

value max_len = ref 70;
value set_max_len x =
  max_len.val := if x > 3 then x else failwith "set_max_len"
;

value son = ref None;
value set_son_pid pid = son.val := Some pid;

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
  | Start_o_sequence
  | Suspend
  | Transpose_chars
  | Unix_line_discard
  | Upcase_word
  | Yank ]
;

type istate = [ Normal | Quote | Escape | CSI | Oseq ];

value (command_of_char, set_char_command) =
  let command_vect = Array.create 256 Self_insert in
  (fun c ->
     match A.Char.to_ascii c with
     [ Some c -> command_vect.(Char.code c)
     | None -> Self_insert ],
   fun c comm -> command_vect.(Char.code c) := comm)
;

value (escape_command_of_char, set_escape_command) =
  let command_vect = Array.create 256 Abort in
  (fun c ->
     match A.Char.to_ascii c with
     [ Some c -> command_vect.(Char.code c)
     | None -> Abort ],
   fun c comm -> command_vect.(Char.code c) := comm)
;

value (csi_command_of_char, set_csi_command) =
  let command_vect = Array.create 256 Abort in
  (fun c ->
     match A.Char.to_ascii c with
     [ Some c -> command_vect.(Char.code c)
     | None -> Abort ],
   fun c comm -> command_vect.(Char.code c) := comm)
;

value (o_command_of_char, set_o_command) =
  let command_vect = Array.create 256 Abort in
  (fun c ->
     match A.Char.to_ascii c with
     [ Some c -> command_vect.(Char.code c)
     | None -> Abort ],
   fun c comm -> command_vect.(Char.code c) := comm)
;

value meta_as_escape = ref True;
value unset_meta_as_escape () = meta_as_escape.val := False;
value set_utf8 () = A.encoding.val := Utf_8;

value init_commands () = (
  set_char_command (CTRL 'a') Beginning_of_line;
  set_char_command (CTRL 'e') End_of_line;
  set_char_command (CTRL 'f') Forward_char;
  set_char_command (CTRL 'b') Backward_char;
  set_char_command (CTRL 'p') Previous_history;
  set_char_command (CTRL 'n') Next_history;
  set_char_command (CTRL 'r') Reverse_search_history;
  set_char_command (CTRL 'd') Delete_char;
  set_char_command (CTRL 'h') Backward_delete_char;
  set_char_command DEL Backward_delete_char;
  set_char_command (CTRL 't') Transpose_chars;
  set_char_command (CTRL 'q') Quoted_insert;
  set_char_command (CTRL 'k') Kill_line;
  set_char_command (CTRL 'y') Yank;
  set_char_command (CTRL 'u') Unix_line_discard;
  set_char_command (CTRL 'l') Refresh_line;
  set_char_command (CTRL 'g') Abort;
  set_char_command (CTRL 'c') Interrupt;
  set_char_command (CTRL 'z') Suspend;
  set_char_command (CTRL '|') Quit;
  set_char_command '\n' Accept_line;
  set_char_command (CTRL 'x') Operate_and_get_next;
  set_char_command ESC Start_escape_sequence;
  set_escape_command 'f' Forward_word;
  set_escape_command 'b' Backward_word;
  set_escape_command 'c' Capitalize_word;
  set_escape_command 'u' Upcase_word;
  set_escape_command 'l' Downcase_word;
  set_escape_command '<' Beginning_of_history;
  set_escape_command '>' End_of_history;
  set_escape_command 'd' Kill_word;
  set_escape_command (CTRL 'h') Backward_kill_word;
  set_escape_command DEL Backward_kill_word;
  set_escape_command '[' Start_csi_sequence;
  set_escape_command 'O' Start_o_sequence;
  set_escape_command '/' Expand_abbrev;
  set_csi_command 'A' Previous_history;
  set_csi_command 'B' Next_history;
  set_csi_command 'C' Forward_char;
  set_csi_command 'D' Backward_char;
  set_o_command 'A' Previous_history;
  set_o_command 'B' Next_history;
  set_o_command 'C' Forward_char;
  set_o_command 'D' Backward_char;
  if meta_as_escape.val then (
    set_char_command (META 'f') Forward_word;
    set_char_command (META 'b') Backward_word;
    set_char_command (META '<') Beginning_of_history;
    set_char_command (META '>') End_of_history;
    set_char_command (META 'c') Capitalize_word;
    set_char_command (META 'u') Upcase_word;
    set_char_command (META 'l') Downcase_word;
    set_char_command (META 'd') Kill_word;
    set_char_command (META (CTRL 'h')) Backward_kill_word;
    set_char_command (META DEL) Backward_kill_word;
    set_char_command (META '/') Expand_abbrev;
  )
  else ();
);

type line =
  { buf : mutable A.String.t;
    cur : mutable int;
    len : mutable int }
;
type abbrev_data =
  { hist : list A.String.t;
    rpos : int;
    clen : int;
    abbr : A.String.t;
    found : list A.String.t }
;

type state =
  { od : line;
    nd : line;
    line : line;
    last_line : mutable A.String.t;
    istate : mutable istate;
    shift : mutable int;
    cut : mutable A.String.t;
    last_comm : mutable command;
    histfile : mutable option out_channel;
    history : mutable Cursor.t A.String.t;
    abbrev : mutable option abbrev_data }
;

value put_bs st c = A.Char.prerr_backsp c;
value put_space st = output_char stderr ' ';
value put_newline st = prerr_endline "";
value flush_out st = flush stderr;
value bell () = (prerr_string "\007"; flush stderr);

value saved_tcio =
  try Unix.tcgetattr Unix.stdin with
  [ Unix.Unix_error _ _ _ -> (
      Printf.eprintf "Error: standard input is not a terminal\n";
      flush stderr;
      exit 1
    ) ]
;
value edit_tcio = ref None;

value set_edit () = (
  let tcio =
    match edit_tcio.val with
    [ Some e -> e
    | None -> (
        let tcio = Unix.tcgetattr Unix.stdin in
        tcio.Unix.c_echo := False;
        tcio.Unix.c_icanon := False;
        tcio.Unix.c_vmin := 1;
        tcio.Unix.c_isig := False;
        tcio.Unix.c_ixon := False;
        edit_tcio.val := Some tcio;
        tcio
      ) ]
  in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcio;
  init_commands ();
)
and unset_edit () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN saved_tcio;

value line_set_nth_char line i c =
  if i == A.String.length line.buf then
    line.buf := A.String.concat line.buf (A.String.of_char c)
  else
    A.String.set line.buf i c
;

value line_to_nd st = (
  let rec line_rec i = (
    if i == st.line.cur then st.nd.cur := st.nd.len else ();
    if i < st.line.len then (
      let c = A.String.get st.line.buf i in
      if c = A.Char.of_ascii '\t' then
        for i = st.nd.len + 1 to (st.nd.len + 8) / 8 * 8 do
          line_set_nth_char st.nd st.nd.len (A.Char.of_ascii ' ');
          st.nd.len := st.nd.len + 1;
        done
      else if
        match A.Char.ctrl_val c with
        [ Some c -> (
            line_set_nth_char st.nd st.nd.len (A.Char.of_ascii '^');
            line_set_nth_char st.nd (st.nd.len + 1) c;
            st.nd.len := st.nd.len + 2;
            True
          )
        | None -> False ]
      then ()
      else
        match A.encoding.val with
        [ Ascii ->
            match A.Char.not_ascii_val c with
            [ Some (c1, c2, c3) -> (
                line_set_nth_char st.nd st.nd.len (A.Char.of_ascii '\\');
                line_set_nth_char st.nd (st.nd.len + 1) c1;
                line_set_nth_char st.nd (st.nd.len + 2) c2;
                line_set_nth_char st.nd (st.nd.len + 3) c3;
                st.nd.len := st.nd.len + 4;
              )
            | None -> (
                line_set_nth_char st.nd st.nd.len c;
                st.nd.len := st.nd.len + 1
              ) ]
        | Iso_8859 ->
            match A.Char.meta_ctrl_val c with
            [ Some c -> (
                line_set_nth_char st.nd st.nd.len (A.Char.of_ascii 'M');
                line_set_nth_char st.nd (st.nd.len + 1) (A.Char.of_ascii '-');
                line_set_nth_char st.nd (st.nd.len + 2) (A.Char.of_ascii '^');
                line_set_nth_char st.nd (st.nd.len + 3) c;
                st.nd.len := st.nd.len + 4
              )
            | None -> (
                line_set_nth_char st.nd st.nd.len c;
                st.nd.len := st.nd.len + 1
              ) ]
        | Utf_8 -> (
            line_set_nth_char st.nd st.nd.len c;
            st.nd.len := st.nd.len + 1
          ) ];
      line_rec (i + 1)
    )
    else if st.nd.len > max_len.val then (
      let shift =
        if st.nd.cur - st.shift >= 0 &&
           st.nd.cur - st.shift < max_len.val - 2
        then
          st.shift
        else if st.nd.cur < max_len.val - 3 then 0
        else st.nd.cur - max_len.val / 2
      in
      for i = 0 to max_len.val - 3 do
        let ni = i + shift in
        A.String.set st.nd.buf i
          (if ni < st.nd.len then A.String.get st.nd.buf ni
           else A.Char.of_ascii ' ');
      done;
      A.String.set st.nd.buf (max_len.val - 2) (A.Char.of_ascii ' ');
      A.String.set st.nd.buf (max_len.val - 1)
        (if shift = 0 then A.Char.of_ascii '>'
         else if st.nd.len - shift < max_len.val - 2 then A.Char.of_ascii '<'
         else A.Char.of_ascii '*');
      st.nd.cur := st.nd.cur - shift;
      st.nd.len := max_len.val;
      st.shift := shift
    )
    else st.shift := 0
  )
  in
  st.nd.len := 0;
  line_rec 0
);

value display st =
  disp_rec 0 where disp_rec i =
    if i < st.nd.len then (
      if i >= st.od.len ||
         A.String.get st.od.buf i <> A.String.get st.nd.buf i
      then (
        while i < st.od.cur do
          st.od.cur := st.od.cur - 1;
          put_bs st (A.String.get st.od.buf i);
        done;
        while st.od.cur < i do
          let c = A.String.get st.nd.buf st.od.cur in
          st.od.cur := st.od.cur + 1;
          A.Char.prerr c;
        done;
        let c = A.String.get st.nd.buf i in
        line_set_nth_char st.od i c;
        st.od.cur := st.od.cur + 1;
        A.Char.prerr c
      )
      else ();
      disp_rec (i + 1)
    )
    else (
      if st.od.len > st.nd.len then (
        while st.od.cur < st.od.len do
          let c =
            if st.od.cur < st.nd.len then A.String.get st.nd.buf st.od.cur
            else A.Char.of_ascii ' '
          in
          A.Char.prerr c;
          st.od.cur := st.od.cur + 1;
        done;
        while st.od.cur > st.nd.len do
          st.od.cur := st.od.cur - 1;
          put_bs st (A.String.get st.od.buf st.od.cur);
          put_space st;
          put_bs st (A.Char.of_ascii ' ');
        done
      )
      else ();
      st.od.len := st.nd.len;
      while st.od.cur < st.nd.cur do
        A.Char.prerr (A.String.get st.nd.buf st.od.cur);
        st.od.cur := st.od.cur + 1;
      done;
      while st.od.cur > st.nd.cur do
        st.od.cur := st.od.cur - 1;
        put_bs st (A.String.get st.nd.buf st.od.cur);
      done;
      flush_out st
    )
;

value update_output st = (line_to_nd st; display st);

value balance_paren st c =
  match A.Char.to_ascii c with
  [ Some (')' | ']' | '}' as c) ->
      let i =
        find_lparen c (st.line.cur - 2) where find_lparen r i =
          if i < 0 then i
          else
            match A.Char.to_ascii (A.String.get st.line.buf i) with
            [ Some (')' | ']' | '}' as c) ->
                find_lparen r (find_lparen c (i - 1) - 1)
            | Some '(' -> if r == ')' then i else -1
            | Some '[' -> if r == ']' then i else -1
            | Some '{' -> if r == '}' then i else -1
            | Some '"' ->
                let rec skip_string i =
                  if i < 0 then i
                  else if
                    A.String.get st.line.buf i == A.Char.of_ascii '"'
                  then i - 1
                  else skip_string (i - 1)
                in
                find_lparen r (skip_string (i - 1))
            | _ -> find_lparen r (i - 1) ]
      in
      if i >= 0 then (
        let c = st.line.cur in
        st.line.cur := i;
        update_output st;
        st.line.cur := c;
        let _ = Unix.select [Unix.stdin] [] [] 1.0 in
        ()
      )
      else ()
  | Some _ | None -> () ]
;

value delete_char st = (
  st.line.len := st.line.len - 1;
  for i = st.line.cur to st.line.len - 1 do
    A.String.set st.line.buf i (A.String.get st.line.buf (i + 1));
  done
);

value insert_char st x = (
  for i = st.line.len downto st.line.cur + 1 do
    line_set_nth_char st.line i (A.String.get st.line.buf (i - 1));
  done;
  st.line.len := st.line.len + 1;
  line_set_nth_char st.line st.line.cur x
);

value move_in_word buf e f g =
  move_rec where move_rec i =
    if e i then i
    else if A.Char.is_word_char (A.String.get buf i) then f move_rec i
    else g move_rec i
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
    forward_move st.line (fun _ i -> i) (fun mv i -> (delete_char st; mv i))
      i
  in
  forward_move st.line (fun mv i -> (delete_char st; mv i)) (fun _ i -> i) i
;

value backward_kill_word st = (
  let k = backward_word st.line in
  let sh = st.line.cur - k in
  st.line.len := st.line.len - sh;
  for i = k to st.line.len - 1 do
    A.String.set st.line.buf i (A.String.get st.line.buf (i + sh));
  done;
  k
);

value capitalize_word st =
  let i = st.line.cur in
  let i0 = forward_move st.line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move st.line
    (fun mv i -> (
       let f = if i == i0 then A.Char.uppercase else A.Char.lowercase in
       A.String.set st.line.buf i (f (A.String.get st.line.buf i));
       mv (i + 1)
     ))
    (fun _ i -> i) i0
;

value upcase_word st =
  let i = st.line.cur in
  let i = forward_move st.line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move st.line
    (fun mv i -> (
       let f = A.Char.uppercase in
       A.String.set st.line.buf i (f (A.String.get st.line.buf i));
       mv (i + 1)
     ))
    (fun _ i -> i) i
;

value downcase_word st =
  let i = st.line.cur in
  let i = forward_move st.line (fun _ i -> i) (fun mv i -> mv (i + 1)) i in
  forward_move st.line
    (fun mv i -> (
       let f = A.Char.lowercase in
       A.String.set st.line.buf i (f (A.String.get st.line.buf i));
       mv (i + 1)
     ))
    (fun _ i -> i) i
;

value transpose_chars st =
  if st.line.cur == st.line.len then (
    let c = A.String.get st.line.buf (st.line.cur - 1) in
    A.String.set st.line.buf (st.line.cur - 1)
      (A.String.get st.line.buf (st.line.cur - 2));
    A.String.set st.line.buf (st.line.cur - 2) c;
  )
  else (
    let c = A.String.get st.line.buf st.line.cur in
    A.String.set st.line.buf st.line.cur
      (A.String.get st.line.buf (st.line.cur - 1));
    A.String.set st.line.buf (st.line.cur - 1) c;
    st.line.cur := st.line.cur + 1
  )
;

value set_line st str = (
  st.line.len := 0;
  st.line.cur := 0;
  for i = 0 to A.String.length str - 1 do
    insert_char st (A.String.get str i);
    st.line.cur := st.line.len;
  done
);

value save_if_last st =
  if Cursor.is_last_line st.history then
    st.last_line := A.String.sub st.line.buf 0 st.line.len
  else ()
;

value previous_history st =
  try (
    save_if_last st;
    Cursor.before st.history;
    set_line st (Cursor.peek st.history)
  )
  with
  [ Cursor.Failure -> bell () ]
;

value next_history st =
  try (Cursor.after st.history; set_line st (Cursor.peek st.history)) with
  [ Cursor.Failure -> set_line st st.last_line ]
;

value reverse_search_history st =
  let question str =
    List.fold_left A.String.concat (A.String.of_ascii "(reverse-i-search)'")
      [str; A.String.of_ascii "': "]
  in
  let make_line str fstr = (
    st.line.cur := 0;
    st.line.len := 0;
    let len = A.String.length str in
    for i = 0 to len - 1 do
      insert_char st (A.String.get str i);
      st.line.cur := st.line.cur + 1;
    done;
    let len = A.String.length fstr in
    for i = 0 to len - 1 do
      insert_char st (A.String.get fstr i);
      st.line.cur := st.line.cur + 1;
    done
  )
  in
  let initial_str = A.String.sub st.line.buf 0 st.line.len in
  let rec find_line (cnt, fstr) str =
    find_rec 0 0 where find_rec ifstr istr =
      if istr == A.String.length str then (cnt, fstr)
      else if ifstr == A.String.length fstr then
        if try (Cursor.before st.history; True) with
           [ Cursor.Failure -> False ]
        then
          find_line (cnt + 1, Cursor.peek st.history) str
        else (bell (); (cnt, fstr))
      else if A.String.get str istr != A.String.get fstr ifstr then
        find_rec (ifstr + 1) 0
      else find_rec (ifstr + 1) (istr + 1)
  in
  let rec incr_search (cnt, fstr) str = (
    let q = question str in
    make_line q fstr;
    st.line.cur := A.String.length q - 3;
    update_output st;
    let c = A.Char.read () in
    match command_of_char c with
    [ Start_escape_sequence -> fstr
    | Self_insert ->
        let str = A.String.concat str (A.String.of_char c) in
        incr_search (find_line (cnt, fstr) str) str
    | Backward_delete_char ->
        if A.String.length str == 0 then incr_search (cnt, fstr) str
        else (
          let str = A.String.sub str 0 (A.String.length str - 1) in
          for i = 1 to cnt do Cursor.after st.history; done;
          incr_search (find_line (0, initial_str) str) str
        )
    | Abort -> (
        for i = 1 to cnt do Cursor.after st.history; done;
        bell ();
        initial_str
      )
    | Reverse_search_history ->
        let (cnt, fstr) =
          try (
            Cursor.before st.history;
            find_line (cnt + 1, Cursor.peek st.history) str
          )
          with
          [ Cursor.Failure -> (bell (); (cnt, initial_str)) ]
        in
        incr_search (cnt, fstr) str
    | _ -> fstr ]
  )
  in
  let fstr = incr_search (0, initial_str) A.String.empty in
  make_line A.String.empty fstr
;

value rec beginning_of_history st = (
  save_if_last st;
  Cursor.goto_first st.history;
  try set_line st (Cursor.peek st.history) with [ Cursor.Failure -> bell () ]
);

value rec end_of_history st = (
  Cursor.goto_last st.history;
  set_line st st.last_line
);

value rec back_search st ad hist rpos =
  match hist with
  [ [] -> (
      for i = 0 to A.String.length ad.abbr - 1 do
        insert_char st (A.String.get ad.abbr i);
        st.line.cur := st.line.cur + 1;
      done;
      bell ()
    )
  | [l :: ll] ->
      let i = A.String.length l - rpos in
      if i <= 0 then back_search st ad ll 0
      else
        let i = backward_word {buf = l; cur = i; len = A.String.length l} in
        if A.String.length l - i < A.String.length ad.abbr then
          back_search st ad [l :: ll] (A.String.length l - i)
        else if A.String.sub l i (A.String.length ad.abbr) = ad.abbr then
          let i1 = forward_word {buf = l; cur = i; len = A.String.length l} in
          let f = A.String.sub l i (i1 - i) in
          if List.mem f ad.found then
            back_search st ad [l :: ll] (A.String.length l - i)
          else (
            let ad =
              {hist = [l :: ll]; rpos = A.String.length l - i1; clen = i1 - i;
               abbr = ad.abbr; found = [f :: ad.found]}
            in
            st.abbrev := Some ad;
            for i = 0 to A.String.length f - 1 do
              insert_char st (A.String.get f i);
              st.line.cur := st.line.cur + 1;
            done
          )
        else back_search st ad [l :: ll] (A.String.length l - i) ]
;

value expand_abbrev st abbrev = (
  let ad =
    match abbrev with
    [ Some x -> x
    | None ->
        let len = get_word_len st in
        let abbr = A.String.sub st.line.buf (st.line.cur - len) len in
        let line_beg = A.String.sub st.line.buf 0 (st.line.cur - len) in
        let line_end =
          A.String.sub st.line.buf st.line.cur (st.line.len - st.line.cur)
        in
        {hist = [line_beg :: Cursor.get_all st.history @ [line_end]];
         rpos = 0; clen = len; abbr = abbr; found = [abbr]} ]
  in
  for i = 1 to ad.clen do
    st.line.cur := st.line.cur - 1;
    delete_char st;
  done;
  back_search st ad ad.hist ad.rpos;
  update_output st
);

value rec update_line st comm c = (
  let abbrev = st.abbrev in
  st.abbrev := None;
  match comm with
  [ Beginning_of_line ->
      if st.line.cur > 0 then (st.line.cur := 0; update_output st) else ()
  | End_of_line ->
      if st.line.cur < st.line.len then (
        st.line.cur := st.line.len;
        update_output st
      )
      else ()
  | Forward_char ->
      if st.line.cur < st.line.len then (
        st.line.cur := st.line.cur + 1;
        update_output st
      )
      else ()
  | Backward_char ->
      if st.line.cur > 0 then (
        st.line.cur := st.line.cur - 1;
        update_output st
      )
      else ()
  | Forward_word ->
      if st.line.cur < st.line.len then (
        st.line.cur := forward_word st.line;
        update_output st
      )
      else ()
  | Backward_word ->
      if st.line.cur > 0 then (
        st.line.cur := backward_word st.line;
        update_output st
      )
      else ()
  | Capitalize_word ->
      if st.line.cur < st.line.len then (
        st.line.cur := capitalize_word st;
        update_output st
      )
      else ()
  | Upcase_word ->
      if st.line.cur < st.line.len then (
        st.line.cur := upcase_word st;
        update_output st
      )
      else ()
  | Downcase_word ->
      if st.line.cur < st.line.len then (
        st.line.cur := downcase_word st;
        update_output st
      )
      else ()
  | Previous_history -> (previous_history st; update_output st)
  | Next_history -> (next_history st; update_output st)
  | Beginning_of_history -> (beginning_of_history st; update_output st)
  | End_of_history -> (end_of_history st; update_output st)
  | Reverse_search_history -> (reverse_search_history st; update_output st)
  | Delete_char -> (
      if st.line.len = 0 then raise End_of_file else ();
      if st.line.cur < st.line.len then (delete_char st; update_output st)
      else ()
    )
  | Backward_delete_char ->
      if st.line.cur > 0 then (
        st.line.cur := st.line.cur - 1;
        delete_char st;
        update_output st
      )
      else ()
  | Transpose_chars ->
      if st.line.len > 1 && st.line.cur > 0 then (
        transpose_chars st;
        update_output st
      )
      else ()
  | Kill_word ->
      if st.line.cur < st.line.len then (
        st.line.cur := kill_word st;
        update_output st
      )
      else ()
  | Backward_kill_word ->
      if st.line.cur > 0 then (
        st.line.cur := backward_kill_word st;
        update_output st
      )
      else ()
  | Quoted_insert -> st.istate := Quote
  | Start_escape_sequence -> st.istate := Escape
  | Start_csi_sequence -> st.istate := CSI
  | Start_o_sequence -> st.istate := Oseq
  | Self_insert -> (
      insert_char st c;
      st.line.cur := st.line.cur + 1;
      balance_paren st c;
      update_output st
    )
  | Expand_abbrev -> expand_abbrev st abbrev
  | Refresh_line -> (
      put_newline st;
      st.od.cur := 0;
      st.od.len := 0;
      update_output st
    )
  | Kill_line -> (
      st.cut :=
        A.String.sub st.line.buf st.line.cur (st.line.len - st.line.cur);
      if st.line.len > st.line.cur then (
        st.line.len := st.line.cur;
        update_output st
      )
      else ()
    )
  | Unix_line_discard ->
      if st.line.cur > 0 then (
        st.line.cur := 0;
        st.line.len := 0;
        update_output st
      )
      else ()
  | Yank ->
      if A.String.length st.cut > 0 then (
        for i = 0 to A.String.length st.cut - 1 do
          insert_char st (A.String.get st.cut i);
          st.line.cur := st.line.cur + 1;
        done;
        update_output st
      )
      else ()
  | Abort -> bell ()
  | Interrupt -> (
      if st.line.cur > 0 then (
        st.line.cur := 0;
        st.line.len := 0;
        update_output st
      )
      else ();
      match son.val with
      [ Some pid -> Unix.kill pid Sys.sigint
      | _ -> () ]
    )
  | Suspend -> (
      unset_edit ();
      Unix.kill (Unix.getpid ()) Sys.sigtstp;
      set_edit ();
      st.od.cur := 0;
      st.od.len := 0;
      update_output st
    )
  | Quit ->
      match son.val with
      [ Some pid -> Unix.kill pid Sys.sigquit
      | _ -> () ]
  | _ -> () ]
);

value save_history st line =
  let last_line =
    try Cursor.peek_last st.history with [ Cursor.Failure -> A.String.empty ]
  in
  if line <> last_line && line <> A.String.empty then (
    Cursor.insert_last st.history line;
    match st.histfile with
    [ Some fdo -> (A.String.output fdo line; output_char fdo '\n'; flush fdo)
    | None -> () ]
  )
  else ()
;

local st =
  {od = {buf = A.String.empty; cur = 0; len = 0};
   nd = {buf = A.String.empty; cur = 0; len = 0};
   line = {buf = A.String.empty; cur = 0; len = 0};
   last_line = A.String.empty; istate = Normal; shift = 0;
   cut = A.String.empty; last_comm = Accept_line; histfile = None;
   history = Cursor.create (); abbrev = None}
in
value edit_line () = (
  let rec edit_loop () = (
    let c = A.Char.read () in
    let comm =
      match st.istate with
      [ Quote -> Self_insert
      | Normal -> command_of_char c
      | Escape -> escape_command_of_char c
      | CSI -> csi_command_of_char c
      | Oseq -> o_command_of_char c ]
    in
    st.istate := Normal;
    st.last_comm := comm;
    match comm with
    [ Accept_line | Operate_and_get_next -> (
        let v_max_len = max_len.val in
        max_len.val := 10000;
        update_output st;
        max_len.val := v_max_len;
        put_newline st;
        let line = A.String.sub st.line.buf 0 st.line.len in
        st.abbrev := None;
        save_history st line;
        line
      )
    | _ -> (update_line st comm c; edit_loop ()) ]
  )
  in
  st.od.len := 0;
  st.od.cur := 0;
  st.line.len := 0;
  st.line.cur := 0;
  if st.last_comm == Operate_and_get_next then
    try (
      Cursor.after st.history;
      set_line st (Cursor.peek st.history);
      update_output st
    )
    with
    [ Cursor.Failure -> () ]
  else Cursor.goto_last st.history;
  edit_loop ()
)
and open_histfile trunc file = (
  if not trunc then
    match try Some (open_in file) with _ -> None with
    [ Some fi -> (
        try
          while True do
            Cursor.insert st.history (A.String.input_line fi);
          done
        with
        [ End_of_file -> () ];
        close_in fi
      )
    | _ -> () ]
  else ();
  let fd =
    Unix.openfile file
      ([Unix.O_WRONLY; Unix.O_CREAT] @
       (if trunc then [Unix.O_TRUNC] else []))
      0o666
  in
  let fdo = Unix.out_channel_of_descr fd in
  if not trunc then seek_out fdo (out_channel_length fdo) else ();
  st.histfile := Some fdo
)
and close_histfile () =
  match st.histfile with
  [ Some fdo -> close_out fdo
  | None -> () ]
;

value (set_prompt, get_prompt, input_char) =
  let prompt = ref ""
  and buff = ref A.String.empty
  and ind = ref 1 in
  let set_prompt x = prompt.val := x
  and get_prompt () = prompt.val
  and input_char ic =
    if ic != stdin then A.Char.input ic
    else (
      if ind.val > A.String.length buff.val then (
        prerr_string prompt.val;
        flush stderr;
        try (set_edit (); buff.val := edit_line (); unset_edit ()) with e -> (
          unset_edit ();
          raise e
        );
        ind.val := 0
      )
      else ();
      let c =
        if ind.val == A.String.length buff.val then
          A.Char.of_ascii '\n'
        else
          A.String.get buff.val ind.val
      in
      ind.val := ind.val + 1;
      c
    )
  in
  (set_prompt, get_prompt, input_char)
;
