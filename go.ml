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

open Ledit;
open Sys;

value version = "1.5";

value usage () =
  do prerr_string "Usage: ";
     prerr_string argv.(0);
     prerr_endline " [options] [comm [args]]";
     prerr_endline " -h file : history file";
     prerr_endline " -x  : don't remove old contents of history";
     prerr_endline " -l len : line max length";
     prerr_endline " -v : prints ledit version and exit";
     prerr_endline "Exec comm [args] as child process";
  return exit 1
;

value get_arg i = if i >= Array.length argv then usage () else argv.(i);

value histfile = ref "";
value trunc = ref True;
value comm = ref "cat";
value args = ref [| "cat" |];

let rec arg_loop i =
  if i < Array.length argv then
    arg_loop
      (match argv.(i) with
       [ "-h" -> do histfile.val := get_arg (i + 1); return i + 2
       | "-l" ->
           let x = get_arg (i + 1) in
           do try set_max_len (int_of_string x) with _ -> usage (); return
           i + 2
       | "-x" -> do trunc.val := False; return i + 1
       | "-v" ->
           do Printf.printf "Ledit version %s\n" version; flush stdout; return
           exit 0
       | _ ->
           let i = if argv.(i) = "-c" then i + 1 else i in
           if i < Array.length argv then
             do comm.val := argv.(i);
                args.val := Array.sub argv i (Array.length argv - i);
             return Array.length argv
           else Array.length argv ])
  else ()
in
arg_loop 1;

open Unix;

value string_of_signal =
  fun
  [ 2 -> "Interrupted"
  | 3 -> "Quit"
  | 10 -> "Bus error"
  | 11 -> "Segmentation fault"
  | x -> "Signal " ^ string_of_int x ]
;

value rec read_loop () =
  do try
       match input_char Pervasives.stdin with
       [ '\n' -> print_newline ()
       | x -> print_char x ]
     with
     [ Break -> () ];
  return read_loop ()
;

value go () =
  let (id, od) = pipe () in
  let pid = fork () in
  if pid < 0 then failwith "fork"
  else if pid > 0 then
    do dup2 od stdout;
       close id;
       close od;
       set_son pid;
       signal sigchld
         (Signal_handle
            (fun _ ->
               match snd (waitpid [WNOHANG] pid) with
               [ WSIGNALED sign ->
                   do prerr_endline (string_of_signal sign);
                      flush Pervasives.stderr;
                   return raise End_of_file
               | _ -> raise End_of_file ]));
    return
    try
      do if histfile.val <> "" then open_histfile trunc.val histfile.val
         else ();
         catch_break True;
         read_loop ();
         if histfile.val <> "" then close_histfile () else ();
      return ()
    with x ->
      do signal sigchld Signal_ignore;
         try do close stdout; return let _ = wait () in () with
         [ Unix_error _ _ _ -> () ];
      return
      match x with
      [ End_of_file -> ()
      | _ ->
          do prerr_string "(ledit) "; flush Pervasives.stderr; return raise x ]
  else
    do dup2 id stdin; close id; close od; execvp comm.val args.val; return
    failwith "execv"
;

value handle f a =
  try f a with
  [ Unix.Unix_error code fname param ->
      do Printf.eprintf "Unix error: %s\nOn function %s %s\n"
           (Unix.error_message code) fname param;
         flush Pervasives.stderr;
      return exit 2
  | e -> Printexc.catch raise e ]
;

handle go ();
