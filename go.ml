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

open Ledit;
open Sys;

value version = "1.11";

value usage () =
  do {
    prerr_string "Usage: ";
    prerr_string argv.(0);
    prerr_endline " [options] [comm [args]]";
    prerr_endline " -h file : history file";
    prerr_endline " -x  : don't remove old contents of history";
    prerr_endline " -l len : line max length";
    prerr_endline " -v : prints ledit version and exit";
    prerr_endline "Exec comm [args] as child process";
    exit 1
  }
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
       [ "-h" -> do { histfile.val := get_arg (i + 1); i + 2 }
       | "-l" ->
           let x = get_arg (i + 1) in
           do { try set_max_len (int_of_string x) with _ -> usage (); i + 2 }
       | "-x" -> do { trunc.val := False; i + 1 }
       | "-v" ->
           do {
             Printf.printf "Ledit version %s\n" version; flush stdout; exit 0
           }
       | _ ->
           let i = if argv.(i) = "-c" then i + 1 else i in
           if i < Array.length argv then do {
             comm.val := argv.(i);
             args.val := Array.sub argv i (Array.length argv - i);
             Array.length argv
           }
           else Array.length argv ])
  else ()
in
arg_loop 1;

value string_of_signal =
  fun
  [ 2 -> "Interrupted"
  | 3 -> "Quit"
  | 10 -> "Bus error"
  | 11 -> "Segmentation fault"
  | x -> "Signal " ^ string_of_int x ]
;

value rec read_loop () =
  do {
    try
      match input_char stdin with
      [ '\n' -> print_newline ()
      | x -> print_char x ]
    with
    [ Break -> () ];
    read_loop ()
  }
;

value stupid_hack_to_avoid_sys_error_at_exit () =
  Unix.dup2 (Unix.openfile "/dev/null" [Unix.O_WRONLY] 0) Unix.stdout
;

value go () =
  let (id, od) = Unix.pipe () in
  let pid = Unix.fork () in
  if pid < 0 then failwith "fork"
  else if pid > 0 then do {
    Unix.dup2 od Unix.stdout;
    Unix.close id;
    Unix.close od;
    set_son pid;
    let _ : signal_behavior =
      signal sigchld
        (Signal_handle
           (fun _ ->
              match snd (Unix.waitpid [Unix.WNOHANG] pid) with
              [ Unix.WSIGNALED sign ->
                  do {
                    prerr_endline (string_of_signal sign);
                    flush stderr;
                    raise End_of_file
                  }
              | _ -> raise End_of_file ]))
    in
    try
      do {
        if histfile.val <> "" then open_histfile trunc.val histfile.val
        else ();
        catch_break True;
        read_loop ();
        if histfile.val <> "" then close_histfile () else ()
      }
    with x ->
      let _ : signal_behavior = signal sigchld Signal_ignore in
      do {
        try do { Unix.close Unix.stdout; let _ = Unix.wait () in () } with
        [ Unix.Unix_error _ _ _ -> () ];
        stupid_hack_to_avoid_sys_error_at_exit ();
        match x with
        [ End_of_file -> ()
        | _ ->
            do { prerr_string "(ledit) "; flush stderr; raise x } ]
      }
  }
  else do {
    Unix.dup2 id Unix.stdin;
    Unix.close id;
    Unix.close od;
    Unix.execvp comm.val args.val;
    failwith "execv"
  }
;

value handle f a =
  try f a with
  [ Unix.Unix_error code fname param ->
      do {
        Printf.eprintf "Unix error: %s\nOn function %s %s\n"
          (Unix.error_message code) fname param;
        flush stderr;
        exit 2
      }
  | e -> Printexc.catch raise e ]
;

handle go ();
