(* $Id$ *)

open Ledit;
open Sys;

value version = "1.2";

value usage () =
  do prerr_string "Usage: ";
     prerr_string argv.(0);
     prerr_endline " [options] [-c comm [args]]";
     prerr_endline " -h file : history file";
     prerr_endline " -x  : don't remove old contents of history";
     prerr_endline " -l len : line max length";
     prerr_endline " -v : prints ledit version and exit";
     prerr_endline "If -c present, exec comm [args] as child process";
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
       | "-c" ->
           let i = succ i in
           if i < Array.length argv then
             do comm.val := argv.(i);
                args.val := Array.sub argv i (Array.length argv - i);
             return Array.length argv
           else Array.length argv
       | "-v" ->
           do Printf.printf "Ledit version %s\n" version; flush stdout; return
           exit 0
       | _ -> if i == 1 && argv.(1) = argv.(0) then i + 1 else usage () ])
  else ()
in
arg_loop 1;
set_echo False;

value rec read_loop () =
  do try
       match input_char stdin with
       [ '\n' -> print_newline ()
       | x -> print_char x ]
     with
     [ Break -> () ];
  return read_loop ()
;

open Unix;

value saved_tcio = tcgetattr stdin;

value set_edit () =
  let tcio = tcgetattr stdin in
  do tcio.c_echo := False;
     tcio.c_icanon := False;
     tcio.c_vmin := 1;
     tcio.c_isig := False;
     tcio.c_ixon := False;
     tcsetattr stdin TCSANOW tcio;
  return ()
and unset_edit () = tcsetattr stdin TCSANOW saved_tcio;

value string_of_signal =
  fun
  [ 2 -> "Interrupted"
  | 3 -> "Quit"
  | 10 -> "Bus error"
  | 11 -> "Segmentation fault"
  | x -> "Signal " ^ string_of_int x ]
;

value go () =
  let (id, od) = pipe () in
  let pid = fork () in
  if pid < 0 then failwith "fork"
  else if pid > 0 then
    do dup2 od stdout;
       close id;
       close od;
       set_edit ();
       set_son pid;
       signal sigchld
         (Signal_handle
            (fun _ ->
               do match snd (waitpid [WNOHANG] pid) with
                  [ WSIGNALED sign ->
                      do prerr_endline (string_of_signal sign);
                         flush Pervasives.stderr;
                      return ()
                  | _ -> () ];
               return raise End_of_file));
    return
    try
      do if histfile.val <> "" then open_histfile trunc.val histfile.val
         else ();
         catch_break True;
         read_loop ();
         if histfile.val <> "" then close_histfile () else ();
         unset_edit ();
      return ()
    with x ->
      do unset_edit (); close stdout; return
      match x with
      [ End_of_file -> ()
      | _ ->
          do prerr_string "(ledit) "; flush Pervasives.stderr; raise x; return
          () ]
  else
    do dup2 id stdin; close id; close od; execvp comm.val args.val; return
    failwith "execv"
;

Printexc.catch go ();
