(* $Id$ *)

value input_char : in_channel -> char;
value set_prompt : string -> unit;
value get_prompt : unit -> string;
value open_histfile : bool -> string -> unit;
value close_histfile : unit -> unit;
value set_max_len : int -> unit;
value set_echo : bool -> unit;
value set_son : int -> unit;
