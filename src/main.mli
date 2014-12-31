exception Session_End
exception Unknown_Command
exception Syntax_Error
exception File_Not_Existing

type session_state_type = {
  mutable repeat : int;
  mutable interp : Script.interpreter_state_type;
  mutable active : bool;
  mutable screen_size_x : int;
  mutable screen_size_y : int;
  mutable editor : string;
}

val state : session_state_type
val root_frame : Script.frame_type
val next_token : Genlex.token Stream.t -> Genlex.token
val get_point : Genlex.token Stream.t -> Drawables.point_type
val get_distance : Genlex.token Stream.t -> int
val get_name : Genlex.token Stream.t -> string
val do_set : Genlex.token Stream.t -> unit
val do_move : Genlex.token Stream.t -> unit
val do_turn : Genlex.token Stream.t -> unit
val do_scale : Genlex.token Stream.t -> unit
val do_rectangle : Genlex.token Stream.t -> unit
val do_circle : Genlex.token Stream.t -> unit
val do_line : Genlex.token Stream.t -> unit
val do_store : Genlex.token Stream.t -> unit
val do_clear : 'a -> unit
val do_draw : Genlex.token Stream.t -> unit
val do_resize : int -> int -> unit
val do_screen : Genlex.token Stream.t -> unit
val do_list : Genlex.token Stream.t -> unit
val do_info : 'a -> unit
val do_save : Genlex.token Stream.t -> unit
val lexer : char Stream.t -> Genlex.token Stream.t
val execute_command : Genlex.token Stream.t -> unit
val do_batch : string -> unit
val do_edit : Genlex.token Stream.t -> unit
val cfg : Helper.config_type
