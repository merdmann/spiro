(* This file is part of Spiro.

    Spiro is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Spiro is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Spiro.  If not, see <http://www.gnu.org/licenses/>.

*)
open Drawables;;

exception Metafile_Not_Found

type form_type = 
	  Rectangle of Drawables.rectangle_type 
  | Line of Drawables.line_type					  (* draw a line *)
  | Circle of Drawables.circle_type 			(* draw a circle *)
  | Point of Drawables.point_type				  (* draw a point *)
  | Set of Drawables.point_type    			  (* set the cursor *)
  | Move of Drawables.move_type    			  (* move the cursor *) 
  | Draw of metafile_type				          (* draw a meta file *)
	| Rotate of float                       (* rotate by a given turn *)
	| Scale of float                        (* scale *)
	| Repeat of int                         (* repeat the next operation n times *)
  | Null

and metafile_type = 
{ 
	 mutable name  : string;
   mutable items : form_type list;		(* list of elements *)     
}

type frame_type = {
	mutable origin : point_type;
	mutable turn   : float;
	mutable scale  : float 
}


(* this maintains the intepreter state *)
type interpreter_state_type = 
{
	mutable cursor  : point_type ;
	mutable current : metafile_type;
	mutable files   : (string * metafile_type) list;
}
	

val make_frame : ?myturn:float -> ?myscale:float -> int -> int  -> frame_type 
	
(* draw the metafile using the specified origin *)
val display : interpreter_state_type -> frame_type -> string -> unit   
  
(* add an item to the metafile *)
val add : interpreter_state_type -> form_type -> unit
 
(* store the current commands as metafile with the given name *)
val store : interpreter_state_type -> string -> unit

val make_interpreter : unit -> interpreter_state_type 

val list_metafiles : interpreter_state_type -> unit 

(* get the metafile from a name *)
val get_metafile : interpreter_state_type -> string -> metafile_type 

(* show the contents of a metafile *)
val display_source : ?chan:out_channel -> metafile_type -> unit

val get_cursor : interpreter_state_type -> point_type
val set_cursor : interpreter_state_type -> point_type -> unit

val clear_metafile : interpreter_state_type -> unit 

 