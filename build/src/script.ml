(*  This file is part of Spiro.

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

(** This module draws the contents of a metafile in a given refernce frame *)

open Graphics;;
open Drawables;;
open Printf;;
open Transformation;;

exception Metafile_Not_Found ;;
exception End_of_Metafile ;;

type form_type = 
      Rectangle of Drawables.rectangle_type 
    | Line of Drawables.line_type				(* draw a line *)
    | Circle of Drawables.circle_type 			(* draw a circle *)
    | Point of Drawables.point_type				(* draw a point *)
    | Set of Drawables.point_type    			(* set the cursor *)
    | Move of Drawables.move_type    			(* move the cursor *) 
    | Draw of metafile_type				        (* draw a meta file *)
	| Rotate of float                       	(* rotate by a given turn *)
	| Scale of float                        	(* scale *)
	| Repeat of int                         	(* repeat the next operation n times *)
  	| Null
	and metafile_type = { 
	 	mutable name  : string;
   		mutable items : form_type list;		(* list of elements *)     
	};;


(* reference frame *)
type frame_type = {
	mutable origin : point_type;
	mutable turn   : float;
	mutable scale  : float 
};;


(* this maintains the intepreter state *)
type interpreter_state_type = 
{
	mutable cursor  : point_type ;
	mutable current : metafile_type;
	mutable files   : (string * metafile_type) list;
};;

let make_metafile mf_name = 
	if mf_name = "" then  { name = "xxxyyy" ; items = [] } else { name = mf_name; items = [] };; 

(* make an blank interpreter *)
let make_interpreter () = {
  cursor = make_point 0 0;
	current = make_metafile ""; 
	files = [ ("", make_metafile "" ) ]};;

let make_frame ?(myturn = 0.0) ?(myscale = 1.0) x y = 
				{ origin = make_point x y; turn = myturn; scale = myscale};;

	
let frame_to_string f = sprintf "origin=%s turn=%f scale=%f" (to_string f.origin) f.turn f.scale ;;		
				
(* get the current cursor *)
let get_cursor interp =
	interp.cursor
;;

(* set the cursor *)
let set_cursor interp value =
	interp.cursor <- value
;;	

(* list the files stored in the interpreter *)
let list_metafiles interp = 
	List.iter (fun x -> 
		let name = (fst x) in
		let file = (snd x) in
		if name = "" then
			printf "%s %d\n" file.name (List.length file.items)
		else 
			printf "%s %d\n" name (List.length file.items) 
	) interp.files 
;;

(* store the current metafile with a given name in the interpreter *)
let store interp name =
	let exist n  = 
		try List.assoc n interp.files; true
		with Not_found -> false in 
			
	if (exist name) then
		interp.files <- List.remove_assoc name interp.files ;
				
  	interp.current.name <- name ;
  	interp.files <- List.append interp.files [ ( name, interp.current ) ];
	interp.current <- make_metafile ""
;;

(* add an item to the metafile *)
let add interp item = 
	interp.current.items <- List.append interp.current.items [ item ] 
;;

(* clear the current metafile *)
let clear_metafile interp = 
	interp.current <- make_metafile "";
;;		

(* get a metafile by name *)
let get_metafile interp name = 
	if name = "" then
		 interp.current 
  else 
		try 
			List.assoc name interp.files 
		with Not_found ->
			raise Metafile_Not_Found
;;

(* draw the metafile in a given frame *)
let rec draw interp f frame =
	let cursor = make_point 0 0 in
	let myframe = frame in 
	let repeat = ref 1 in

	(*printf "Draw %s %s\n" f.name (frame_to_string myframe); *)
	let tr p = translation ( scale myframe.scale (rotate myframe.turn p)) myframe.origin in  
		List.iter (fun o ->
			let c = tr cursor in  
	    		let ox = c.x  in
	    			let oy = c.y  in 
	      				moveto ox oy;				
	  					match o with
	          	  			(Line a)       -> let p = tr (translation a.right_point cursor) in
	          	  								lineto p.x p.y
	        			  | (Circle a)     -> draw_circle ox oy (scale_int a.radius myframe.scale)
	        			  | (Rectangle a)  -> let connect x y  = 
											  	let q = tr (translation (make_point x y) cursor) in 
											  		lineto q.x q.y in
														connect a.right_corner.x 0;
														connect a.right_corner.x a.right_corner.y;
														connect 0 a.right_corner.y;
														connect 0 0
	        			  | (Point a)      -> plot ox oy
	        			  | (Draw a)       -> myframe.origin <- cursor ; 
								   				for i=1 to !repeat do draw interp a myframe done
						  | (Scale a)      -> myframe.scale <- a
						  | (Rotate a)     -> myframe.turn <- myframe.turn +. a 
	        			  | (Set a)        -> cursor.x <- a.x ; 
	        			  					  cursor.y <- a.y
						  | (Move a)       -> cursor.x <- cursor.x + a.dx; 
											  cursor.y <- cursor.y + a.dy
						  | (Repeat a)     -> (* in case of a repeat we create a new reference fram *)
								   			  repeat := a 
	        			  | _ 			   -> print_endline "Unknown operation"

	    )
  	 	f.items ;
  	 	interp.cursor <- cursor
;;

(* draw the commands which are stored in the interpreter *)
let display interp frame name  = 
	if name = "" then
		draw interp interp.current frame
	else 
		let metafile = List.assoc( name ) interp.files in
			draw interp metafile frame
;;

(* dump a metafile on the standard output *)
let display_source ?(chan=stdout) f  =   
	let cnt = ref 0 in
		List.iter (fun o ->  
				let operation = match o with
          		  (Line a)      -> sprintf "line %s" (to_string a.right_point)
        		| (Circle a)    -> sprintf "circle %d" a.radius 
        		| (Rectangle a) -> sprintf "rect %s" (to_string a.right_corner)
        		| (Point a)     -> sprintf "plot %s" (to_string a)
        		| (Set a)       -> sprintf "set %s"  (to_string a)
				| (Move a)      -> sprintf "move %d %d" a.dx a.dy 
        		| (Draw a)      -> sprintf "draw %s" a.name
				| (Repeat a)    -> sprintf "repeat %d" a
				| (Rotate a)    -> sprintf "turn %f" (deg_from_rad a)
				| (Scale a)     -> sprintf "scale %f" a 
        		| _             -> sprintf "unknown operation" in				
			  				if chan = stdout then begin
									cnt := !cnt + 1;
									printf "%2.2d %s\n" !cnt operation
									end 
			  				else 
			 					fprintf chan "%s\n" operation
			) 
			f.items 
;;
