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

(** This file contains the main loop of the application. *)
open Script;;
open Drawables;;
open Graphics;;
open Genlex;;
open Helper;;
open Files;;
open Printf;;
open String;;
open Filename;;
(* open Unix;; *)
open Sys;;
open Printexc;;
open Transformation;;

exception Session_End;;
exception Unknown_Command;;
exception Syntax_Error ;;
exception File_Not_Existing ;;

(** the session state is stored here *)
type session_state_type = 
{ 
	mutable repeat   : int;
	mutable interp   : interpreter_state_type;
   	mutable active   : bool ; 		    (* command loop is still active *)
   	mutable screen_size_x : int;
   	mutable screen_size_y : int;          
	mutable editor   : string         
};;
  
(* setup the initial state of the application *)
let state = { 
	active = true; 
	repeat = 1;
	interp=make_interpreter();
    screen_size_x = 900;
    screen_size_y = 900;
	editor="gedit" 
};;

let reset_frame f = 
		f.origin <- make_point 0 0;
		f.turn <- 0.0;
		f.scale <- 1.0 
;;

let root_frame = { origin = make_point 0 0; turn=0.0; scale=1.0 } ;;


(** get next token; if there is nothing found we return "" keyword in order 
 *  to simplyfy the parsing of the command line arguments. 
 *)
let next_token t =
  try
    Stream.next t
  with Stream.Failure -> 
    (Kwd "")
;;

(* get a point from the input stream *)
let get_point t =
  let pnt = ( next_token t, next_token t, next_token t) in 
    match pnt with 
        Int x, Kwd "/", Int y -> make_point x y 
			| Kwd "", Kwd "", Kwd "" -> make_point 0 0
      | _ -> raise Syntax_Error 
;;

(* readin a given distance *)
let get_distance t = 
  let dist = next_token t in 
    match dist with 
      Int x ->  x
      | _ -> raise Syntax_Error   
;;

let get_name t =
  let name = next_token t in
  match name with 
    Ident x -> x;
    | _ -> raise Syntax_Error 
;;

        
(** set the cursor to the specified point *)
let do_set t = 
  add state.interp (Set (get_point t));  
;; 

(** move the cursor *)
let do_move t =
	match next_token t, next_token t with
		Int a, Int b ->
				 add state.interp (Move { dx=a; dy=b})
		| _ -> raise Syntax_Error
;;

(** set the turn value *)
let do_turn t = 
		match next_token t with
		  Int a -> add state.interp (Rotate (rad_from_deg (float_of_int a)))
		| Float a -> add state.interp (Rotate (rad_from_deg a))
		| _ -> raise Syntax_Error
;;

let do_scale t = 
		match next_token t with
		  Int a -> add state.interp (Scale (float_of_int a))
		| Float a -> add state.interp (Scale a)
		| _ -> raise Syntax_Error
;;

(** create a rectangle *)
let do_rectangle t = 
  let lx = get_distance t in
  	let ly = get_distance t in 
  add state.interp (Rectangle (make_rectangle lx ly));  
;;

(** create a circle *)
let do_circle t =
  let r = get_distance t in
 	add state.interp (Circle (make_circle r));;      

(** draw a line *)
let do_line t = 
  let p = get_point t in
  	add state.interp (Line (make_line p.x p.y));;      

(** store the current a meta file with a given name and start a new one *)
let do_store t = 
  let name = get_name t in
  	store state.interp name 
;;

(** clear the screen and the current metafile *)
let do_clear t =
	clear_graph () ;
	root_frame.turn <- 0.0;
	root_frame.scale <- 1.0;
	root_frame.origin <- make_point 0 0;
	set_cursor state.interp (make_point 0 0) ;
	clear_metafile state.interp
;;

(** draw either the current matafile or a names metafile *)
let do_draw t =
  match next_token t, next_token t, next_token t with
      Int x, Kwd "/", Int y -> display state.interp root_frame ""
    | Ident name, _ , _ -> add state.interp (Draw (get_metafile state.interp name))
    | _ -> display state.interp root_frame ""
;;  


(** set the screen dimensions *)
let do_resize x y = 
  state.screen_size_x <- x;
  state.screen_size_y <- y;
  resize_window state.screen_size_x state.screen_size_y 
;;
  
(** resize the visible canvas *)
let do_screen t =
  match Stream.next t, Stream.next t with
    Int x, Int y -> do_resize x y
    | Int x, _ -> do_resize x state.screen_size_y
    | _ -> raise Syntax_Error
;;

(** list the avaiable metafiles *)
let do_list t =
  match next_token t with
    | Ident a -> display_source (get_metafile state.interp a) 
    | _ -> display_source (get_metafile state.interp "") 
;; 

let do_info t = 
	printf " Scale=%f, Turn=%f, origin=%s, screen_size=%dx%d \n" 
							root_frame.scale root_frame.turn (Drawables.to_string root_frame.origin)
							state.screen_size_x state.screen_size_y
;;

(** Save the given metafile f into file with the name f.mf *) 
let do_save t =
	match next_token t with
		| Ident name -> let f = get_metafile state.interp name in
											let chn = open_out (name ^ ".mf") in
												display_source ~chan:chn f;
												close_out chn;
		| _ -> raise Syntax_Error
;;

(** the list of commands *)
let lexer = make_lexer [ 
			(* grafical commands *)
			"rect"; "circle"; "line"; 
			"quit"; "set" ; "move"; "turn"; "scale" ; 

			(* online dialog *)
			"draw"; "!"; "?" ;
			"save"; "load"; "edit";
			(* screen related command  *)
			"screen"; "clear" ; 
			(* metafile commands *)
			"store"; "dir"; "list";
			
			(* argument elements *)
			"/"; ","; 			
			 ]
;;

(** interpret a command and execute it *)
let rec execute_command token =
    match (Stream.next token) with
			(* cursor movements *)
        	  Kwd "move"   -> do_move token
			| Kwd "set"    -> do_set token				
			| Kwd "turn"   -> do_turn token
			| Kwd "scale"  -> do_scale token 
			(* grafical object *)
      		| Kwd "rect"   -> do_rectangle token
      		| Kwd "circle" -> do_circle token
      		| Kwd "line"   -> do_line token        
      		| Kwd "draw"   -> do_draw token
			| Int value    -> add state.interp (Repeat value); execute_command token
			(* management functions *)												
			| Kwd "clear"  -> do_clear token
      		| Kwd "quit"   -> raise Session_End
      		| Kwd "store"  -> do_store token      
      		| Kwd "screen" -> do_screen token		 
			| Kwd "dir"    -> list_metafiles state.interp
			| Kwd "list"   -> do_list token
			| Kwd "!"      -> do_batch (get_name token)
			| Kwd "save"   -> do_save token
			| Kwd "load"   -> do_load token
			| Kwd "edit"   -> do_edit token
			| Kwd "?"      -> do_info token
			(* if no keyword is matching, use the identifer as metafile name *)
      		| Ident name   ->	( try add state.interp (Draw (get_metafile state.interp name))
					 								with Metafile_Not_Found -> raise Unknown_Command )
			(* if nothing matches *)		
		  	| _ ->  raise Unknown_Command
and do_batch name = 
		let chan = open_in name in
		try 	
			while true do		
   			let cmd = input_line chan in
					if (length cmd) > 0 then if (get cmd 0) != '#' then begin				
						let token = lexer (Stream.of_string cmd) in
							execute_command token;
					end 
			done
		with End_of_file -> close_in chan		
		
and do_edit t =
		match next_token t with
			Ident name -> 
				let f = get_metafile state.interp name in
				let tmp = temp_file name ".mf" in
				let chn = open_out tmp in
					display_source ~chan:chn f;
					close_out chn;
					let result = command ("vi " ^ tmp) in begin
						if( result != 0 ) then
						    printf "Error ***\n";
					end 
			| _ -> raise Syntax_Error

and do_load t =
	let name = get_name t in 
		do_clear t;
		do_batch (name ^ ".mf");
		store state.interp name 
;;

(** toplevel starts here *)
open_graph "";;

let cfg : config_type = make_config_type ;;

load_config "spiro.cfg" cfg ;;

state.screen_size_x = config_int "screen_size_x" cfg;;
state.screen_size_y = config_int "screen_size_y" cfg;;

resize_window state.screen_size_x state.screen_size_y ;;

set_window_title "Spiro";;

printf "Spiro\n";;
printf "Copyright 2014,2015 Michael Erdmann (michael.erdmann@snafu.de)\n\n";;

while state.active do
	try 
   		begin   
			reset_frame root_frame; 
			display state.interp root_frame "";
     		print_point(get_cursor state.interp);
    		print_string " ? ";  
    		let cmd = read_line () in
				if (length cmd)>0 then
  			   		let token = lexer (Stream.of_string cmd) in
    					execute_command token;         
  		end 
	with x ->
		match x with
     	 	Session_End -> 
         		print_endline "done.";
       			state.active <- false;
       		| File_Not_Existing -> print_endline "Error: metafile not exiting ****"
       		| Unknown_Command -> print_endline "Error: unkown command ****"
       		| Syntax_Error -> print_endline "Error: Syntactical Error ****" 
			| Metafile_Not_Found -> printf "Metafile not found ****\n"      
       		| _ as exn -> printf "Exception %s *******\n" (Printexc.to_string exn )
done ;;

