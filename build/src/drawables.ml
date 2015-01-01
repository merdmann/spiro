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

(* All drawable objects are drawn relative to the orignin of the coordinate
   system.
 *)
open Printf;;

type point_type = { mutable x : int; mutable y : int } ;;
type rectangle_type = { right_corner : point_type };;
type line_type = { right_point : point_type } ;;
type circle_type = {  radius : int };;
type file_type = { name : string };;
type move_type = { dx : int; dy : int };;

let make_point x0 y0 = ({x=x0; y=y0} : point_type) ;;
let make_rectangle x0 y0 = ( { right_corner = make_point x0 y0 } : rectangle_type );; 
let make_line x0 y0 = ( { right_point = make_point x0 y0 } : line_type );; 
let make_circle r = ( { radius = r } : circle_type );; 
let make_file file_name = { name=file_name } ;;
  
(* print a point *)
let to_string p = sprintf "[ %d/%d ]" p.x p.y ;;
let print_point p =	printf "%s" (to_string p);;


