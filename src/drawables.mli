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

(* All drawable objects are drawn relative to the origin of the coordinate
   system.
 *)
type point_type = { mutable x : int; mutable y : int } 
type rectangle_type = { right_corner : point_type }
type line_type = { right_point : point_type } 
type circle_type = {  radius : int }
type file_type = { name : string }
type move_type = { dx : int; dy : int }
  
val make_point : int -> int -> point_type 
val make_rectangle : int -> int -> rectangle_type
val make_circle : int -> circle_type   
val make_line : int -> int -> line_type
val print_point : point_type -> unit 
val to_string : point_type -> string

