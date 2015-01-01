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

open Drawables;;

let pi = 4.0 *. atan 1.0;;

let rad_from_deg x = 2.0 *. pi *. x /. 360.0 ;;
let deg_from_rad x = 360.0 *. x /. (2.0 *. pi) ;; 	


(* rotate by a given degree *)
let rotate turn p = 
	let x = float_of_int p.x in
		let y = float_of_int p.y in 
			let result = make_point 0 0 in
				result.x <- int_of_float ( (cos turn) *. x -. (sin turn) *. y );
				result.y <- int_of_float ( (sin turn) *. x +. (cos turn) *. y );
				result
;;		
	 
(* scale *)	
let scale s p = 
	make_point (int_of_float (s *. (float_of_int p.x))) 
	           (int_of_float (s *. (float_of_int p.y)))
						
let translation p q =
		let result = make_point 0 0 in
			result.x <- p.x + q.x;
			result.y <- p.y + q.y;
			result
;;

let scale_int x f = int_of_float( f *. (float_of_int x) );;
