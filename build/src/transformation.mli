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


val rad_from_deg : float -> float
val deg_from_rad : float -> float

val rotate :  float -> point_type -> point_type  
val scale : float -> point_type -> point_type
val translation : point_type -> point_type -> point_type 
val scale_int : int -> float -> int 