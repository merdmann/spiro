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
type config_type = 
{
   mutable items : (string*string) list;               
};;

val make_config_type : config_type
  
val load_config : string -> config_type -> unit 
val config_value : string -> config_type -> string
val config_int : string -> config_type -> int
  
