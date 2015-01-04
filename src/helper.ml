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

(* This is a collection of more or less important helper functions *)

open Genlex;;

type config_type = 
{
   mutable items : (string*string) list 
};;

let make_config_type : config_type = { items = [( "", "" )] } ;;

(** load the given configuration file *)
let load_config name cfg =
  let lexer = make_lexer [ "=" ] in
  let s = open_in name in
  try 
    while true do
      let line = input_line s in
      if String.get line 0 != '#' then
      	let t = lexer(Stream.of_string line) in
      		match Stream.next t, Stream.next t, Stream.next t with 
        	    Ident x, Kwd "=", Ident y ->  cfg.items <- (x,y) :: cfg.items 
            | Ident x, Kwd "=", Int y ->  cfg.items <- (x,(string_of_int y)) :: cfg.items 
        		| _ -> print_endline "Error;"
    done
  with End_of_file -> close_in s
;;

(** get a configuration parameter value as string *)
let config_value name cfg = List.assoc name cfg.items ;;
(** get a configuration parameter value as integer *)
let config_int name cfg = int_of_string (config_value name cfg);
  


  

