

(** readin all lines from the given chanel *)
let process_lines chan exec = 
	try 	
		while true do		
   		begin     		
     		let cmd = input_line chan in
    			exec cmd;         
  		end 
		done
	with End_of_file -> close_in chan
									
;;	