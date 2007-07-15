open Expat


let _ =
let p = parser_create None in



let silent = ref true in

let start_element elt attr = match elt with
	| "embedobject" -> silent := true
	| "cim" | "bevezeto" | "test" -> silent := false 
	| _ -> ()
in


let end_element elt = match elt with
	| "embedobject" -> silent := false
	| "cim" | "bevezeto" | "test" -> silent := true; print_newline ()
	| _ -> ()
in

let character str =  
	if not !silent then
		print_endline str
in
	

set_start_element_handler p start_element;
set_end_element_handler p end_element;
set_character_data_handler p character;

try
while true do
let line = input_line stdin in
parse p line;
done
with End_of_file -> print_endline "finished"