type start_element_handler = (string -> (string * string) list -> unit) 

type end_element_handler = (string -> unit)

type parser = { mutable starth : start_element_handler;
                mutable endh   : end_element_handler;
                mutable datah  : (string -> unit)}
                 
let donothing_start_element_handler name args = ()
  ;;
  
let donothing_end_element_handler name  = ()
  ;;
  
let donothing_data_handler s = ()
  ;;
  
let create () = {
  starth = donothing_start_element_handler;
  endh  = donothing_end_element_handler;
  datah = donothing_data_handler;
  }
;;
     
let set_start_element_handler parser f = parser.starth <- f
  ;;

let reset_start_element_handler parser = parser.starth <- donothing_start_element_handler
  ;;
    
let set_end_element_handler parser f = parser.endh <- f
  ;;           

let reset_end_element_handler parser = parser.endh <- donothing_end_element_handler
  ;;

let set_character_data_handler parser f = parser.datah <- f
  ;;
  
let reset_character_data_handler parser  = parser.datah <- donothing_data_handler
 ;;
  


let parse_lexbuf parser lexbuf = 
  let empty_list = [] in
    
  try
  while (true) do
    match Xml_tokenizer.tokenize lexbuf with
       Xml_tokenizer.StartDoc(version, encoding) -> ()
     | Xml_tokenizer.StartE(name, _, _) -> parser.starth name empty_list
     | Xml_tokenizer.EndE(name, _, _)  -> parser.endh name
     | Xml_tokenizer.Data(s,_,_)       -> parser.datah s
  done;
  with End_of_file -> ()
;;  

let parse_string parser s =
  let lexbuf = Lexing.from_string s in
  parse_lexbuf parser lexbuf
;;

let parse_channel parser ic = 
  let lexbuf = Lexing.from_channel ic in
  parse_lexbuf parser lexbuf
;;
