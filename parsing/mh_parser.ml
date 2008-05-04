(* reads mh docs from ic and calls f with every doc *)
  
let parse ic f =

  let id = ref "" in
  let in_id = ref true in
  let buffer = Buffer.create 4096 in
    
  let start_element name args =
    match name with
      "DOC"     -> Buffer.clear buffer
      | "DOCID" -> in_id := true;
      | _ -> ()
    
  in
  
  let end_element name = 
    match name with
      "DOC"     -> f !id (Buffer.contents buffer)
      | "DOCID" -> in_id := false;
      | _ -> ()
  in
  
  let data s =
    if !in_id then
      id := s
    else
      Buffer.add_string buffer s
  in
  
  let parser = Xml_parser.create () in
  Xml_parser.set_start_element_handler parser start_element;
  Xml_parser.set_end_element_handler parser end_element;
  
  Xml_parser.set_character_data_handler parser data;
  Xml_parser.parse_channel parser ic
;;
 


