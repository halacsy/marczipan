(* post-hoc filter for ocamorph
   compile: ocamlc -o ocafilter ocafilter.ml 
*)

(* Split a string into a list of substrings based on a delimiter character *)
let split c str = 
  let rec aux s acc = 
    try  let ind=String.index s c in
         aux (String.sub s (ind+1) ((String.length s) - ind -1 )) 
              ((String.sub s 0 ind)::acc)       
    with Not_found -> List.rev (s::acc) 
  in aux str []

in


	
let get_pos tag = match split '/' tag with
	pos :: _ -> pos
   | _ -> ""
in

let get_lemma anal = match split '/' anal with
	l :: _ -> String.lowercase l
	| _ ->  String.lowercase anal
in

let process_phrase lex phrase  = 
	Ngramtree.add lex phrase
	
in
let aline  (word, tag, anal) (window, tree) =
	let pos = get_pos tag in
	match pos with	
	 	"ADJ"  -> ( (get_lemma word ) :: (window) , tree)
 	 |  "NOUN" when List.length window > 0 -> ([], (process_phrase tree ((get_lemma word )::window)) )
	 | _ -> ([], tree)
in

let fold_lines f init chan =
   let res = ref init in
   (try while true do
      res  := f !res (input_line chan)
   done with End_of_file -> ());
   !res
in

let aaline (window, tree) line = 
	
	match (split '\t' line) with
		word :: tag :: _ :: anal -> aline (word, tag, anal) (window,tree)
		| _ -> ([], tree)
		
in
let (_, tree) = fold_lines aaline ([], Ngramtree.empty) stdin in

Ngramtree.print tree ;