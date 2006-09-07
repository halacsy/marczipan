module Lex = Mfhash.Make (Hashlex.HashedString)

let default_type_freqs_size = 10

(* data to be collected about a document during indexing *)
type doc_info =  DocInfo of int * (* doc_id *) 		
     				 		(int) Lex.t * (*type_freqs :*) 
							int   (* tokens*)				
				

let empty doc_id = DocInfo(doc_id, Lex.create default_type_freqs_size, 2)

let add_term_accurance (DocInfo(doc_id, type_freqs, tokens)) term pos =  
	Lex.update type_freqs 1 term (succ) ;
    DocInfo(doc_id, type_freqs, (succ tokens))

	
type writer = {base : string ; out : out_channel }

let create_writer base = {base = base ; out = open_out_bin base}
	
let write out = function
	| DocInfo(doc_id, type_freqs, tokens) ->
		 Printf.fprintf out.out "%d %d %d\n" doc_id   (Lex.size type_freqs) tokens  ;
		out 
	