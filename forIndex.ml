module Lex = Mfhash.Make (Hashlex.HashedString)

let default_type_freqs_size = 10

(** pointer to the term list *)
type ptr_terms_t = int

(** pointer to the meta fields *)
type ptr_meta_t = int 


type doc_info = {types : int;
				 len   : int;
				 ptr_terms : ptr_terms_t;
				 ptr_meta  : ptr_meta_t }
				
(* data to be collected about a document during indexing *)
type doc_info_writer =  {doc_id : int ; (* doc_id *)
 						 meta : docMeta.t;
     			         type_freqs:	(int) Lex.t ; (*type_freqs :*) 
				         mutable tokens :	int   (* tokens*)				
				}

let start_collection indexdir =
	
let empty doc_id = {doc_id = doc_id ; type_freqs = Lex.create default_type_freqs_size; tokens = 0 }

let add_term_accurance di term pos =  
	Lex.update di.type_freqs 0 term (succ) ;
    di.tokens = (succ di.tokens);
	di

	
type writer = {base : string ; out : out_channel }

let create_writer base = {base = base ; out = open_out_bin base}
	
let write out di = 
		 Printf.fprintf out.out "%d %d %d\n" di.doc_id   (Lex.size di.type_freqs) di.tokens  ;
		out 
