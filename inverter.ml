module Lex = Mfhash.Make (Hashlex.HashedString)

type posting =  {doc_id : int;
				 mutable freq : int;
				 positions  : (int) Varray.t}


type terminfo = {mutable df: int;
				 mutable tf : int;
				 mutable postings :  posting list}
				

type t = {mutable tokens : int;
		  lexicon        : (terminfo) Lex.t}
		
let new_posting doc_id pos = 
	let t = {doc_id = doc_id; freq = 1; positions = Varray.create 2 0} in
	Varray.add t.positions pos;
	t
	
let empty_postinglist = [] 
	
let empty_terminfo = {df = 0; tf = 0; postings = empty_postinglist}
	
let empty = {tokens = 0; lexicon = Lex.create 100000}
	
let number_of_tokens ii = ii.tokens

let number_of_types ii = Lex.size ii.lexicon
	
let add_term_accurance ii doc term pos =		
		let add_term_accurance  doc pos terminfo =  
		
			terminfo.tf <- terminfo.tf;
			let _ = 
			match terminfo.postings with
				| posting :: tail when posting.doc_id == doc -> 
					
					posting.freq <- succ posting.freq;
					Varray.add posting.positions pos 
				
                | _ -> 	
	 				terminfo.df <- (succ terminfo.df) ;
					terminfo.postings <- ((new_posting doc pos) :: terminfo.postings) 
			in
			terminfo

		in
		Lex.update ii.lexicon empty_terminfo term (add_term_accurance doc pos) ;
		 ii.tokens <- succ ii.tokens;
		ii

