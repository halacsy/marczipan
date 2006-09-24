module Lex = Mfhash.Make (Hashlex.HashedString)

type t = {mutable tokens : int;
		  lexicon        : (Terminfo.t) Lex.t;
		  mutable temp_files : string  list; 
		  max_tokens_in_memory : int;
		}
		
	
let empty () = {tokens = 0; lexicon = Lex.create 100000; temp_files = [];  max_tokens_in_memory = 3}
	
let number_of_tokens ii = ii.tokens

let number_of_types ii = Lex.size ii.lexicon

let flush_memory ii =
	let t = Timem.init () in
	Timem.start t "flushing";
	let temp_file = "terminfos.temp." ^ (string_of_int (List.length ii.temp_files)) in
	ii.temp_files <- temp_file :: ii.temp_files ;
	let oc = open_out_bin temp_file in
	let write term terminfo =
		Terminfo.write oc term terminfo
	in
	Lex.sorted_iter write ii.lexicon;
	close_out oc;
	ii.tokens <- 0;
	Lex.clear ii.lexicon;
	Timem.stop t	
	
let add_term_accurance ii doc term pos =
		if ii.tokens = ii.max_tokens_in_memory then
			flush_memory ii;
			
		Lex.update ii.lexicon (Terminfo.empty ()) term (fun ti -> Terminfo.occurrence ti doc pos; ti) ;
	    ii.tokens <- succ ii.tokens

let end_collection ii = flush_memory ii;
