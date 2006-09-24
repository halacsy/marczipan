module Lex = Mfhash.Make (Hashlex.HashedString)

type t = {mutable tokens : int;
		  lexicon        : (Terminfo.t) Lex.t;
		  mutable temp_files : string  list; 
		  max_tokens_in_memory : int;
		}
		
	
let empty max_tokens = {tokens = 0; lexicon = Lex.create 10; temp_files = [];  max_tokens_in_memory = max_tokens }
	
let number_of_tokens ii = ii.tokens

let number_of_types ii = Lex.size ii.lexicon

let write_current_terminfos ii file =
	let oc = open_out_bin file in
	Lex.sorted_iter (Terminfo.write oc) ii.lexicon;
	close_out oc
	
let flush_memory ii =
	if ii.tokens > 0 then
	let t = Timem.init () in
	Timem.start t "flushing";
	let temp_file = "terminfos.temp." ^ (string_of_int (List.length ii.temp_files)) in
	ii.temp_files <- temp_file :: ii.temp_files ;
	write_current_terminfos ii temp_file;
	ii.tokens <- 0;
	Lex.clear ii.lexicon;
	Timem.stop t	
	
let add_term_accurance ii doc term pos =		
		Lex.update ii.lexicon (Terminfo.empty ()) term (fun ti -> Terminfo.occurrence ti doc pos; ti) ;
	    ii.tokens <- succ ii.tokens

let end_doc ii =
		(*Sok szivas targya volt, hogy ennek ide kell kerulnie. Ha dokumentum hataron
			belul lenne flush, akkor terminfo listak atlapolodhatnanak, ami bonyolitja
			az osszefuzest.*)
		if ii.tokens > ii.max_tokens_in_memory then
			flush_memory ii

			
let end_collection ii = 
	(* kell-e merge *)
	if (List.length ii.temp_files) > 0 then
		let t = Timem.init () in
		flush_memory ii;
		Timem.start t "merging";
		Merger.merge ii.temp_files;
		Timem.stop t
	else
	
		write_current_terminfos ii "terminfos.merged"
