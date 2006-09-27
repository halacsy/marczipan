module Lex = Mfhash.Make (Hashlex.HashedString)

type t = {mutable tokens 		: int;
		  lexicon        		: (Terminfo.t) Lex.t;
		  mutable	doc_count 	: int ;				(* number of documents *)
		  mutable	cur_doc  	: int ;               (* the document we are currently processing *)
		
		  mutable temp_files : string  list; 
		  max_tokens_in_memory : int;
		  stopper        : Timem.t;
		  index_writer   : TermIndex.writer;
		  dir            : string
		}
		
	
let start_collection dir max_tokens = 
	let stopper = Timem.init () in
	Timem.start stopper "collection";
	Timem.start stopper "run";
	Timem.start stopper "collecting terminfos";
	
	{tokens = 0; 
	 lexicon = Lex.create 10000; 
	 doc_count = 0;
	 cur_doc = -1; 
	 temp_files = [];  
	 max_tokens_in_memory = max_tokens; 
	 stopper = stopper;
	index_writer = TermIndex.open_writer dir;
	dir = dir }

(* call this before adding posting info
nincs ellenorizve, hogy novekvo-e a doc_id *)
let start_doc ii  = 
		let doc_id = succ ii.cur_doc in
		ii.doc_count <- succ ii.doc_count;
		ii.cur_doc  <- doc_id;
		doc_id
		
;;

let add_term ii term pos =		
		Lex.update ii.lexicon (Terminfo.empty ()) term (fun ti -> Terminfo.occurrence ti ii.cur_doc pos; ti) ;
	    ii.tokens <- succ ii.tokens
;;



		
let write_current_terminfos ii file =
	let oc = open_out_bin file in
	Lex.sorted_iter (Terminfo.write oc) ii.lexicon;
	close_out oc
	
let flush_memory ii =
	if ii.tokens > 0 then
	begin
	Timem.start ii.stopper "flushing";
		let temp_file = "/terminfos.temp." ^ (string_of_int (List.length ii.temp_files)) in
		ii.temp_files <- temp_file :: ii.temp_files ;
		write_current_terminfos ii temp_file;
		let t = ii.tokens in
		ii.tokens <- 0;
		Lex.clear ii.lexicon;
	Timem.finish_speed ii.stopper t "tokens";
	end

let end_doc ii =
		(*Sok szivas targya volt, hogy ennek ide kell kerulnie. Ha dokumentum hataron
			belul lenne flush, akkor terminfo listak atlapolodhatnanak, ami bonyolitja
			az osszefuzest.*)
		if ii.tokens > ii.max_tokens_in_memory then
		begin
			let t = ii.tokens in
			Timem.finish_speed ii.stopper t "tokens";
			flush_memory ii;
			Timem.finish_speed ii.stopper t "tokens";			
			Timem.start ii.stopper "run";
			Timem.start ii.stopper "collecting terminfos";

		end

			
let end_collection ii = 
	(* kell-e merge *)
	if (List.length ii.temp_files) > 0 then begin
		let t = ii.tokens in
		Timem.finish_speed ii.stopper t "tokens";
		flush_memory ii;
		Timem.finish_speed ii.stopper t "tokens";
		Timem.start ii.stopper "merging";
		Merger.merge ii.index_writer ii.temp_files;
		Timem.finish ii.stopper
	end
	else begin
		let  t= ii.tokens in
		Timem.finish_speed ii.stopper t "tokens";
		Timem.start ii.stopper "writing terminfos";	
		write_current_terminfos ii "terminfos.merged";
		Timem.finish_speed ii.stopper t "tokens";
		Timem.finish_speed ii.stopper t "tokens"
	end;
	Timem.finish_speed ii.stopper ii.doc_count "documents"
;;
		
