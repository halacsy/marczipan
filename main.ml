

type inv_index_state = { 
   						mutable	doc_count : int ;				(* number of documents *)
						mutable	last_doc  : int ;               (* the document we are currently processing *)
  						mutable	terminfos : InvIndex.t ;		(* the inverted index, aka terminfo table *)
						mutable	doc_writer: Docinfo.writer 	(* the writer of the doc_info records *)
  }
					
(* token, amit a hivonak visszaadunk, hogy erre hivatkozzon, egy-egy doc feldolgozasa kozben *)
type doc_term_indexer =   {inv_index : inv_index_state ;
					       mutable doc_info  : Docinfo.doc_info} 

let start = {
				doc_count  = 0;
			 	last_doc   = -1;
				terminfos  = (InvIndex.empty ());
				doc_writer = Docinfo.create_writer "index";
			}
			
			
(* call this before adding posting info
   nincs ellenorizve, hogy novekvo-e a doc_id *)
let start_doc iis  = 
	let doc_id = succ iis.last_doc in
	iis.doc_count <- succ iis.doc_count;
	iis.last_doc  <- doc_id;
	{inv_index = iis ;
	 doc_info  = Docinfo.empty doc_id}

	
let end_doc dti = 
		let iis = dti.inv_index in
		iis.doc_writer <- Docinfo.write iis.doc_writer dti.doc_info 
		
		
let add_term dti term pos =
	let iis = dti.inv_index in
	iis.terminfos <- InvIndex.add_term_accurance iis.terminfos iis.last_doc term pos;
	dti.doc_info  <- Docinfo.add_term_accurance dti.doc_info term pos 

let end_run iis = 
	let t = Timem.init () in
	Timem.start t "iterating";
	let 	(s, lex, postings, positions) = InvIndex.iterate_over iis.terminfos in
		Printf.printf "calculated size is %d\n" s;
			Printf.printf "terms %d postings %d positions %d \n" lex postings positions ;
	Timem.stop t;
	
	Timem.start t "writing inx";
	let out = open_out_bin "index.inx" in
	InvIndex.write out iis.terminfos ;
	close_out out ;
	Timem.stop t;
	
	Timem.start t "reading inx";
	let inp = open_in_bin "index.inx" in
	let iix = InvIndex.read inp in
	Timem.stop t ;
	let 	(s2, lex2, postings2, positions2) = InvIndex.iterate_over iix in
		Printf.printf "backed up calculated size is %d\n" s2;
			Printf.printf "terms %d postings %d positions %d \n" lex2 postings2 positions2 ;
			
	Printf.printf "backed number of tokens:    %d\n" (InvIndex.number_of_tokens iix);
	Printf.printf "backed number of types:     %d\n" (InvIndex.number_of_types iix)
	
	
let pretty_print   iis  =
	Printf.printf "number of documents: %d\n" iis.doc_count;
	Printf.printf "number of tokens:    %d\n" (InvIndex.number_of_tokens iis.terminfos);
	Printf.printf "number of types:     %d\n" (InvIndex.number_of_types iis.terminfos)
(*;	InvIndex.pretty_print iis.terminfos *)


 
let proc_sentence invi sentence =
		let dti = start_doc invi in
        let n = List.fold_left (fun i (word, gold)  ->  add_term  dti word  i ;  (succ i)) (0) sentence in
	    end_doc  dti

let invi = start
	
let _ = 
	let t = Timem.init () in
	Timem.start t "indexing";			
Io.iter_sentence stdin ( proc_sentence invi ) ;
	Timem.stop t ;
end_run invi;			
pretty_print invi
		
