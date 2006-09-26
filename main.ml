

type inv_index_state = { 
   						mutable	doc_count : int ;				(* number of documents *)
						mutable	last_doc  : int ;               (* the document we are currently processing *)
  						mutable	terminfos : InvIndex.t ;		(* the inverted index, aka terminfo table *)
						mutable	doc_writer: Docinfo.writer 	(* the writer of the doc_info records *)
  }
					
(* token, amit a hivonak visszaadunk, hogy erre hivatkozzon, egy-egy doc feldolgozasa kozben *)
type doc_term_indexer =   {inv_index : inv_index_state ;
					       mutable doc_info  : Docinfo.doc_info} 

let start max_tokens = {
				doc_count  = 0;
			 	last_doc   = -1;
				terminfos  = (InvIndex.empty max_tokens );
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
		iis.doc_writer <- Docinfo.write iis.doc_writer dti.doc_info;
		InvIndex.end_doc iis.terminfos 
		
		
let add_term dti term pos =
	let iis = dti.inv_index in
	InvIndex.add_term_accurance iis.terminfos iis.last_doc term pos;
	dti.doc_info  <- Docinfo.add_term_accurance dti.doc_info term pos 

let end_run iis = 
	InvIndex.end_collection iis.terminfos;
	Printf.printf "merged\n"; 
	Merger.pretty_print_stream "terminfos.merged" 
	
	
let pretty_print   iis  =
	Printf.printf "number of documents: %d\n" iis.doc_count;
	Printf.printf "number of tokens:    %d\n" (InvIndex.number_of_tokens iis.terminfos);
	Printf.printf "number of types:     %d\n" (InvIndex.number_of_types iis.terminfos)
	

 
let proc_sentence invi sentence =
		let dti = start_doc invi in
        let n = List.fold_left (fun i (word, gold)  ->  add_term  dti word  i ;  (succ i)) (0) sentence in
	    end_doc  dti


	
let _ = 
	let limit = int_of_string Sys.argv.(1) in
	let invi = start limit in
	Printf.eprintf "token limit = %d\n" limit;
	let t = Timem.init () in
	Timem.start t "indexing";			
	Timem.start t "inverting runs";
	Io.iter_sentence stdin ( proc_sentence invi ) ;
	Timem.stop t;
	end_run invi;
	Timem.stop t ;			
		
