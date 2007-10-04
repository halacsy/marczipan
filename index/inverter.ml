module Lex = Mfhash.String

module Make(InvIndexWriter : InvIndex.Writer ) = struct
	
	type inverter = { mutable tokens : int;
	lexicon : (DocList.Collector.t) Lex.t;
	mutable	doc_count : int;				(* number of documents *)
 
	merger : Merger.t;
	max_tokens_in_memory : int;
	stopper : Timem.t;
	index_writer : InvIndexWriter.t;
	dir : string;
	for_index : ForIndex.forIndex_writer
	}

	type doc_handler = { cur_doc : int;
	for_index_doc_handler : ForIndex.doc_handler
	}

	let doc_id dh = dh.cur_doc;;
	
	let start_collection dir max_tokens =
		let stopper = Timem.init () in
		Timem.start stopper "collection";
		Timem.start stopper "run";
		Timem.start stopper "collecting terminfos";
	
		{ 
			tokens = 0;
		lexicon = Lex.create 10;
		doc_count = 0;
		merger = Merger.init dir;
		max_tokens_in_memory = max_tokens;
		stopper = stopper;
		dir = dir;
		index_writer = InvIndexWriter.create dir;
		for_index = ForIndex.start_collection dir
		}

(* call this before adding posting info nincs ellenorizve, hogy novekvo-e  *)
(* a doc_id                                                                *)
	let start_doc ii meta =
		let docid = ii.doc_count in
		ii.doc_count <- succ ii.doc_count;
		{ cur_doc = docid;
		for_index_doc_handler = (ForIndex.start_doc ii.for_index docid meta) }
 
	;;

	let add_term inverter doc_handler term pos =
		let refresh ti =
 
			DocList.Collector.occurrence ti doc_handler.cur_doc pos;
			ti
		in
		let _ =
			Lex.update (fun () -> DocList.Collector.create doc_handler.cur_doc pos) (refresh) inverter.lexicon term in
			inverter.tokens <- succ inverter.tokens;
			ForIndex.add_term inverter.for_index doc_handler.for_index_doc_handler term pos
	;;

	let flush_memory ii =
		if ii.tokens > 0 then
			begin
				Timem.start ii.stopper "flushing";
				let iterator f = Lex.sorted_iter compare f ii.lexicon in
				let _ = Merger.flush ii.merger iterator in
				let t = ii.tokens in
				ii.tokens <- 0;
				Lex.clear ii.lexicon;
				Timem.finish_speed ii.stopper t "tokens";
			end

	let end_doc ii doc_handler =
		ForIndex.end_doc ii.for_index doc_handler.for_index_doc_handler;

(* Sok szivas targya volt, hogy ennek ide kell kerulnie. Ha dokumentum     *)
(* hataron belul lenne flush, akkor terminfo listak atlapolodhatnanak, ami *)
(* bonyolitja az osszefuzest.                                              *)
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
		ForIndex.end_collection ii.for_index;
	(* kell-e merge *)
		if Merger.need_merge ii.merger then begin
			let t = ii.tokens in
			Timem.finish_speed ii.stopper t "tokens";
			flush_memory ii;
			Timem.finish_speed ii.stopper t "tokens";
			Timem.start ii.stopper "merging";
			Merger.merge (InvIndexWriter.write_term_entry ii.index_writer) ii.merger;
			Timem.finish ii.stopper
		end
		else begin
			let t = ii.tokens in
			Timem.finish_speed ii.stopper t "tokens";
			Timem.start ii.stopper "writing terminfos";
			Lex.sorted_iter compare (InvIndexWriter.write_term_entry ii.index_writer) ii.lexicon;
			Timem.finish_speed ii.stopper t "tokens";
			Timem.finish_speed ii.stopper t "tokens"
		end;
		InvIndexWriter.close ii.index_writer ii.doc_count;
		Timem.finish_speed ii.stopper ii.doc_count "documents"
	;;
end