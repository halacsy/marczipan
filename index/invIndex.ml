module Lex = Mfhash.String
module type Lexicon = sig
	type t
	val init : string -> t
	val find : t -> string -> int * int * int64
	val iter : (char list -> int -> int -> int -> int64 -> unit) -> t -> unit
	val index : t -> string -> int
	module Writer :
  sig
		type t
		val create : string -> t
		val close : t -> unit
		val add : t -> string -> int -> int -> int64 -> unit
  end
end
 
module type Writer = sig
	type t
	val create : string -> t
	val close : t -> int -> unit
	val write_term_entry : t -> string -> DocList.Collector.t -> unit
end

module type Reader = sig
	type t
	type lexicon_t
		(** opens a reader  *)
	val open_reader : string -> t
	(* string-hez visszaadja df-t, tf-t es egy ()-> fuggvenyt, ami egy fuggvenyt ad vissza, amit ()-ra olvas egy
	   doc_id, tf part
	*)
	val term_info : t -> string -> int * int * (unit ->(unit -> (int*int)))
	
	val doc_count : t -> int
	val token_count : t -> int
	val type_count : t-> int
	val lexicon : t -> lexicon_t
	
	val iter_over_terms : (char list -> int -> int -> int -> (unit -> DocList.stream) -> unit ) -> t -> unit
 
  val term2index : t -> string -> int
  
  val find_term : t -> string -> int * int * int64
end
	
type collection_statistics = {
	doc_count : int;
	token_count : int;
	type_count : int;
}
	
(* TODO, S-t bevenni *)
module type S =
	sig
	  module Writer : Writer
	  module Reader : Reader
	end

module Make (TermLexicon : Lexicon ) : S = struct

	module Writer : Writer = struct
  (** a term lookup table-t kezeli *)
		type t =
			{
				doclist_oc : out_channel;
				metadata_oc : out_channel;
				lexicon_writer : TermLexicon.Writer.t;
				mutable distinct_types : int; (* hany kulonbozo szo van a lexikonban *)
				mutable total_tokens : int; (* hany tokenbol all a korpusz *)
			}

		let create index_dir =
			{
				doclist_oc = open_out_bin (index_dir ^ "/" ^ "postings");
				metadata_oc = open_out_bin (index_dir ^ "/" ^ "index.meta");
				lexicon_writer = TermLexicon.Writer.create index_dir;
				distinct_types = 0;
				total_tokens = 0;
			}

		let write_term_entry w term terminfo =
			flush w.doclist_oc;
			let pos = LargeFile.pos_out w.doclist_oc in
			let df = DocList.Collector.df terminfo in
			let tf = DocList.Collector.tf terminfo in
			DocList.write w.doclist_oc (DocList.Collector.doclist terminfo);
			TermLexicon.Writer.add w.lexicon_writer term df tf pos;
	
			w.distinct_types <- w.distinct_types + 1;
			w.total_tokens <- w.total_tokens + tf;
		;;

		let close w doc_count =
			Printf.fprintf w.metadata_oc "documents %d\n" doc_count;
			Printf.fprintf w.metadata_oc "types %d\n" w.distinct_types ;
			Printf.fprintf w.metadata_oc "tokens %d\n"  w.total_tokens;
			close_out w.metadata_oc;
			close_out w.doclist_oc;
			TermLexicon.Writer.close w.lexicon_writer;
		;;

	end

	module Reader : Reader   = struct
	  type lexicon_t = TermLexicon.t
		type t =
			{
				doclist_ic : in_channel;
				lexicon : lexicon_t;
				mutable tokens : int;
				stopper : Timem.t;
				statistics : collection_statistics;
			}

		let open_reader index_dir =
			
			let ic = open_in_bin (index_dir ^ "/" ^ "index.meta") in
			let doc_count = Scanf.sscanf (input_line ic) "documents %d" (fun a -> a) in
			let types = Scanf.sscanf (input_line ic) "types %d" (fun a -> a) in
			let tokens = Scanf.sscanf (input_line ic) "tokens %d" (fun a -> a) in
			let statistics = 
				{doc_count = doc_count;
				 token_count = tokens;
				 type_count = types;
				}
			in
			let reader = { doclist_ic = open_in_bin (index_dir ^ "/" ^ "postings");
			lexicon = TermLexicon.init index_dir;
			tokens = 0;
			stopper = Timem.init ();
			statistics = statistics;
			} in
			reader
		;;

		let token_count reader = reader.statistics.token_count;;
		let doc_count reader = reader.statistics.token_count;;
	
		let type_count reader = reader.statistics.token_count;;
	
		let term_info reader term =
			let (df, tf, pos) = TermLexicon.find reader.lexicon term in
			let open_stream () =
				LargeFile.seek_in reader.doclist_ic pos;
				let doclist = DocList.read reader.doclist_ic df in
				let stream = DocList.open_stream doclist in
				let next () = DocList.next_doc stream  in
				next
			in
			(df, tf, open_stream)
	
	  let  iter_over_terms f reader =
	    let aux term ix  df tf pos =
	      let open_stream () =
  				LargeFile.seek_in reader.doclist_ic pos;
  				let doclist = DocList.read reader.doclist_ic df in
  				DocList.open_stream doclist
  			in
  			f term ix df tf open_stream
      in
      TermLexicon.iter aux reader.lexicon
    
    let lexicon reader = reader.lexicon
    
    let term2index reader  = TermLexicon.index reader.lexicon
    let find_term reader = TermLexicon.find reader.lexicon
	end

	end