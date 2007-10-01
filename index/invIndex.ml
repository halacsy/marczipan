module Lex = Mfhash.String
module type Lexicon = sig
	type t
	val init : string -> t
	val find : t -> string -> int * int * int64

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
	val close : t -> unit
	val write_term_entry : t -> string -> DocList.Collector.t -> unit
end

	module type Reader  = sig
		type t
		(** opens a reader  *)
		val open_reader : string -> t
		val tokens : t -> int
		val term_info :
		t -> string -> int * int * (unit -> DocList.stream)
 
	end
	
 
(* TODO, S-t bevenni *)
module type S =
	functor (TermLexicon : Lexicon) ->
	sig
	module Writer : Writer
	end

module Make (TermLexicon : Lexicon ) = struct

	module Writer : Writer = struct
(** a term lookup table-t kezeli *)
		type t =
			{
				doclist_oc : out_channel;
				lexicon_writer : TermLexicon.Writer.t
			}

		let create index_dir =
			{
				doclist_oc = open_out_bin (index_dir ^ "/" ^ "postings");
				lexicon_writer = TermLexicon.Writer.create index_dir;
			}

		let write_term_entry w term terminfo =
			flush w.doclist_oc;
			let pos = LargeFile.pos_out w.doclist_oc in
			let df = DocList.Collector.df terminfo in
			let tf = DocList.Collector.tf terminfo in
			DocList.write w.doclist_oc (DocList.Collector.doclist terminfo);
			TermLexicon.Writer.add w.lexicon_writer term df tf pos;
	
		;;

		let close w =
			close_out w.doclist_oc;
			TermLexicon.Writer.close w.lexicon_writer;
		;;

	end

	module Reader : Reader = struct
	
		type t =
			{
				doclist_ic : in_channel;
				lexicon : TermLexicon.t;
				mutable tokens : int;
				stopper : Timem.t
			}

		let open_reader index_dir =
			let reader = { doclist_ic = open_in_bin (index_dir ^ "/" ^ "postings");
			lexicon = TermLexicon.init index_dir;
			tokens = 0;
			stopper = Timem.init ();
			} in
			reader
		;;

		let tokens reader = reader.tokens;;
	
		let term_info reader term =
			let (df, tf, pos) = TermLexicon.find reader.lexicon term in
			let open_stream () =
				Timem.start reader.stopper "loading postings";
				LargeFile.seek_in reader.doclist_ic pos;
				let doclist = DocList.read reader.doclist_ic df in
				Timem.finish reader.stopper;
				DocList.open_stream doclist
			in
			(df, tf, open_stream)
	
	end

	end