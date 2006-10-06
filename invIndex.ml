module Lex = Mfhash.Make (Hashlex.HashedString)

(** a term lookup table-t kezeli *)
type writer = {doclist_oc : out_channel;
			   lexicon_writer : Lexicon.Writer.t
}

type term_record = (int * int * Int64.t) 

type reader = {doclist_ic : in_channel;
			   lexicon      : Lexicon.t;
			   mutable tokens       : int;
				stopper     : Timem.t
}


let open_writer index_dir =
	{doclist_oc = open_out_bin (index_dir ^ "/" ^ "postings");
	lexicon_writer = Lexicon.Writer.create index_dir;
	} 

let write_term_entry w term terminfo =
	flush w.doclist_oc;
	let pos = LargeFile.pos_out w.doclist_oc in
	let df = DocList.Collector.df terminfo in
	let tf = DocList.Collector.tf terminfo in
	DocList.write w.doclist_oc (DocList.Collector.doclist terminfo);
	Lexicon.Writer.add w.lexicon_writer term df tf pos;
	
;;

let close_writer w =
	close_out w.doclist_oc;
	Lexicon.Writer.close w.lexicon_writer;
;;	



let open_reader index_dir = 
	let reader = {doclist_ic = open_in_bin (index_dir ^ "/" ^ "postings");
				  lexicon = Lexicon.init index_dir ;
				   tokens  = 0;
				  stopper = Timem.init ();
	} in
	reader
;;


	
	
let tokens reader = reader.tokens;;
	
let term_info reader term =
	let (df, tf, pos) = Lexicon.find reader.lexicon term in
	let open_stream () =
		Timem.start reader.stopper "loading postings";
		LargeFile.seek_in reader.doclist_ic  pos;
		let doclist = DocList.read reader.doclist_ic  df in
		Timem.finish reader.stopper ;
    	DocList.open_stream doclist
	in
	(df, tf, open_stream)
	
(*let pretty_print reader =
	let aux term (df, tf, pos) =
		Printf.printf "%s -> " term;
	    Printf.printf "df = %d tf = %d\n " df tf;
		LargeFile.seek_in reader.doclist_ic  pos;
		let doclist = DocList.read reader.doclist_ic  df in
		DocList.pretty_print doclist ; 
	in
	Lex.sorted_iter  aux reader.lexicon
*)