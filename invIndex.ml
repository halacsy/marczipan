module Lex = Mfhash.Make (Hashlex.HashedString)

(** a term lookup table-t kezeli *)
type writer = {doclist_oc : out_channel;
			   lexicon_oc : out_channel
}

type term_record = (int * int * int) 

type reader = {doclist_ic : in_channel;
			   lexicon      : (term_record) Lex.t;
			   mutable tokens       : int;
				stopper     : Timem.t
}


let open_writer index_dir =
	{doclist_oc = open_out_bin (index_dir ^ "/" ^ "postings");
	lexicon_oc = open_out_bin (index_dir ^ "/" ^ "lexicon");
	} 

let write_term_entry w term terminfo =
	flush w.doclist_oc;
	let pos = pos_out w.doclist_oc in
	let df = DocList.Collector.df terminfo in
	let tf = DocList.Collector.tf terminfo in
	Io.output_string w.lexicon_oc term;
	output_binary_int w.lexicon_oc df;
	output_binary_int w.lexicon_oc tf;
	output_binary_int w.lexicon_oc pos	;
	DocList.write w.doclist_oc (DocList.Collector.doclist terminfo)
;;

let close_writer w =
	close_out w.doclist_oc;
	close_out w.lexicon_oc
;;	



let open_reader index_dir = 
	let reader = {doclist_ic = open_in_bin (index_dir ^ "/" ^ "postings");
				  lexicon = Lex.create 10 ;
				   tokens  = 0;
				  stopper = Timem.init ();
	} in
	Timem.start reader.stopper "loading lexicon";
	let lic =  open_in_bin (index_dir ^ "/" ^ "lexicon") in
	let rec loop () = 
		let term = Io.input_string lic in
		let df   = input_binary_int lic in
		let tf   = input_binary_int lic in
		let pos  = input_binary_int lic in
		Lex.update reader.lexicon (df, tf, pos) term (fun x -> x);
		reader.tokens <- reader.tokens + tf;
		loop ()
	in
	try
	 loop () 
 	with End_of_file -> 

	Timem.finish reader.stopper;
	reader
;;

let types reader = Lex.size reader.lexicon;;
	
let tokens reader = reader.tokens;;
	
let term_info reader term =
	let (df, tf, pos) = Lex.find reader.lexicon term in
	let open_stream () =
		Timem.start reader.stopper "loading postings";
		seek_in reader.doclist_ic  pos;
		let doclist = DocList.read reader.doclist_ic  df in
		Timem.finish reader.stopper ;
    	DocList.open_stream doclist
	in
	(df, tf, open_stream)
	
let pretty_print reader =
	let aux term (df, tf, pos) =
		Printf.printf "%s -> " term;
	    Printf.printf "df = %d tf = %d\n " df tf;
		seek_in reader.doclist_ic  pos;
		let doclist = DocList.read reader.doclist_ic  df in
		DocList.pretty_print doclist ; 
	in
	Lex.sorted_iter  aux reader.lexicon
