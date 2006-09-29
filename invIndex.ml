module Lex = Mfhash.Make (Hashlex.HashedString)

(** a term lookup table-t kezeli *)
type writer = {doclist_oc : out_channel;
			   lexicon_oc : out_channel
}

type term_record = (int * int * int) 

type reader = {doclist_ic : in_channel;
			   lexicon      : (term_record) Lex.t;
				stopper     : Timem.t
}


let open_writer index_dir =
	{doclist_oc = open_out_bin (index_dir ^ "/" ^ "postings");
	lexicon_oc = open_out_bin (index_dir ^ "/" ^ "lexicon");
	} 

let write_term_entry w term terminfo =
	flush w.doclist_oc;
	let pos = pos_out w.doclist_oc in
	let df = Terminfo.Collector.df terminfo in
	let tf = Terminfo.Collector.tf terminfo in
	Io.output_string w.lexicon_oc term;
	output_binary_int w.lexicon_oc df;
	output_binary_int w.lexicon_oc tf;
	output_binary_int w.lexicon_oc pos	;
	Terminfo.write w.doclist_oc (Terminfo.Collector.doclist terminfo)
;;

let close_writer w =
	close_out w.doclist_oc;
	close_out w.lexicon_oc
;;	

let load_lookup_table index_dir lex = 
	let lic =  open_in_bin (index_dir ^ "/" ^ "lexicon") in
	let rec loop () = 
		let term = Io.input_string lic in
		let df   = input_binary_int lic in
		let tf   = input_binary_int lic in
		let pos  = input_binary_int lic in
		Lex.update lex (df, tf, pos) term (fun x -> x);
		loop ()
	in
	try
	 loop () 
 	with End_of_file -> ()

let open_reader index_dir = 
	let reader = {doclist_ic = open_in_bin (index_dir ^ "/" ^ "postings");
				  lexicon = Lex.create 4000000 ;
				  stopper = Timem.init ();
	} in
	Timem.start reader.stopper "loading lexicon";
	let _ = load_lookup_table index_dir reader.lexicon in
	Timem.finish reader.stopper;
	reader
;;

let types reader = Lex.size reader.lexicon
	
let fetch_posting reader term =
	let (_, _, pos) = Lex.find reader.lexicon term in
	seek_in reader.doclist_ic pos;
	let doclist = Terminfo.read reader.doclist_ic in
	doclist
