module Lex = Mfhash.Make (Hashlex.HashedString)
module HashedInt = struct
	type t = int
	let hash w s = (Hashtbl.hash w) mod s
	let equal (s1:int) (s2:int) = (s1 = s2) 
	let compare (s1:int) (s2:int) = compare s1 s2
end ;;
module IntHashTable = Mfhash.Make (HashedInt)

let default_type_freqs_size = 10


				
(* data to be collected about a document during indexing *)
type doc_handler =  {doc_id         : int ; (* doc_id *)
 				         type_freqs     : (int) Lex.t ; (*type_freqs :*) 
				         mutable tokens : int   (* tokens*)	;			
						 meta           : DocMeta.t
						}

type forIndex_writer =  {dir			: string;
						 stat_oc	    : out_channel;
						 content_oc     : out_channel;
						 meta_oc		: out_channel;
						}

let start_collection dir = 
	{dir = dir ;
	 stat_oc = open_out_bin (dir ^ "/docindex");
	 content_oc = open_out_bin (dir ^ "/docterms");
	 meta_oc = open_out_bin (dir ^ "/docmeta")
	}
	
let start_doc fiw doc_id meta = 
	{doc_id = doc_id ; type_freqs = Lex.create default_type_freqs_size; tokens = 0 ; meta = meta}


let add_term fiw di term pos =  
	Lex.update di.type_freqs 0 term (succ) ;
    di.tokens <- (succ di.tokens)
;;


let end_doc fiw  diw = 
	(** kiirjuk a metat es feljegyezzuk a poziciojat *)
	flush fiw.meta_oc;
	let metastart = pos_out fiw.meta_oc in
	DocMeta.write fiw.meta_oc diw.meta;
	(** kiirjuk a doc adatait *)
	output_binary_int fiw.stat_oc diw.doc_id;
	output_binary_int fiw.stat_oc  diw.tokens;
	output_binary_int fiw.stat_oc  (Lex.size diw.type_freqs);
	output_binary_int fiw.stat_oc metastart;
;;	
	
let end_collection fiw =
	close_out fiw.meta_oc;
	close_out fiw.stat_oc;
	close_out fiw.content_oc;
	
(** pointer to the term list *)
type ptr_terms_t = int

(** pointer to the meta fields *)
type ptr_meta_t = int 


type doc_info = {types : int;
				 len   : int;
				 ptr_terms : ptr_terms_t;
				 ptr_meta  : ptr_meta_t }
				



type reader = {docmap  : (doc_info) IntHashTable.t;
			   meta_ic : in_channel; 
			   stopper : Timem.t}

let read_docstats index_dir lex = 			
	let ic =  open_in_bin (index_dir ^ "/" ^ "docindex") in
	let rec loop () = 
		let docid = input_binary_int ic in
		let len   =  input_binary_int ic in
		let types = input_binary_int ic in
		let meta_start = input_binary_int ic in
		let info = {types = types; len = len; ptr_meta = meta_start; ptr_terms = 0} in
		IntHashTable.update lex info docid  (fun x -> x);
		loop ()
	in
	try
	 loop () 
 	with End_of_file -> ()

let open_reader index_dir = 
	let reader = {docmap  = IntHashTable.create 5000 ;
				  meta_ic = open_in_bin (index_dir ^ "/docmeta" );
				  stopper = Timem.init ();
	} in
	Timem.start reader.stopper "loading doc stats";
	let _ = read_docstats index_dir reader.docmap in
	Timem.finish reader.stopper;
	reader
	;;
	

let doc_meta r di =
	seek_in r.meta_ic di.ptr_meta; 
	DocMeta.read r.meta_ic
;;

let doc_info reader docid =
	(IntHashTable.find reader.docmap docid)
;;	

let doc_len di = di.len	
	
