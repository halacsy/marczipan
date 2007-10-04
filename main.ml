module TermLexicon = FileBasedLexicon.Make
(*
module TermLexicon = FsaLexicon.Make
*)
module InvIndex = InvIndex.Make(TermLexicon)

module Inverter = Inverter.Make(InvIndex.Writer)
module IndexReader = InvIndex.Reader
module Searcher = Searcher.Make(InvIndex.Reader)

let proc_sentence ii sentence =
		let st = String.concat " " (List.map (fun (w,_) -> w) sentence) in
		let meta = DocMeta.empty () in
		let _ = DocMeta.add_string meta 0 st in
		let doc_handler = Inverter.start_doc ii meta in
		let aux i (word, _) =
				Inverter.add_term ii doc_handler word  i;
				succ i
		in
        let n = List.fold_left aux 0 sentence in
		Inverter.end_doc  ii doc_handler
	 
;;
 
let dump_lexicon index_dir = 
	let lexicon = TermLexicon.init index_dir in
	let print_term term tf df pos =
		List.iter print_char term;
		Printf.printf "\t%d\t%d\n"  tf df;
	in
	TermLexicon.iter print_term lexicon


let query index_dir =
	
	let ii = IndexReader.open_reader index_dir in
	let fi = ForIndex.open_reader index_dir in
	let result = Searcher.search ii fi ("hulladék"::"zöld"::[]) in
	let print_doc (docid, w) = 
		let di = ForIndex.doc_info fi docid in
		let meta = ForIndex.doc_meta fi di in
		Printf.printf "%f %s\n" w (DocMeta.get_string meta 0); 	
	in
	List.iter print_doc result;
;;

let search index_dir  =
	let ii = IndexReader.open_reader index_dir in
	  Printf.printf "ii opened";
(*	InvIndex.pretty_print ii;
*)	let fi = ForIndex.open_reader index_dir in
	let t = Timem.init () in
	let search term = 
	Timem.start t ("searching " ^ term);
	let (df, tf, open_stream) = IndexReader.term_info ii term in
	Printf.printf "term %s df tf %d %d\n" term df tf ;
	let doc_stream = open_stream () in
	Timem.finish t ;
	let rec loop i =
		let (docid, freq) = DocList.next_doc doc_stream in
		let di = ForIndex.doc_info fi docid in
		
		Printf.printf "%d. doc: %d freq: %d doclen: %d \n" i docid freq (ForIndex.doc_len di);
		let meta = ForIndex.doc_meta fi di in
		Printf.printf "%s\n" (DocMeta.get_string meta 0); 	
		if (i mod 10) =0  then begin
			Printf.printf "more?\n";
			if (read_line () ) = "y" then
				loop (i+1)
		end
		else
			loop (i+1)
	in
	try
	loop 1
	with DocList.End_of_stream -> ()
	in
	let rec loop () =
		Printf.printf "query:\n";
		let term = read_line () in
		let _ =
		try
		search term
		with Not_found -> Printf.printf "not found\n"
		in
		loop ()
	in
	loop ()
;;


let dump_index index_dir =
(*	let ii = InvIndex.open_reader index_dir in
	InvIndex.pretty_print ii
*) ();;


let print_stat index_dir =
	let ii = IndexReader.open_reader index_dir in
	Printf.printf "tokens: %d\n" (IndexReader.token_count ii);
;;

let index indexdir limit =
	Printf.eprintf "token limit = %d\n" limit;
	let ii = Inverter.start_collection indexdir limit in
	Io.iter_sentence stdin ( proc_sentence ii ) ;
	Inverter.end_collection ii
	;;
(*	Merger.pretty_print_stream "terminfos.merged" 
 *)
()

let index_mh indexdir limit =
  Printf.eprintf "token limit = %d\n" limit;
	let ii = Inverter.start_collection indexdir limit in
  let proc_doc id content =
  (*  Printf.printf "adding %s\n" id;
    *)let meta = DocMeta.empty () in
    let _ = DocMeta.add_string meta 0 id in
  
    let doc_handler = Inverter.start_doc ii meta in
    let i = ref 0 in
    let aux (Tokenizer.Token (s, _, _, _)) =
      Inverter.add_term ii doc_handler s  !i;
      incr i;
    in
    Tokenizer.iterate aux (Lexing.from_string content);
  	Inverter.end_doc  ii doc_handler
  in  
  Mh_parser.parse stdin proc_doc;  
	Inverter.end_collection ii
	;;

  
let dump tempfile = 
	Merger.pretty_print_stream tempfile
;;

let usage () = 
	Printf.eprintf "usage : %s index-dir build tok-limit | indexmh tok-limit | search | dump_lexicon | dump-index | stat \n" Sys.argv.(0)
;;
	
let _ =	
if (Array.length Sys.argv) < 2 then 
	let _ = usage () in	exit 1
else
	let indexdir = Sys.argv.(1) in
	match Sys.argv.(2) with
		"build" -> index indexdir  (int_of_string Sys.argv.(3))
		| "query" -> query indexdir
		|"indexmh" -> index_mh indexdir  (int_of_string Sys.argv.(3))
    
	 |  "dump-index" -> dump_index indexdir
	 |  "dump-lexicon" ->  dump_lexicon  indexdir
	 |  "stat" -> print_stat indexdir
	 |  "search" -> search indexdir 
	 |  "dump-temp" -> dump Sys.argv.(3)
	 | _ -> usage () ; exit 1
	
