 
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
	let lic =  open_in_bin (index_dir ^ "/" ^ "lexicon") in
	let rec loop () = 
		let term = Io.input_string lic in
		let df   = input_binary_int lic in
		let tf   = input_binary_int lic in
		let pos  = Io.input_vint64 lic in
        Printf.printf "%s\t%d\t%d\n" term df tf;
		loop ()
	in
	try
	 loop () 
 	with End_of_file -> ()
	
let search index_dir  =
	let ii = InvIndex.open_reader index_dir in
(*	InvIndex.pretty_print ii;
*)	let fi = ForIndex.open_reader index_dir in
	let t = Timem.init () in
	let search term = 
	Timem.start t ("searching " ^ term);
	let (df, tf, open_stream) = InvIndex.term_info ii term in
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
	let ii = InvIndex.open_reader index_dir in
	InvIndex.pretty_print ii
;;


let print_stat index_dir =
	let ii = InvIndex.open_reader index_dir in
	Printf.printf "unique terms: %d\n" (InvIndex.types ii);
	Printf.printf "unique tokens: %d\n" (InvIndex.tokens ii)
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

let dump tempfile = 
	Merger.pretty_print_stream tempfile
;;

let usage () = 
	Printf.eprintf "usage : %s index-dir build tok | search | dump-temp | dump-index | stat \n" Sys.argv.(0)
;;
	
let _ =	
if (Array.length Sys.argv) < 2 then 
	let _ = usage () in	exit 1
else
	let indexdir = Sys.argv.(1) in
	match Sys.argv.(2) with
		"build" -> index indexdir  (int_of_string Sys.argv.(3))
	 |  "dump-index" -> dump_index indexdir
	 |  "dump-lexicon" ->  dump_lexicon  indexdir
	 |  "stat" -> print_stat indexdir
	 |  "search" -> search indexdir 
	 |  "dump-temp" -> dump Sys.argv.(3)
	 | _ -> usage () ; exit 1
	
