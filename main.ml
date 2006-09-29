 
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
	    Printf.printf "%d" n;
	Inverter.end_doc  ii doc_handler;
	  Printf.printf "%d" n;
;;

let print_stat reader =
	Printf.printf "unique terms: %d\n" (InvIndex.types reader)
;;
	
let search index_dir =
	let ii = InvIndex.open_reader index_dir in
	let fi = ForIndex.open_reader index_dir in
	let term = "a" in
	let (df, tf, open_stream) = InvIndex.term_info ii term in
	Printf.printf "term %s df tf %d %d\n" term df tf ;
	let doc_stream = open_stream () in
	let rec loop () =
		let (docid, freq) = DocList.next_doc doc_stream in
		let di = ForIndex.doc_info fi docid in
		
		Printf.printf "doc: %d freq: %d doclen: %d \n" docid freq (ForIndex.doc_len di);
		let meta = ForIndex.doc_meta fi di in
		Printf.printf "%s\n" (DocMeta.get_string meta 0); 	
		loop ()
	in
	try
	loop ()
	with DocList.End_of_stream -> ()

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
	Printf.eprintf "usage : %s index-dir build tok | search | dump-temp\n" Sys.argv.(0)
;;
	
let _ =	
if (Array.length Sys.argv) < 2 then 
	let _ = usage () in	exit 1
else
	let indexdir = Sys.argv.(1) in
	match Sys.argv.(2) with
		"build" -> index indexdir  (int_of_string Sys.argv.(3))
	 |  "search" -> search indexdir
	 |  "dump-temp" -> dump Sys.argv.(3)
	 | _ -> usage () ; exit 1
	
