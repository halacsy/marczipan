 
let proc_sentence ii sentence =
		let _ = InvIndex.start_doc ii in
        let n = List.fold_left (fun i (word, gold)  ->  InvIndex.add_term ii word  i ;  (succ i)) (0) sentence in
	    InvIndex.end_doc  ii
;;

	
let _ = 
	let limit = int_of_string Sys.argv.(1) in
	Printf.eprintf "token limit = %d\n" limit;
	let ii = InvIndex.start_collection limit in
	Io.iter_sentence stdin ( proc_sentence ii ) ;
	InvIndex.end_collection ii
	;
	Merger.pretty_print_stream "terminfos.merged" 
 
