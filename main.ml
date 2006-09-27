 
let proc_sentence ii sentence =
		let _ = InvIndex.start_doc ii in
        let n = List.fold_left (fun i (word, gold)  ->  InvIndex.add_term ii word  i ;  (succ i)) (0) sentence in
	    InvIndex.end_doc  ii
;;

let print_stat reader =
	Printf.printf "unique terms: %d\n" (TermIndex.types reader)
;;
	
let search index_dir =
	let id = TermIndex.open_reader index_dir in
	print_stat id
;;

let index indexdir limit =
	Printf.eprintf "token limit = %d\n" limit;
	let ii = InvIndex.start_collection indexdir limit in
	Io.iter_sentence stdin ( proc_sentence ii ) ;
	InvIndex.end_collection ii
	;;
(*	Merger.pretty_print_stream "terminfos.merged" 
 *)
()

let usage () = 
	Printf.eprintf "usage : %s index-dir build|search  tok\n" Sys.argv.(0)
;;
	
let _ =	
if (Array.length Sys.argv) < 2 then 
	let _ = usage () in	exit 1
else
	let indexdir = Sys.argv.(1) in
	match Sys.argv.(2) with
		"build" -> index indexdir  (int_of_string Sys.argv.(3))
	 |  "search" -> search indexdir
	 | _ -> usage () ; exit 1
	
