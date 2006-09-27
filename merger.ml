type stream = {ic: in_channel; mutable term : string ; mutable terminfo :  Terminfo.t} 
			
		
(* todo: itt derul ki, ha ures a fajl.*)
let open_terminfo_stream file =
	let ic = open_in_bin file in
	let (term, first) = Terminfo.read ic in
	{ic = ic; term = term; terminfo = first}
	
(** terminfo streambol felolvassa a kovetkezo terminfot. Ha nincs tobb, zarja a streamet.*)			
let fetch_next stream = 
	  	try 
			let (term, terminfo) = Terminfo.read stream.ic in
			stream.term <- term;
			stream.terminfo <- terminfo;
		with Terminfo.End_of_terminfos -> 	close_in stream.ic ; raise Terminfo.End_of_terminfos


let pretty_print_stream file =
	let stream = open_terminfo_stream file in
	let rec loop () = 
			Terminfo.pretty_print stream.term stream.terminfo;
			fetch_next stream;
			loop ()
	in
	try
		loop ()
	with Terminfo.End_of_terminfos -> ()
;;

(* kiveszi a felsot a heap-bol, a stream-et fetcheli, majd visszarakja *)
let get_top heap =
	let ((term, docid), stream, heap) = Heap.consume heap in
	let ti = stream.terminfo in
	let heap= try 
				fetch_next stream;
				Heap.insert heap  (stream.term, Terminfo.last_doc stream.terminfo) stream
			  with Terminfo.End_of_terminfos -> heap
	in
	(term, ti, heap);;
	
let merge_tops heap  =
	(* kivesszuk az elsot es ha meg van ugyanolyan termu, akkor hozzafuzzuk, majd kiirjuk *) 
	let (term, merged_ti, heap) = get_top heap in
	let rec aux heap =
		try
			let (nterm, docid) = Heap.top_prior heap in
			flush_all ();
			if term = nterm then
				let (_, ti, heap) = get_top heap in
				Terminfo.append merged_ti ti;
				aux heap 
			else
	 			(heap)
		with Heap.Queue_is_empty -> (heap)
	
	in
	
	(term, merged_ti, aux heap);;

	


(** majd a hierarchikus merge-nel a kozbenso lepeseknel nem kell teljes indexet 
	irnunk. Ezert van ez itt kulon veve.*)
let imerge writer files =
	(* megnyitjuk az osszes fajlt es beolvassuk az elso terminfoit, es heapbe rakjul *)
	let aux heap file =
		let stream = open_terminfo_stream file in
		Heap.insert heap  (stream.term, Terminfo.last_doc stream.terminfo) stream
	in
	let heap = List.fold_left aux (Heap.empty) files in

	(* amit elfogyasztunk *)
    let rec loop heap =
		let (term, merged_terminfo, heap) = merge_tops heap in
		let _ = writer term merged_terminfo in
		loop heap
	in
	try
	loop heap;
	with Heap.Queue_is_empty -> ()
;;

let final_merge writer files =
	imerge (TermIndex.write_term_entry writer) files;
;;		

let merge writer files =
	final_merge writer files 
;;

(*
	
let _ =
	let l = "terminfos.temp.0" :: "terminfos.temp.1" :: [] in
	let _ = merge l in
	let rec loop stream = match stream with 
		Stream(_, term, terminfo) as stream -> 
			
				Terminfo.pretty_print term terminfo;
				loop (fetch_next stream)
			| _ -> ()
	in
		Printf.printf "0\n";
		loop (open_terminfo_stream "terminfos.temp.0");
	
		Printf.printf "1\n";
		loop (open_terminfo_stream "terminfos.temp.1");

		Printf.printf "merged\n";
		loop (open_terminfo_stream "terminfos.merged")

*)