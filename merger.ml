type t = {work_dir   : string;
		  mutable temp_files : string list}

let init work_dir = {work_dir = work_dir; temp_files = [] }
	
type stream = {ic: in_channel; mutable term : string ; mutable terminfo :  DocList.Collector.t} 
			
(* ide kerul a tempfile iras az inverterbol, hogy egy helyen legyen *)


let write_to_temp oc term collected =
	Io.output_string oc term;
	DocList.Collector.write oc collected;
;;

let need_merge m = (List.length m.temp_files) > 0

let flush m iter =
	let temp_file = m.work_dir ^ "/merger.temp." ^ (string_of_int (List.length m.temp_files)) in
	m.temp_files <- temp_file :: m.temp_files ;
	let oc = open_out_bin temp_file in
	iter (write_to_temp oc);
	close_out oc
;;		

let read_next_from_temp ic =
	let term = Io.input_string ic in
	let collected = DocList.Collector.read ic in
	(term, collected)
;;
		
(* todo: itt derul ki, ha ures a fajl.*)
let open_terminfo_stream file =
	let ic = open_in_bin file in
	let (term, first) = read_next_from_temp ic in
	{ic = ic; term = term; terminfo = first}
	
(** terminfo streambol felolvassa a kovetkezo terminfot. Ha nincs tobb, zarja a streamet.*)			
let fetch_next stream = 
	  try
			let (term, terminfo) = read_next_from_temp stream.ic in
			stream.term <- term;
			stream.terminfo <- terminfo;
		with End_of_file -> 	close_in stream.ic ; raise End_of_file


let pretty_print_stream file =
	let stream = open_terminfo_stream file in
	let rec loop () = 
			DocList.Collector.pretty_print stream.term stream.terminfo;
			fetch_next stream;
			loop ()
	in
	try
		loop ()
	with End_of_file -> ()
;;

(* kiveszi a felsot a heap-bol, a stream-et fetcheli, majd visszarakja *)
let get_top heap =
	let ((term, docid), stream, heap) = Heap.consume heap in
	let ti = stream.terminfo in
	let heap= try 
				fetch_next stream;
				Heap.insert heap  (stream.term, DocList.Collector.last_doc stream.terminfo) stream
			  with End_of_file -> heap
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
				DocList.Collector.append merged_ti ti;
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
		Heap.insert heap  (stream.term, DocList.Collector.last_doc stream.terminfo) stream
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
	imerge (InvIndex.write_term_entry writer) files;
;;		

let merge writer m =
	final_merge writer m.temp_files 
;;

(*
	
let _ =
	let l = "terminfos.temp.0" :: "terminfos.temp.1" :: [] in
	let _ = merge l in
	let rec loop stream = match stream with 
		Stream(_, term, terminfo) as stream -> 
			
				DocList.pretty_print term terminfo;
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