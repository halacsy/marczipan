type stream = Closed | Stream of  in_channel * string *  Terminfo.t 
			
		
let open_terminfo_stream file =
	let ic = open_in_bin file in
	let (term, first) = Terminfo.read ic in
	Stream(ic, term,  first )
	
(** terminfo streambol felolvassa a kovetkezo terminfot. Ha nincs tobb, zarja a streamet.*)			
let fetch_next stream = match stream with
	| Closed -> stream
	| Stream(ic, _,  _) ->  try 
		let (term, terminfo) = Terminfo.read ic in
		Stream(ic, term,  terminfo)
		(** ha rossz a fajl es nem jonnek be a terminfok, akkor nem szerzunk rola tudomast *)
		with Terminfo.End_of_terminfos -> 	close_in ic ; Closed

(** a listaban levo streamek termifoit osszefuzi *)
let merge_streams l = match l with
	| [] -> raise (Invalid_argument "apply: index out of bounds")
	| Stream(_,term1,ti)::tail -> let merge ti str = match str with
		 						Stream(_,term2, ti2) ->
			 						let m = Terminfo.merge ti ti2 
									in m   
									
							  | _  -> raise (Invalid_argument "someone want to merge closed stream")
							in
							List.fold_left merge  ti tail 
	| _ -> 	raise (Invalid_argument "someone want to merge closed stream")
	
(** l streameket lepteti, es ha van meg bennunk cucc, akkor beteszi oket a heapbe *)	
let fetch_streams h l =
 	let aux heap stream =
		match  (fetch_next stream) with
			Closed -> heap
		 |  Stream(_, term, ti) as stream -> Heap.insert heap  (term, Terminfo.last_doc ti) stream
	in
	List.fold_left aux h l
	
let merge files =
	(* megnyitjuk az osszes fajlt es beolvassuk az elso terminfoit, es heapbe rakjul *)
	let aux heap file =
		match open_terminfo_stream file with
			 Stream(_, term, ti) as stream -> Heap.insert heap  (term, Terminfo.last_doc ti) stream
			| _ -> raise (Invalid_argument "someone want to merge closed stream")
	in
	let heap = List.fold_left aux (Heap.empty) files in

	let winner heap = 
		let ((term,docid), stream, heap) = Heap.consume heap in
		let rec aux heap streams =
			try
				let (nterm, docid) = Heap.top_prior heap in
				if term = nterm then
					let ((a,b), stream, heap) = Heap.consume heap in
					aux heap (stream::streams) 
				else
		 			(streams, heap)
			with Heap.Queue_is_empty -> (streams, heap)
		in
		let (streams, heap) = aux heap (stream::[]) in
		(term, streams, heap) 
	
	in
	let oc = open_out_bin "terminfos.merged" in
	(* amit elfogyasztunk *)
    let rec loop heap =
		let (term, winners, heap) = winner heap in
		let merged_terminfo = merge_streams winners in
		let _ = Terminfo.write oc term merged_terminfo in
		let heap = fetch_streams heap winners in
		loop heap
	in
	try
	loop heap;
	with Heap.Queue_is_empty ->
	close_out oc
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