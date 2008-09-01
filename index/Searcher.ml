module DocMap = Mfhash.Int
type freq_prob = Freq of int | Prob of float

let prob fp = match fp with
  Prob f -> f
 | _ -> failwith "bad argument of prob"
;;

module Make (Reader : InvIndex.Reader ) = struct
	type query = string list

	let alpha = 0.5;;

	type term_doc_list =
		{ term : string;
		coll_prob : float;
		mutable curr_docid : int;
		mutable curr_doc_prob : freq_prob;
		doc_stream : unit -> (int * int)
		}
 
 (* ha jol ertem, amit itt csinaltam, de vajon mikor, akkor egy heapben taroljuk a term_doc_listeket
   minden listanal tudjuk, h eppen melyik dokumentumban vagyunk.
   Tehat van k darab term_doc_list, ezeket mindig rendezve taroljuk aszerint, hogy melyik dok a kovetkezo a listan.
   a Heap.consume_tops visszaadja azoket a term_doc_listakat amik eppen aktualis legkissebb docid-hoz tartoznak
   ezeket csak osszegezni kell.
   egyszeru term oriented megoldas.
   
    *)
	let score_docs f index forIndex query =

		let total_count = float_of_int (Reader.token_count index) in

		let step term_doc_list =
			let (docid, freq) =  term_doc_list.doc_stream () in

		
			term_doc_list.curr_docid <- docid;
			term_doc_list.curr_doc_prob <- Freq  freq 
		in

		let open_term_info term =
			let (tf, df, open_stream) = Reader.term_info index term in
			let doc_stream = open_stream () in
			let x = {
				term = term;
				coll_prob = (float) tf /. total_count;
				curr_docid = - 1;
				curr_doc_prob = Freq 0;
				doc_stream = doc_stream;
			} in
			step x;
			x
		in
		(* doc_listek rendezve vannak, a heapben meg rendezve tartjuk a        *)
		(* doc_listeket                                                        *)

		(* nem minden query termbol lesz doclist, mert lehet, h nincs is az    *)
		(* indexben                                                            *)
 
		let add_to_heap heap term =
			try
				let dl = open_term_info term in
				Heap.insert heap ( dl.curr_docid) dl
			with Not_found -> (* ha egy term nincs a gyujtemenyben *)
				heap
		in
 
		let heap = List.fold_left add_to_heap Heap.empty query in
 
		let rec loop heap =
		(* minden menetben megnezzuk, h a heap tetejen levo doc-nak mi a sulya *)
			let (doc, term_doc_lists, heap) = Heap.consume_tops heap in
			
			(* hat kezd barokkosodni. Lent beraktunk max_intes term_infokat,     *)
			(* amikbol mar nem tudunk olvasni. Ha csak ilyen van mar, akkor kesz *)
			(* vagyunk                                                           *)
			if doc = (max_int) then
		()
			else begin

  			(* egy doknal vagyunk, most kikeressuk a hosszat es minden freq(t,d) atszamoljuk P(t,d)-re *)
  			let di = ForIndex.doc_info forIndex doc in
    		let doc_len = ForIndex.doc_len di in
    		let freq2prob tdl =
    		  let freq = match tdl.curr_doc_prob with
    		    Freq f -> f
    		    | _ -> failwith ("algorithmic error")
  		    in
  		    tdl.curr_doc_prob <-Prob ( (float_of_int freq) /. float_of_int doc_len)
  		  in
  			List.iter freq2prob term_doc_lists;
  						
			(* most van egy doc dokunk, es, hogy hol melyik szavak szerepelnek   *)
			(* benne                                                             *)
				let weight w term_doc_list =
					w +. log( alpha *. (prob term_doc_list.curr_doc_prob) +. (1. -. alpha) *. term_doc_list.coll_prob);
				in
				let w = List.fold_left weight 0.0 term_doc_lists in
			(* de meg azok a termek is hozzaadnak, akik ebben a dokumentumban    *)
			(* nincsenek benne                                                   *)
				let weight w term_doc_list =
					w +.log ( (1. -. alpha) *. term_doc_list.coll_prob);
				in
				let w = Heap.fold weight w heap in
			(* itt megvan a doc, w sulya *)
				f doc w;
 
			(* es most ujra rakjuk a heapbe azokat a doclisteket, akiket         *)
			(* leptetunk                                                         *)
				let add_to heap term_doc_list =
					let _ =
						try
							step term_doc_list;
						with DocList.End_of_stream ->
					(* ez a term mar nem szerepel tobb dokban. De meg kell tartanunk *)
					(* hogy az unseen sulyt hozzaadja mindenkihez                    *)
							term_doc_list.curr_docid <- max_int;
							term_doc_list.curr_doc_prob <- Prob 0.0
 
					in
				  Heap.insert heap ( term_doc_list.curr_docid) term_doc_list 
				in
				let heap = List.fold_left add_to heap term_doc_lists
				in
				loop heap;
				end
		in
		try
			loop heap
		with Heap.Queue_is_empty -> ()
	;;

	let search2 index forIndex query =
		let res = ref [] in
		let aux docid w =
			res := (docid, w) :: !res
		in
		score_docs aux index forIndex query;
		let list = !res in
		let compare (docid1, w1) (docid2, w2) =
			compare w2 w1 (* forditott rendezes *)
		in
		let list = List.sort compare list in
		list
	;;

	let search index forIndex query =

		let total_count = float_of_int (Reader.token_count index) in
		let docMap = DocMap.empty () in
		let for_each term =
			try
				let (tf, df, open_stream) = Reader.term_info index term in
				let doc_stream = open_stream () in
 
				let for_each docid freq =
					let di = ForIndex.doc_info forIndex docid in
					let doclen = ForIndex.doc_len di in
			(*		Printf.printf "P(%s | %d) = %d / %d + %d / %d\n" term docid freq doclen tf (int_of_float total_count);
				*)	let w = alpha *. (float) freq /. (float) doclen +. (1. -. alpha) *. (float) tf /. total_count in
					let _ = DocMap.update (fun () -> w) (fun x -> x +. w) docMap docid in ()
 
				in
	
				try
					while true do
						let (docid, freq) =  doc_stream () in
						for_each docid freq
					done;
				with DocList.End_of_stream -> ()

			with Not_found -> () (* ha nincs a term az indexben *)
		in
		List.iter for_each query;
		let list = DocMap.to_list docMap in
		let compare (docid1, w1) (docid2, w2) =
			compare w2 w1 (* forditott rendezes *)
		in
		let list = List.sort compare list in
		list

end