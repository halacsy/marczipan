module DocMap = Mfhash.Int

module Make (Reader : InvIndex.Reader ) = struct
	type query = string list

	let total_count = 1000000.;;
	let alpha = 0.5;;

	let search index forIndex query =
		let docMap = DocMap.empty () in
		let for_each term =
			let (tf, df, open_stream) = Reader.term_info index term in
			let doc_stream = open_stream () in
			let for_each docid freq =
				let di = ForIndex.doc_info forIndex docid in
				let doclen = ForIndex.doc_len di in
				let w = alpha *. (float) freq /. (float) doclen +. (1. -. alpha) *. (float) tf /. total_count in
				let _ = DocMap.update (fun () -> w) (fun x -> x +. w) docMap docid in ()
			
			in
			try
				while true do
					let (docid, freq) = DocList.next_doc doc_stream in
					for_each docid freq
				done;
			with DocList.End_of_stream -> ()
 
		in
		List.iter for_each query;
		let list = DocMap.to_list docMap in
		let compare (docid1, w1) (docid2, w2) =
			compare w1 w2
		in
		let list = List.sort compare list in
		list

end