module Make : InvIndex.Lexicon = struct
	
	type fptr = Int64.t

	let max_block_size = Int64.of_int (1024 * 8);;

	type t = { terms : string array;
	positions : fptr array;
	lexicon_ic : in_channel }
	let init index_dir =
	(* load the lexicon index *)
		let ic = open_in_bin (index_dir ^ "/" ^ "lexicon.inx") in
		let len = Io.input_vnatint ic in
		let terms = Array.create len "" in
		let positions = Array.create len 0L in
		for i = 0 to (len - 1) do
			terms.(i) <- Io.input_string ic;
			positions.(i) <- Io.input_vint64 ic;
		done;
		close_in ic;
		Printf.printf "lexicon index loaded: %d\n" len;
		{ terms = terms; positions = positions; lexicon_ic = open_in_bin (index_dir ^ "/" ^ "lexicon") }
	;;

	(* lusta voltam megirni*)
  let iter f lex = ();;
	
(* Returns the offset of the greatest index entry which is less than or    *)
(* equal to term.                                                          *)
	let search_pos a term =
		let lo = ref 0 in
		let hi = ref ((Array.length a) - 1) in
		while (!hi >= !lo) do
			let mid = (!hi + !lo ) lsr 1 in
			let delta = compare term a.(mid) in
			if delta < 0 then
				hi := mid - 1
			else if delta > 0 then
				lo := mid + 1
				else
					begin
						hi := mid;
						lo := mid + 1
					end
		done;
		!hi
	;;

	let find lex term =
		let ix = search_pos lex.terms term in
		let pos = if ix < 0 then 0L else lex.positions.(ix) in
		LargeFile.seek_in lex.lexicon_ic pos;
		let rec seek_term () =
			let rterm = Io.input_string lex.lexicon_ic in
			let df = Io.input_vnatint lex.lexicon_ic in
			let tf = Io.input_vnatint lex.lexicon_ic in
			let pos = Io.input_vint64 lex.lexicon_ic in
			if (term = rterm )	then (df, tf, pos)
			else if (term < rterm ) then raise Not_found
			else
				seek_term ()
		in
		try
			seek_term ()
		with End_of_file -> raise Not_found

	
	module Writer =
	struct
		type t = {	index_dir : string;
		lexicon_oc : out_channel;
		mutable last_term : string;
		mutable last_block_start : fptr;
		mutable index : (string * fptr) list
		}
 
		let create index_dir = { index_dir = index_dir;
		lexicon_oc = open_out_bin (index_dir ^ "/" ^ "lexicon");
		last_term = "";
		last_block_start = - 1L;
		index = []}
 
		let close lw =
			close_out lw.lexicon_oc;
	(* print the lexicon index *)
			let oc = open_out_bin (lw.index_dir ^ "/" ^ "lexicon.inx") in
	(* could be more clever *)
			let l = List.rev lw.index in
			Io.output_vnatint oc (List.length l);
			let aux (term, pos) =
				Io.output_string oc term;
				Io.output_vint64 oc pos
			in
			List.iter (aux) l;
			close_out oc
		;;

		let add_block_index lw term start_pos =
			lw.index <- (term, start_pos) :: lw.index
		;;
 
		let add lw term df tf pos =
	(* hatha ez egy blok kezdet lesz *)
			let start_pos = LargeFile.pos_out lw.lexicon_oc in
			Io.output_string lw.lexicon_oc term;
			Io.output_vnatint lw.lexicon_oc df;
			Io.output_vnatint lw.lexicon_oc tf;
			Io.output_vint64 lw.lexicon_oc pos;
			let end_pos = LargeFile.pos_out lw.lexicon_oc in
	(* kilogna-e blockbol *)
			if (Int64.sub end_pos lw.last_block_start) > max_block_size then begin
				add_block_index lw term start_pos;
				lw.last_block_start <- start_pos
			end
	
	end

end