module IntBlockList = BlockList.Make(struct type t = int let default = 0 end)

module Make : InvIndex.Lexicon = struct
	
	type fptr = Int64.t

(* a lexikon nem más, mint egy fsa, ami uniq id-t rendel minden termhez,   *)
(* es három tömb: a termid-hez redelnek tf, df értékeket és a posting      *)
(* lista kezdetét                                                          *)

	type t =
		{
			fsa : Compact_fsa.t;
			tfa : int array;
			dfa : int array;
			pa : fptr array
		}
	
	let init dir =
		let ic = open_in_bin (dir ^ "/fsalex") in
		let fsa = Compact_fsa.load_binary ic in
		let tfl = Io.input_vnatint ic in
		let tfa = Array.make tfl 0 in
		for i = 0 to tfl - 1 do
			tfa.(i) <- Io.input_vnatint ic;
		done;
 
		let dfl = Io.input_vnatint ic in
		let dfa = Array.make dfl 0 in
		for i = 0 to dfl - 1 do
			dfa.(i) <- Io.input_vnatint ic;
		done;
 
		let pl = Io.input_vnatint ic in
		let pa = Array.make tfl 0L in
		for i = 0 to pl - 1 do
			pa.(i) <- Io.input_vint64 ic;
		done;
 
		{ fsa = fsa;
		tfa = tfa;
		dfa = dfa;
		pa = pa
		}
	;;
 
	let find lex term =
		let ix = Compact_fsa.word2index lex.fsa term in
		let tf = lex.tfa.(ix) in
		let df = lex.dfa.(ix) in
		let p = lex.pa.(ix) in
		(tf, df, p)
	
	
	let iter  f lex =
		let doTerm term ix =
			let tf = lex.tfa.(ix) in
			let df = lex.dfa.(ix) in
			let p = lex.pa.(ix) in
			f term tf df p
		in
		Compact_fsa.iter doTerm lex.fsa 
		;;
		
	module Writer = struct
		type t =
			{
				dir : string;
				fsa_builder : Fsa_builder.t;
				mutable n : int;
				mutable tflist : int list;
				mutable dflist : int list;
				mutable pointerlist : int64 list
 
			}
 
		let create d =
			{
				dir = d;
				fsa_builder = Fsa_builder.start ();
				tflist = [];
				dflist = [];
				pointerlist = [];
				n = 0;
			}
 
		let add w term tf df pointer =
			let _ = Fsa_builder.add_word w.fsa_builder term in
			w.tflist <- tf :: w.tflist;
			w.dflist <- df :: w.dflist;
			w.pointerlist <- pointer :: w.pointerlist;
			w.n <- w.n + 1;
 
		;;

		let close w =
			Fsa_builder.stop w.fsa_builder;
			let timer = Timem.init () in
 
			Timem.start timer "dumping lexicon";
 
			let oc = open_out_bin (w.dir ^ "/fsalex") in
 
			Timem.start timer "compacting fsa";
			let fsa = Compact_fsa.compact w.fsa_builder in
			Compact_fsa.save_binary fsa oc;
			Timem.finish timer;
 
			Timem.start timer "outputting terminfo arrays";
 
			(* nem tul hatekony kod *)
			let tfa = Array.of_list (List.rev w.tflist) in
			Io.output_vnatint oc (Array.length tfa);
			Array.iter (Io.output_vnatint oc) tfa;
 
			let dfa = Array.of_list (List.rev w.dflist) in
			Io.output_vnatint oc (Array.length dfa);
			Array.iter (Io.output_vnatint oc) dfa;
 
			let pa = Array.of_list (List.rev w.pointerlist) in
			Io.output_vnatint oc (Array.length pa);
			Array.iter (Io.output_vint64 oc) pa;
			Timem.finish timer;
 
			close_out oc;
			Timem.finish timer
 
		end (* Writer *)
end (* FsaLexicon *)