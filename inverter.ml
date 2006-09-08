module Lex = Mfhash.Make (Hashlex.HashedString)

type posting =  Posting of int * (* doc_id *) 
			            int *  (* freq *) 
			  			int list (* positions *)
			   


type terminfo = Terminfo of
					int *		(* df = hany dokumentumban szerpel *)
	 				int *		(* tf hanyszor szerepel osszesen *)
	 				posting list (* elofordulasok *)
				

type t = Inv_index of
	 				int *              (* number of tokens in the index*)
	  				(terminfo) Lex.t 	(* lexicon maps terms and their posting list *)



let empty_postinglist = [] 
	
let empty_terminfo = Terminfo(0, 0, empty_postinglist) 

let empty = Inv_index(0, Lex.create 100000)
	
let number_of_tokens (Inv_index(tokens, _)) = tokens

let number_of_types (Inv_index(_, lexicon)) = Lex.size lexicon
	
let add_term_accurance (Inv_index(tokens, lexicon)) doc term pos =		
		let add_term_accurance  doc pos (Terminfo(df, tf, postinglist)) =  
			let new_tf = succ tf in

			let (new_postinglist, new_df) = 
			match postinglist with
				| Posting(doc_id, freq, positions) :: tail when doc_id == doc -> 
					(Posting(doc, (succ freq), pos :: positions) :: tail, df)
                | _ -> 	 
					(Posting(doc, 1, pos ::[]) :: postinglist, (succ df))
			in
			Terminfo(new_df, new_tf, new_postinglist)
		
		in
		Lex.update lexicon empty_terminfo term (add_term_accurance doc pos) ;
		Inv_index (succ tokens, lexicon)

(*	
let write (Inv_index(tokens, lexicon)) o = begin
	Printf.eprintf "writing inv_index\n";
	
	output_binary_int o tokens ;
	
	let write_term_info term terminfos =
		
*)	