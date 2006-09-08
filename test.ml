let _ =
let t = Timem.init () in
Timem.start t "reading inx";
let inp = open_in_bin "index.inx" in
let iix = InvIndex.read inp in
Timem.stop t ;
let 	(s2, lex2, postings2, positions2) = InvIndex.iterate_over iix in
	Printf.printf "backed up calculated size is %d\n" s2;
		Printf.printf "terms %d postings %d positions %d \n" lex2 postings2 positions2 ;
		
Printf.printf "backed number of tokens:    %d\n" (InvIndex.number_of_tokens iix);
Printf.printf "backed number of types:     %d\n" (InvIndex.number_of_types iix)

