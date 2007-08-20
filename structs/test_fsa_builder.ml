open Fsa_builder
module ISet = Set.Make (struct type t = int let compare = compare end)

	let builder = Fsa_builder.start() 

	
let usage () =
  Printf.printf "%s lexiconfile todo\n" Sys.argv.(0)
  

let _ =
 if Array.length Sys.argv < 2 then
  
  usage ()
 else
  
  let lexicon_ic = open_in Sys.argv.(1) in

	let rec loop() =
		let line = (input_line lexicon_ic) in
		let k = Fsa_builder.add_word builder  line in
		loop()
	in
	try loop () with End_of_file ->
  close_in lexicon_ic;
  stop builder;
  print_stat builder ;
  let fsa = Compact_fsa.compact builder in
(*   print_graphviz fsa;*) 
 (*dump fsa; *)
   Printf.printf "index %d\n" (Compact_fsa.word2index fsa "megy");
   let oc = open_out_bin Sys.argv.(2) in
    Compact_fsa.save_binary fsa oc;
		close_out oc;
		
	let ic = open_in Sys.argv.(2) in
	let fsa = Compact_fsa.load_binary ic in
	   Printf.printf "index %d\n" (Compact_fsa.word2index fsa "megy");
	let print w = List.iter (print_char) w in
	let print word weight = 
		print word; print_char '\t'; print_int weight; print_newline()
	in
	Compact_fsa.iter print fsa;
   ()