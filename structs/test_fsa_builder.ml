open Fsa_builder
module ISet = Set.Make (struct type t = int let compare = compare end)


	


let _ =
  let lexicon_ic = open_in Sys.argv.(1) in
	let builder = Fsa_builder.create_builder in
	let rec loop() =
		let line = (input_line lexicon_ic) in
		Fsa_builder.add_string builder  line;
		loop()
	in
	try loop () with End_of_file ->
  close_in lexicon_ic;
  let fsa = end_of_words builder in
  print_graphviz fsa;