open OUnit
open Fsa_builder

let input_file = "data/fsa_test.lex"

let words = ref [] 

let input_test_words _ =
	try
	let ic = open_in input_file in
	let acc  = ref [] in
	try
		while(true) do 
			acc := (input_line ic) :: !acc;
		done;
	with End_of_file -> 
	(
		close_in ic;
		words := List.rev !acc
	)
	
	with Sys_error s -> assert_failure ("can't read the test lexicon: " ^ s)
;;


let builder = Fsa_builder.start() 



let test_build _ =
	let last_ix = ref (-1) in
	let add_word w =
		let ix = add_word builder w in
		if ix != (!last_ix) + 1 then
			assert_failure "the ids are not continuous";
			
		last_ix := ix
	in
	List.iter (add_word) !words;
  stop builder

;;


let test_fsa_iter  iterator _  =

	let prev = ref (-1) in
	let words = ref !words in
	let word w ix =
		if ix != (!prev) + 1 then
    (
			assert_failure "the ids are not continuous"
		);
		
		prev := ix;
		
		let ws = String.make (List.length w) ' ' in
		let _ = List.fold_left (fun i c -> ws.[i] <- c; succ i) 0 w in
		let w = ws in
		match !words with
			| h :: t -> 
				begin
					words := t;
					if  compare w h != 0 then
						assert_failure ("got " ^ w ^ " back but expected " ^ h)
		    end
			| _ -> assert_failure ("got " ^ w ^ " back but no more word was expected")
	in
	iterator word;
	if List.length !words != 0 then
		assert_failure ("got less words back than in the test : " ^ (String.concat " " !words))
	
let compacted = ref None 

let compact _ =
	  let fsa = Compact_fsa.compact builder in
		compacted := Some fsa
;;  

let simple_iterator f =
		Simple_fsa.iter f (Fsa_builder.fsa builder)
	
let get_compacted () =
	let compacted =  match !compacted with
		| None -> assert_failure "no compacted fsl found"
		| Some v -> v
	in compacted
		
let compacted_iterator _ f =
	let compacted =  get_compacted()
	in
	Compact_fsa.iter f (compacted)
		
let temp_file = "vacak.fsa" 

let save_compacted _ =
	let compacted = get_compacted () in
	let oc = open_out_bin temp_file in
		Compact_fsa.save_binary compacted oc;
	close_out oc
	
let read_compacted_back _ =
	let ic = open_in_bin temp_file in
	try
	compacted := Some (Compact_fsa.load_binary ic );
	close_in ic
	with _ -> assert_failure "can't read compacted fsa"
	

let compare_fsls _ =
	assert_equal ( Fsa_builder.total_transitions builder) (Compact_fsa.total_transitions (get_compacted ()));
	assert_equal ( Fsa_builder.total_states builder) (Compact_fsa.total_states (get_compacted ()))
	
let suite = "fsl" >::: ["read test words" >:: input_test_words;
											  "build simple fsl" >:: test_build ;
												"iterate simple fsl" >:: test_fsa_iter simple_iterator;
												"compact fsl" >:: compact;
												"compare simple and compacted fsl" >:: compare_fsls;
												"iterate compactad fsl" >:: test_fsa_iter (compacted_iterator ());
												"save compacted fsl" >:: save_compacted;
												
												"read compacted back" >:: read_compacted_back;
											  "compare fsls" >:: compare_fsls;
												"iter read compact fsl" >:: test_fsa_iter (compacted_iterator ());
										
																]
