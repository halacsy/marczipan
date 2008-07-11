(* module TermLexicon = FileBasedLexicon.Make *)
module TermLexicon = FsaLexicon.Make
module InvIndex = InvIndex.Make(TermLexicon)

module Inverter = Inverter.Make(InvIndex.Writer)
module IndexReader = InvIndex.Reader
module Searcher = Searcher.Make(InvIndex.Reader)


let analyze = Analyzer.iterate_ngram2 4
let analyze = Analyzer.iterate

let split c str =
	let rec aux acc idx =
		try let idx' = String.index_from str idx c in
		aux ((String.sub str idx (idx' - idx)):: acc) (succ idx')
		with Not_found -> List.rev ((String.sub str idx (String.length str - idx))
		:: acc)
	in
	aux [] 0

let load_topics ic =
	let res = ref [] in
	let _ =
		try
			while(true) do
				let line = input_line ic in
				let fields = split '\t' line in
				res := fields :: !res;
			done;
		with End_of_file -> ()
	in !res
	
let string2tokens s =
	let tokens = ref [] in
	let aux (Tokenizer.Token (s, _, _, _)) =
		tokens := s :: !tokens;
	in
	analyze aux (Lexing.from_string s);
	!tokens
;;

let topic2query fields =
	let id = List.nth fields 0 in
	let title = List.nth fields 1 in
	let desc = List.nth fields 2 in
	let tokens = string2tokens title in
	let tokens = (string2tokens desc) @ tokens in
	(id, tokens)

let index_mh indexdir limit =
	Printf.eprintf "token limit = %d\n" limit;
	let ii = Inverter.start_collection indexdir limit in
	let proc_doc id content =
(* Printf.printf "adding %s\n" id; *)
		let meta = DocMeta.empty () in
		let _ = DocMeta.add_string meta 0 id in
 
		let doc_handler = Inverter.start_doc ii meta in
		let i = ref 0 in
		let aux (Tokenizer.Token (s, _, _, _)) =
			Inverter.add_term ii doc_handler s !i;
			incr i;
		in
		analyze aux (Lexing.from_string content);
		Inverter.end_doc ii doc_handler
	in
	Mh_parser.parse stdin proc_doc;
	Inverter.end_collection ii
;;


(* tokiknak csak a cimet es desc mezojet hasznalja!! *)
let eval index_dir topicfile =
	let tic = open_in_bin (topicfile) in
	let topics = List.map topic2query (load_topics tic) in
	
	let ii = IndexReader.open_reader index_dir in
	let fi = ForIndex.open_reader index_dir in
	
	let process_topic (id, query) =
	  Printf.eprintf "%s " id;
		let result = Searcher.search2 ii fi query in
		let rec print_doc i l =
			if (i < 1000 ) then
				match l with
					| (docid, w) :: tail ->
						let di = ForIndex.doc_info fi docid in
						let meta = ForIndex.doc_meta fi di in
						Printf.printf "%s\tQ0\t%s\t%d\t%f\tSTANDARD\n" id (DocMeta.get_string meta 0) i w;
						print_doc (i + 1) tail;
					| _ -> ()
		in
		print_doc 0 result;
	in
	List.iter process_topic topics
;;

let usage () =
	Printf.eprintf "usage : %s [index|eval] index-dir topics \n" Sys.argv.(0)
;;

let _ =
	if (Array.length Sys.argv) < 2 then
		let _ = usage () in	exit 1
	else
		match Sys.argv.(1) with
			| "index" -> index_mh Sys.argv.(2) 100000000
			| _ -> eval Sys.argv.(2) Sys.argv.(3)
 
