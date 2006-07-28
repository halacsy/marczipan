 module Cmap = Map.Make(struct type t = string let compare = compare end) 
(* module Cmap = Bst *)

  
type  'a t =  TrieNode of  'a t array  * 'a option |  Container of 'a  Cmap.t * int * 'a option


let empty = Container(Cmap.empty, 0, None)
	
let char2int c = Char.code c 
let int2string i = 
	let c = Char.chr i in
	let s = String.create 1 in
	s.[0] <- c;
	s
		
let nullid = 0 

let burst_container (Container (map, size, x) as node) =
	if size < 100 then node
	else
	
		let (TrieNode(pointers, _) as new_trie_node) = TrieNode((Array.create 256 empty), x ) in
		let add word data =
		 
			let leading = word.[0] in
			let index = char2int(leading) in
			
				
			let len = String.length word in
			if len == 1 then
				pointers.(index) <- Container(Cmap.empty, 1, Some(data))
			else
				let tail =  String.sub word 1 ((String.length word) - 1) in

					let updated = 
						match pointers.(index) with
                            | (Container(map, size, x)) -> Container((Cmap.add tail data map), (succ size),  x)
							| _ -> failwith "mi van?"
						in
					pointers.(index) <- updated
		in
		(*	Printf.printf "bursting\n"; *)
		Cmap.iter add map ;
		new_trie_node

let iter  f bt =
	
	let rec iter bt f prefix =
		
	
		let visit node = match node with
	
		| TrieNode(_, Some(data)) | Container(_, _, Some(data)) -> f prefix data 
		| _ -> () 
		in
		let traverse node = match node with
		| TrieNode(pointers, _) -> Array.iteri (fun i node ->  iter node f (prefix ^ (int2string i))) pointers 
		| Container(map, _, _) -> let aux suffix data =  f (prefix ^ suffix) data in Cmap.iter aux map
		in
		visit bt  ; 
		traverse bt

	in iter bt f ""
(*
	let update  word updatef defdata bt =

		let len =  String.length word in

		let suffix depth =
			String.sub word depth (len - depth) 
		in

		let rec update node depth =
			if depth == len then
				match node with
					(TrieNode(pointers, None)) -> TrieNode(pointers, Some(defdata))
				 | 	(TrieNode(pointers, Some(olddata))) -> TrieNode(pointers, Some(updatef olddata))
				 | (Container(map, size, None)) -> Container(map, size, Some(defdata))
				 |  (Container(map, size, Some(olddata))) -> Container(map, size, Some(updatef olddata))
			else
				match node with
					(TrieNode(pointers, x)) -> let index = char2int (word.[depth]) in
											let child = pointers.(index) in
											let new_child = update child (succ depth) in
											pointers.(index) <- new_child;
											TrieNode(pointers, x)
				| (Container(map, size, x)) ->  burst_container (Container(Cmap.update (suffix depth) updatef defdata map, (succ size),  x))
		in
		update bt  0
	*)	
	
let add  word data bt =
	
	let len =  String.length word in
	
	let suffix depth =
		String.sub word depth (len - depth) 
	in
	
	let rec add node depth =
		if depth == len then
			match node with
				(TrieNode(pointers, _)) -> TrieNode(pointers, Some(data))
			 |  (Container(map, size, _)) -> Container(map, size, Some(data))
		else
			match node with
				(TrieNode(pointers, x)) -> let index = char2int (word.[depth]) in
										let child = pointers.(index) in
										let new_child = add child (succ depth) in
										pointers.(index) <- new_child;
										TrieNode(pointers, x)
			| (Container(map, size, x)) ->  burst_container (Container(Cmap.add (suffix depth) data map, (succ size),  x))
	in
	add bt  0


	
let find  word bt =
	let len =  String.length word in
	
	let suffix depth =
		String.sub word depth (len - depth) 
	in
	
	let rec find node depth =
		if depth == len then
			match node with
				(TrieNode(_, Some(value))) -> value
			 |  (Container(_, _, Some(value))) -> value
			 |  _ -> raise Not_found
		else
			match node with
				(TrieNode(pointers,_)) -> let index = char2int (word.[depth]) in
										let child = pointers.(index) in
										find child (succ depth) 
			| (Container(map, _, _)) -> Cmap.find (suffix depth) map
	in
	find bt  0
