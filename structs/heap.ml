
type ('a, 'b) t = Empty | Node of 'b * 'a * ('a, 'b) t * ('a, 'b) t
 let empty = Empty
	
 let rec insert queue prio elt =
   match queue with
     Empty -> Node(prio, elt, Empty, Empty)
   | Node(p, e, left, right) ->
       if prio <= p
       then Node(prio, elt, insert right p e, left)
       else Node(p, e, insert right prio elt, left)

exception Queue_is_empty
 
let rec remove_top = function
     Empty -> raise Queue_is_empty
   | Node(prio, elt, left, Empty) -> left
   | Node(prio, elt, Empty, right) -> right
   | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
                     (Node(rprio, relt, _, _) as right)) ->
       if lprio <= rprio
       then Node(lprio, lelt, remove_top left, right)
       else Node(rprio, relt, left, remove_top right)

let consume = function
     Empty -> raise Queue_is_empty
   | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)

let consume_tops heap =
	let rec aux l h =
		let (prio, elt, tail) = consume h in
		match tail with
            Node(tprio, telt, _, _) as queue when prio = tprio -> 
                aux (elt::l) queue
		| _ -> (prio, elt::l, tail)
	in
	aux [] heap
(**
let _ =
	let heap = empty in
	let heap = insert heap 1 "a" in
	let heap = insert heap 2 "b" in
	let heap = insert heap 1 "e" in
	let heap = insert heap 2 "c" in
	let heap = insert heap 1 "a" in
	let (k, l, heap) = consume_tops heap in
	print_int k ; print_endline (String.concat " " l) ;
	let (k, l, heap) = consume_tops heap in
	print_int k ; print_endline (String.concat " " l) 
	
*)	