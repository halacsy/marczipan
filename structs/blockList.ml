external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"

type 'a block = {mutable content : 'a array;
				 mutable next: ('a block) option }
				
				
type 'a t = {first   : 'a block;
			 mutable last    : 'a block;
			 mutable ix      : int ;
			 default : 'a}

let next_size prev =  16
	
let create default =
	let first = {  content = Array.create (next_size 0) default; next = None} in
	{first = first ; last = first; ix = 0; default = default }
	
let add bl elt =
	if (Array.length bl.last.content) > bl.ix then
		let _ = unsafe_set bl.last.content bl.ix elt in
		bl.ix <- (bl.ix + 1)
	else
		let next = {content = Array.create (next_size 0) bl.default; next = None} in
		next.content.(0) <- elt;
		bl.ix <- 1;
		bl.last.next <- Some(next);
		bl.last <- next
		
let append bl1 bl2 =
	(* lezarjuk a bl1.last tombot *)
	bl1.last.content <- Array.sub bl1.last.content 0 bl1.ix;
	bl1.last.next    <- Some(bl2.first);
	bl1.last         <- bl2.last;
	bl1.ix           <- bl2.ix
	
	
let iter f bl =
	let rec aux block =
			
		match block.next with
		  None -> for i = 0 to (bl.ix -1) do f (unsafe_get block.content i) done
		| Some(next) -> Array.iter f block.content; 
			    		aux next;
	in
	aux bl.first
(*	
let _ = 
	let bl = create 0 in
		for i = 1 to 18 do
			add bl i
		done;
	let bl2 = create 0 in
	for i = 1 to 4 do
		add bl i
	done;
	
	append bl bl2;
	iter ( Printf.printf "%d " ) bl
*)
