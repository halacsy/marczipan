external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"

module type BlockType =
	sig
		type t
		val default:  t
	end

module type S =
 sig 
	type t
	type elt
	val create : unit -> t
	val add : t -> elt -> unit
	val append : t -> t -> unit
	val iter : (elt -> unit) -> t -> unit
	
	type stream 	
	val open_stream :  t -> stream
	val next   :  stream -> elt
 	exception End_of_stream;;

 end

module IntType =
struct
	type t = int
	let default = 0 ;;
end

module Make (BT: BlockType)  =
struct
type elt = BT.t 

type  block = {mutable content : elt array;
				 mutable next: block option }
				
				
type  t = {first   :  block;
			 mutable last    :  block;
			 mutable ix      : int ;
			 mutable len     : int }

let next_size prev =  16
	
let default = BT.default
	
let create () =
	let first = {  content = Array.create (next_size 0) default; next = None} in
	{first = first ; last = first; ix = 0; len = 0; }
	
let add bl elt =
	let cur_size = Array.length bl.last.content in
	if cur_size > bl.ix then
		(* have enough space in this block *)
		let _ = unsafe_set bl.last.content bl.ix elt in
		bl.ix <- (bl.ix + 1)
	else
		(* create new block *)
		let next = {content = Array.create (next_size cur_size) default; next = None} in
		next.content.(0) <- elt;
		bl.ix <- 1;
		bl.last.next <- Some(next);
		bl.last <- next
		
let append bl1 bl2 =
	(* lezarjuk a bl1.last tombot *)
	bl1.last.content <- Array.sub bl1.last.content 0 bl1.ix;
	bl1.last.next    <- Some(bl2.first);
	bl1.last         <- bl2.last;
	bl1.ix           <- bl2.ix;
	bl1.len          <- bl1.len + bl2.len
;;
let iter f bl =
	let rec aux block =
			
		match block.next with
		  None -> for i = 0 to (bl.ix -1) do f (unsafe_get block.content i) done
		| Some(next) -> Array.iter f block.content; 
			    		aux next;
	in
	aux bl.first


type  stream = {mutable cur_block      :  block;
 			 mutable inner_block_ix : int;
			 mutable remain         : int}

exception End_of_stream;;

			
let rec next s =  
		if s.remain = 0 then raise End_of_stream;
		s.remain <- s.remain - 1;
		if (Array.length s.cur_block.content) > s.inner_block_ix then
			let elt = unsafe_get s.cur_block.content s.inner_block_ix  in
			let _ = s.inner_block_ix <- (s.inner_block_ix + 1) in
			elt
		else
			match s.cur_block.next with
	  			None ->  raise End_of_stream; 
				| Some(next_block) -> s.cur_block <- next_block; s.inner_block_ix <- 0; next s	
;;

let open_stream bl = {cur_block = bl.first; inner_block_ix = 0; remain = bl.len} ;;
end;;

module Int = Make(IntType)

(*
let _ = 
	let bl = create () in
		for i = 1 to 18 do
			add bl i
		done;
	let bl2 = create () in
	for i = 1 to 4 do
		add bl i
	done;
	
	append bl bl2;
	iter ( Printf.printf "%d " ) bl;
*)
(*
	let stream = Stream.create bl in
	let rec loop () = 
		Printf.printf "%d \n" (Stream.next stream);
		loop ()
	in
	try
		loop ()
	with Stream.End_of_stream -> ()

	
*)