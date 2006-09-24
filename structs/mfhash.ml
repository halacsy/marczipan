
module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
	val compare: t -> t -> int
  end


module type S =
  sig
    type keyt
    type 'a t
  	val create : int -> 'a t
	val clear : 'a t -> unit
	val copy :  'a t -> 'a  t
	val iter : (keyt -> 'a -> unit) -> 'a  t -> unit
	val sorted_iter : (keyt -> 'a -> unit) -> 'a t -> unit 
(* val print_bucket_stat : 'a t -> unit *)
	val find : 'a t -> keyt -> 'a
	val update : 'a  t ->  'a  -> keyt  -> ('a -> 'a) -> unit  
	val size : 'a t -> int
  end

module Make(H: HashedType): (S with type keyt = H.t) =
  struct
    type keyt = H.t


(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type  ('b ) bucketnode =
  {mutable next: ( 'b) bucketlist;
   key : keyt ;
   mutable value : 'b }
and ( 'b) bucketlist =
    Empty
  | Cons of  ('b) bucketnode
;;

						
type ( 'b) t =
  { mutable size: int;                        (* number of elements *)
    mutable data: ( 'b) bucketlist array } (* the buckets *)

let hash = H.hash 
	
let create initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; data = Array.make s Empty }


let size h = h.size
	
let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty
  done;
  h.size <- 0



let str_eq  = H.equal
		
let copy h =
  { size = h.size;
    data = Array.copy h.data }

let length h = h.size

let resize hashfun tbl =

	begin
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if nsize <> osize then begin
    let ndata = Array.create nsize Empty in
    let rec insert_bucket = function
        Empty -> ()
      | Cons(onode) ->
         (* preserve ordering, insert first the first *)
		 begin
          let nidx = (hashfun onode.key) mod nsize in
          match ndata.(nidx) with
			(* this is empty bucket *)
	 		Empty -> ndata.(nidx) <- 	Cons( {next = Empty; 
										  key = onode.key; 
										  value = onode.value} ) ;
			| Cons(node) ->
				(* insert it to the tail *)
				let rec aux prev  =
					 match prev.next with
					Empty -> prev.next <- Cons ({next = Empty; key = onode.key; value = onode.value});
                    | Cons(nnode) -> aux nnode ;
				in
				aux node ;
		  
		 insert_bucket onode.next ;
		end
	in			
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
	
  end
end

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(node) ->
       f node.key node.value; do_bucket node.next in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done


 	
let sorted_iter f h =

   let add2list l b = 
		let rec aux node res = match node with
			Empty -> res
			| Cons(node) -> aux node.next ( (node.key, node.value) :: res)
		in
		aux b l
	in
	let datalist = Array.fold_left add2list [] h.data in
	 let sdatalist = List.fast_sort (fun (key1,_) (key2,_) -> H.compare key1 key2) datalist in
	 List.iter (fun (key, value) -> f key value) sdatalist   

(*	
let print_bucket_stat h =
	let hist = Hashtbl.create 20 in
	let d = h.data in
	begin
	 for i = 0 to Array.length d - 1 do
	     let len = List.length (bucket2list d.(i)) in
		 try
		 	incr (Hashtbl.find hist len) ;
		 with Not_found -> Hashtbl.add hist len (ref 1);
	   done ;
	   Hashtbl.iter  (fun k f -> Printf.eprintf "%d\t%d\n" k !f) hist ;
	end
*)
let find h k   =
	  let i = (hash k) mod (Array.length h.data) in
	  let l = h.data.(i) in
	  match l with
		| Empty ->  raise Not_found 
		| Cons(node1) -> 
						let rec find_rec nodex = match nodex.next with
	                        | Empty -> raise Not_found
							| Cons(nodexx) ->
								if str_eq k nodexx.key  then
									(
									(* move front the node *)
									nodex.next <- nodexx.next ;
									nodexx.next <- Cons(node1) ;
									h.data.(i) <- Cons( nodexx) ;
									
									nodexx.value
								)
								else
									find_rec nodexx ;
						in
						if  str_eq k node1.key then
							node1.value 
						else 
							find_rec node1
	

	
let update h  default_info k  update_fun  =
	 let i = (hash k) mod (Array.length h.data) in
	 let l = h.data.(i) in
	 match l with
	| Empty ->  h.data.(i) <- Cons( {next = Empty;  key = k; value = update_fun default_info} ) ;
								    h.size <- succ h.size;
								    if h.size > Array.length h.data lsl 1 then resize hash h ;
	
	| Cons(node1) -> 
		let rec update_rec nodex = match nodex.next with
                     | Empty -> nodex.next <- Cons( {next = Empty;  key = k; value = update_fun default_info} ) ;
					   h.size <- succ h.size;
					if h.size > Array.length h.data lsl 1 then resize hash h			
			| Cons(nodexx) ->
				if str_eq k nodexx.key  then
					begin
					(* update info *)
					nodexx.value <- (update_fun nodexx.value) ;
				    (* move front the node *)
					nodex.next <- nodexx.next ;
					nodexx.next <- Cons(node1) ;
					h.data.(i) <- Cons( nodexx) ;
				end
				else
					update_rec nodexx ;
		in
		if  str_eq k node1.key then
			begin
			(* data is at front *)
			node1.value <- (update_fun node1.value) ;
			end
		else 
			update_rec node1

end