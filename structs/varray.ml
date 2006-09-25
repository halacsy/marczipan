
type 'a t = {default: 'a ; mutable last : int; mutable buffer : 'a array  }

let create s def =
	if s > Sys.max_array_length then raise (Invalid_argument "create: too large array size");
	{default = def; last = -1; buffer = Array.create  s def}
	
let add va v =
	va.last <- succ va.last ;
	(* check the size of the buffer*)
	let osize = Array.length va.buffer in
	if (va.last == osize ) then 
		begin
		  (* double the length of the buffer *)
		  let nsize = min (2 * osize + 1) Sys.max_array_length in
		  if nsize == osize then raise (Out_of_memory)  ;
	      va.buffer <- Array.append va.buffer (Array.make (nsize-osize) va.default)
	 	end ;
	va.buffer.(va.last) <- v;
	va.last

(** [Varray.append va vb from ] add elements from [vb] to [va]. Is the same as
	[add va vb.(from); add va vb.(from + 1) ; ... ; va vb.(Length vb -1)]
	
	Feltetelezi, hogy van eleg hely!!
*)
let append va vb from =
	for x = from to vb.last do
		let _ = add va vb.buffer.(x) in ()
	done ;
	()

let append_slice va vb from t =
	for x = from to t do
		let _ = add va vb.buffer.(x) in ()
	done;
	()

let set va  a pos =
	if pos > va.last then raise (Invalid_argument "set: index out of bounds")
	else
	va.buffer.(pos) <- a
	
(** [Varray.apply a f n ] modifies array [a] in place, replacing
 element number [n] with [f a.(n)].

 Raise [Invalid_argument "index out of bounds"]
 if [n] is outside the range 0 to [Varray.length a - 1]. *)
let apply va f pos =
	if pos < 0 || pos > va.last then raise (Invalid_argument "apply: index out of bounds")
	else
	va.buffer.(pos) <- f va.buffer.(pos)

(** [Varray.get a n] returns the element number [n] of array [a].
   The first element has number 0.
   The last element has number [Varray.length a - 1].

   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [(Varray.length a - 1)]. *)	
let get va pos =
	if pos < 0 || pos > va.last then raise (Invalid_argument "index out of bounds")
	else
	va.buffer.(pos)

let iter f va =
	if va.last >= 0 then
	for i = 0 to (va.last ) do
		f va.buffer.(i)
	done 
	
let length va = (va.last + 1)


let to_array va =
	Array.sub va.buffer 0 (va.last + 1)

(** C es Java tipusu iterator. Egy fuggvenyt ad vissza, ami elemenkent adja vissza a tombot *)

let citer va = 
	let i = ref 0 in 
	(fun () -> 
		let n = (get va !i) in begin
			incr i; 
			n
		end
	)
		 
(*
let _ = 
	for j = 1 to 3 do
	let v = create 1 0 in
	let i = add v 1 in
	let i = add v 2 in
	let i = add v 3 in
	let ci = citer v in
	print_int (ci ());
print_int (ci ());
print_int (ci ())
done
*)
(*	
let test_varray = begin
	let va = create 100000 0 in
	for i = 0 to ( Sys.max_array_length / 2 ) do
		add va i ;
	done ;
	let sum = ref 0 in
	let add x = sum:=!sum+ x in
	iter add va;
	print_string "varray" ;
	print_int !sum;
	print_newline ();
	flush_all	
end
*)
(*

let test_list = 
begin
	let va = ref [] in
	for i = 0 to ( Sys.max_array_length / 2 ) do
		va := i::!va
	done ;
	let sum = ref 0 in
	let add x = sum:=!sum+ x in
	List.iter add (List.rev !va);
	print_string "list" ;
	print_int !sum;
	print_newline ();
	flush_all
end
*)
(*
	let test_array ()= begin
	let va = Array.create (Sys.max_array_length / 2 +1) 0 in
	for i = 0 to (Array.length va) - 1 do
		va.(i) <- i ;
	done ;
	let sum = ref 0 in
	let add x = sum:=!sum+ x in
	Array.iter add va;
	print_string "array" ;
	print_int !sum;
	print_newline ();
	flush_all	
end

let _ = 
		
			test_varray;
*)
