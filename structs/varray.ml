
type 'a t = {default: 'a ; mutable last : int; mutable buffer : 'a array  }

let create s def ={default = def; last = -1; buffer = Array.create  s def}
	
let add va v =
	va.last <- succ va.last ;
	(* check the size of the buffer*)
	let osize = Array.length va.buffer in
	if (va.last == osize -1) then 
		begin
		  (* double the length of the buffer *)
		  let nsize = min (2 * osize + 1) Sys.max_array_length in
		  if nsize == osize then raise (Out_of_memory)  ;
	      va.buffer <- Array.append va.buffer (Array.make (nsize-osize) va.default)
	 	end ;
	va.buffer.(va.last) <- v
	

let iter f va =
	if va.last >= 0 then
	for i = 0 to (va.last ) do
		f va.buffer.(i)
	done 
	
let size va = (va.last + 1)
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