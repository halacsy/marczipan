
type t
module type CodecType =
  sig
	type t 
	
	val zero : t
	val one : t
	val shift_left : t -> int -> t
	val shift_right : t -> int -> t
	val succ : t -> t
	val pred : t -> t
	val of_int : int -> t
	val to_int : t -> int
	val add : t -> t -> t
	val sevenf : t
	val logand : t -> t -> t
  end

module  Int64CodecType =
struct
	type t = Int64.t
	let zero = Int64.zero
	let one = Int64.one
	
	let shift_left = Int64.shift_left
	let shift_right = Int64.shift_right
	let succ = Int64.succ
	let pred = Int64.pred
	let of_int  = Int64.of_int
	let to_int  = Int64.to_int
	let add = Int64.add
	let sevenf = 127L
	let logand = Int64.logand
end

module type S =
sig
	type t
	val input_vbyte : (unit -> int) -> t
	val output_vbyte : (int -> unit) -> t -> unit		
end

module Make (CT: CodecType)  =
struct
type t = CT.t

let input_vbyte f =
	let rec add_next_byte v i s =
		match (i land 0x80) with 
		| 0x80  ->	let i = f () in
					(* a legnagyobb bit most nem kell, csak az also het bit-> 7F = 111 1111 *)
					let n = CT.of_int ((i land 0x7F) + 1) in
					(* Printf.printf "n=%d\n" (to_int n); *)
					(* s = hanyadik bajtot olvassuk: mivel elol a legkisebb
						bitek voltak, mindig el kell tolni, amit olvastunk *)
					let v = CT.add  v   (CT.shift_left n (s * 7)) in
					(* Printf.printf "v= %d\n" (to_int v); *)
                    add_next_byte v i (s+1)
		| _ -> v
	in
    (add_next_byte CT.zero 0x80 0 )
;;

let output_vbyte f v =
	let rec output_next_byte v =	
		(* jobb 7 bitet vesszuk *)
		let b = CT.to_int (CT.logand v CT.sevenf) in
        let r = CT.shift_right v 7 in
	(*	Printf.printf "b=%d r=%d\n" b (to_int r); *)
		(* ha nincs tobb, akkor kiirjuk b-t, amiben egy 0 utan koveti az utolso 7 bitet *)
		if r = CT.zero then
			f (b)
		else begin
		(* van meg cucc, akkor a 7bit elo irunk egy 1-t es folytatjuk *)
			f (128 lor b);
			output_next_byte (CT.pred r) 
		end
		in
	output_next_byte (CT.pred v)

end

module Int64 = Make(Int64CodecType)
 
