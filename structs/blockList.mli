module type BlockType =
	sig
		type t
		val default:   t
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

module Int : S with type elt = int
module Make (BT : BlockType) : S with type elt = BT.t
(** Functor building an implementation of the block list structure
   given a  type. *)
