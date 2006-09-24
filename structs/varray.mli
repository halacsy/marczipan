type 'a t
val create : int -> 'a -> 'a t
val add : 'a t -> 'a -> int
val apply : 'a t -> ('a -> 'a) -> int -> unit
val get : 'a t -> int -> 'a
val iter : ('a -> 'b) -> 'a t -> unit
val length : 'a t -> int
val to_array : 'a t -> 'a array
val citer : 'a t -> (unit -> 'a)
val append : 'a t -> 'a t -> int -> unit
val append_slice : 'a t -> 'a t -> int -> int -> unit
val set : 'a t -> 'a -> int -> unit