type t
val init : string -> t
val find : t -> string -> int * int * int64

module Writer :
  sig
    type t
    val create : string -> t
    val close : t -> unit
    val add : t -> string -> int -> int -> int64 -> unit
  end
