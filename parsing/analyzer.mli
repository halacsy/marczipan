module Hash :
  sig
    type key = Mfhash.HashedString.t
    type 'a t = 'a Mfhash.Make(Mfhash.HashedString).t
    val empty : unit -> 'a t
    val create : ?move_to_front:bool -> ?do_resizing:bool -> int -> 'a t
    val clear : 'a t -> unit
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
    val to_list : 'a t -> (key * 'a) list
    val sorted_iter :
      (key -> key -> int) -> (key -> 'a -> unit) -> 'a t -> unit
    val print_bucket_stat : 'a t -> unit
    val find : 'a t -> key -> 'a
    val find_or_add : 'a t -> key -> 'a -> 'a
    val add_or_replace : 'a t -> key -> 'a -> unit
    val update : (unit -> 'a) -> ('a -> 'a) -> 'a t -> key -> 'a
    val update_all : 'a t -> (key -> 'a -> 'a) -> unit
    val size : 'a t -> int
    val array_size : 'a t -> int
  end
val stopwords : string Hash.t
val iterate : (Tokenizer.token -> unit) -> Lexing.lexbuf -> unit
val iterate_ngram : (Tokenizer.token -> unit) -> Lexing.lexbuf -> unit
val iterate_ngram2 : int -> (Tokenizer.token -> unit) -> Lexing.lexbuf -> unit