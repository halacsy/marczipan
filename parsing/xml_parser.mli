type parser 

val create : unit -> parser

(** Let the parser parse a chunk of an XML document. 
    @raise Expat_error error *)
val parse_string : parser -> string -> unit
val parse_channel : parser -> in_channel -> unit

(** {6 Start element setting and resetting} *)

val set_start_element_handler : parser -> 
  (string -> (string * string) list -> unit) -> unit 
val reset_start_element_handler : parser -> unit 

(** {6 End element setting and resetting} *)

val set_end_element_handler : parser -> (string -> unit) -> unit 
val reset_end_element_handler : parser -> unit
  

(** {6 Character data hander setting and resetting} *)

val set_character_data_handler : parser -> (string -> unit) -> unit 
val reset_character_data_handler : parser -> unit