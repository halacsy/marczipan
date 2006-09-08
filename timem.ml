type a = {t : float ; msg : string }
type t = {mutable events : a list}


let init () = {events = []}
	
let start t msg =
     t.events <- {t = Sys.time (); msg = msg} :: t.events

let stop t = match t.events with
    h :: tail -> prerr_float (Sys.time() -. h.t) ; prerr_char ' ' ; prerr_endline h.msg; t.events <- tail
	| _ -> failwith "too much end without enough start"
