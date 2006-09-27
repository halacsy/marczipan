type a = {id : int; t : float ; msg : string }
type t = {mutable last_id : int ; mutable events : a list}


let init () = {last_id = 1; events = []}
	
let start s msg =
	Printf.eprintf "%d. %s started\n" s.last_id msg;
	s.events <- {id = s.last_id; t = Sys.time (); msg = msg} :: s.events;
	s.last_id <- s.last_id + 1;;


let ielapsed t = match t.events with
	    h :: tail ->  ((Sys.time() -. h.t),  h.msg);
		| _ -> failwith "too much end without enough start"

let istop t = match t.events with
    h :: tail ->  t.events <- tail;
 					((Sys.time() -. h.t),  h.msg, h.id);
	| _ -> failwith "too much end without enough start"

let finish t =
	let (time, s, id) = istop t in
	Printf.eprintf "%d. %s finished: %f\n" id s time
	
let finish_speed t n unit =
	let (time, s, id) = istop t in
	let speed =	(float_of_int n) /. time   in
	Printf.eprintf "%d. %s finished: %f doing %d %s; speed = %f %s/sec\n" id s time n unit speed unit
		
	
