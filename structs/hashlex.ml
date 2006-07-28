module CHash = Hashtbl.Make(struct type t = string 
	let compare = compare 
	let hash = Hashtbl.hash 
	let equal = (fun x y -> compare x y = 0) end)


type 'a t = 'a CHash.t

let empty =  (CHash.create 130000 : int t)
	

let iter  f bt = CHash.iter f bt
let add  word data bt = CHash.replace bt word data; bt
let find  word bt = CHash.find bt word
	