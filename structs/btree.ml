
module Cmap = Map.Make(struct type t = string let compare = compare end)

  
type  'a t = 'a Cmap.t


let empty = Cmap.empty
	

let iter  f bt = Cmap.iter f bt
let add  word data bt = Cmap.add word data bt
let find  word bt = Cmap.find word bt
	