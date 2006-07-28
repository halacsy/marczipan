
type  'a t =
    Node of string * 'a *  'a t * 'a t
   | Leaf

let empty = Leaf 

let rec update key updatef defdata bst = match bst with
	    Leaf -> Node (key, defdata, Leaf, Leaf)
	  | Node (y, datay, left, right) ->
	     if key < y then
	        Node (y, datay, update key updatef defdata left, right)
	     else if key > y then
	        Node (y, datay, left, update key  updatef defdata right)
	     else
	        Node (y, (updatef datay), left, right);;
		
let rec add key data bst = match bst with
    Leaf -> Node (key, data, Leaf, Leaf)
  | Node (y, datay, left, right) ->
     if key < y then
        Node (y, datay, add key data left, right)
     else if key > y then
        Node (y, datay, left, add key data right)
     else
        Node (y, data, left, right);;

let rec find key bst = match bst with 
	Leaf -> raise Not_found
   | Node (y, data, left, right) ->
       if key < y then
	       find key left
	   else if key > y then
		  find key right
	   else (* x = y *)
	           data
let rec iter f bst = match bst with
	Leaf -> ()
	| Node (key, data, left, right) -> iter f left ; f key data ; iter f right ;

	