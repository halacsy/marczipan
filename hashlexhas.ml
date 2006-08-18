(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Hash tables *)







(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type  ('a, 'b) bucketnode =
  {mutable next: ('a, 'b) bucketlist;
   key : 'a ;
   mutable value : 'b }
and ('a, 'b) bucketlist =
    Empty
  | Cons of  ('a, 'b) bucketnode
;;

						
type ('a, 'b) t =
  { mutable size: int;                        (* number of elements *)
    mutable data: ('a, 'b) bucketlist array } (* the buckets *)


let create initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; data = Array.make s Empty }

let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty
  done;
  h.size <- 0


let eq_str s1 s2 =
   let l1 = String.length s1 in
   if l1 <> String.length s2 then
     false
   else
     try
       for i = 0 to pred l1 do
         if s1.[i] <> s2.[i] then raise Exit
       done;
       true
     with Exit -> false;;

let strcmp2 s1 s2 =
	let l1 = String.length s1 in
	let l2 = String.length s2 in
	if (l1 <> l2) then false else
	let rec strcmp_rec ix =
		if(ix = l1) then true else 	
		let res = (-) (int_of_char s1.[ix]) (int_of_char s2.[ix]) in
		if(res <> 0) then false else
		strcmp_rec (succ ix)
	in
	strcmp_rec 0

let hash= Hashtbl.hash

type hashed_string = { hash:int; s:string }

let hashed_string s = { hash = Hashtbl.hash s; s = s }

let compare_hashed_string s t = 
	   if (s.hash <> t.hash) then false else s.s = t.s
	
	
let strcmp hs1 hs2 =  compare_hashed_string hs1 hs2
	
let copy h =
  { size = h.size;
    data = Array.copy h.data }

let length h = h.size

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(node) ->
       f node.key.s node.value; do_bucket node.next in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let bucket2list b = 
	let rec aux node res = match node with
		Empty -> res
		| Cons(node) -> aux node.next ( (node.key.s, node.value) :: res)
	in
	aux b []
	
let siter f h =

   let d = h.data in
   for i = 0 to Array.length d - 1 do
     let datalist = bucket2list d.(i) in
	 let sdatalist = List.fast_sort (fun (key1,_) (key2,_) -> compare key1 key2) datalist in
	 List.iter f sdatalist ;
   done
	
let print_bucket_stat h =
	let hist = Hashtbl.create 20 in
	let d = h.data in
	begin
	 for i = 0 to Array.length d - 1 do
	     let len = List.length (bucket2list d.(i)) in
		 try
		 	incr (Hashtbl.find hist len) ;
		 with Not_found -> Hashtbl.add hist len (ref 1);
	   done ;
	   Hashtbl.iter  (fun k f -> Printf.eprintf "%d\t%d\n" k !f) hist ;
	end

let update h ke  update_fun  default_info =
	  let k = hashed_string ke in
	  let i = (k.hash) mod (Array.length h.data) in
	  let l = h.data.(i) in
	  match l with
		| Empty ->  h.data.(i) <-		Cons( {next = Empty; 
									  key = k; 
									  value = default_info} ) ;
									    h.size <- succ h.size;
									(*    if h.size > Array.length h.data lsl 1 then resize hash h ;
*)
		| Cons(node1) -> 
						let rec update_rec nodex = match nodex.next with
	                        | Empty -> nodex.next <- Cons( {next = Empty;  key = k; value = default_info} ) ;
									   h.size <- succ h.size;
								(*	if h.size > Array.length h.data lsl 1 then resize hash h		*)	
							| Cons(nodexx) ->
								if strcmp k nodexx.key  then
									begin
									(* update info *)
									nodexx.value <- (update_fun nodexx.value) ;
								    (* move front the node *)
									nodex.next <- nodexx.next ;
									nodexx.next <- Cons(node1) ;
									h.data.(i) <- Cons( nodexx) ;
								end
								else
									update_rec nodexx ;
						in
						if  strcmp k node1.key then
							begin
							(* data is at front *)
							node1.value <- (update_fun node1.value) ;
							end
						else 
							update_rec node1
	
		

						

