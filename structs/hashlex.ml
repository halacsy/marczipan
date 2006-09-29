module HashedString = struct
	type t = String.t
	let hash w s = (Hashtbl.hash w) mod s
	let equal (s1:string) (s2:string) = (s1 = s2) 
	let compare (s1:string) (s2:string) = String.compare s1 s2
end ;;