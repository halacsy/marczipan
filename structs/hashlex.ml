module HashedString = struct
	type t = String.t
	let hash = Hashtbl.hash
	let equal (s1:string) (s2:string) = (s1 = s2)
end ;;