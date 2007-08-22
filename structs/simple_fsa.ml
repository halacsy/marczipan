type transition = 
  { label : char ;
    mutable target : state;
    mutable weight : int;
  } and
	
state = 
  { id : int; 
    mutable is_final : bool; 

    mutable transitions : transition list
  }
	
	
type t = state


let empty_node id =
	{  id = id; is_final = false; transitions = []; }
	
	
let hash_fun_state state =
  let hash_fun_trans t =
  	((Hashtbl.hash t.label) * 31 + Hashtbl.hash t.target.id)  * 31 + (Hashtbl.hash t.weight)
  in
  
	let sum =   (if state.is_final then 1  else 0) in
	let add_trans_hash sum trans = 
	  sum + sum * 31 + (hash_fun_trans trans)
	in 
	List.fold_left (add_trans_hash) sum state.transitions
;;

(* ket allapot egyenlo, ha ugyanazt a nyelvet fogadjak el, vagyis a kimeno*)
(* eleknel eleg csak a fizikai egyenloseget vizsgalni *)

let equal_state s1 s2 =
	if s1.is_final <> s2.is_final then false else

	if List.length s1.transitions <> List.length s2.transitions then false else
	let rec compare_transitions l1 l2 =
	  (* two transition is equal iif they are labelled with the same character
       and point the same state
    *)
    let equal_transition t1 t2 =
      if t1.label = t2.label &&
         t1.weight = t2.weight &&
         t1.target.id = t2.target.id  then true 
      else false
    in
	  
	   match (l1, l2) with
		| [], [] -> true
		| h1 :: t1, h2 :: t2 -> 
		  
		  if equal_transition h1 h2 then compare_transitions t1 t2 
		  else false
		
		| _ -> false
	in
	compare_transitions s1.transitions s2.transitions
;;


let add_child n c child_node weight =
  let new_transition = 
    {label = c;
     target = child_node;
     weight = weight;
     }
    in
  (n.transitions <- new_transition :: n.transitions; child_node)

(* Not_found, ha nem lehet tovabbmenni *)
let advance state c = 
	let rec assoc  = function
    [] -> raise Not_found
  | h::l -> if h.label = c then h.target else assoc  l
	in
	assoc  state.transitions

let is_final state = state.is_final
   
let mem n s =
  (print_endline s;
   let l = String.length s in
   let rec aux n i =
     if i >= l
     then true
     else
       (try let n = advance n s.[i] in aux n (i + 1) with | Not_found -> false)
   in aux n 0)


(* van-e gyereke *)
let has_children n = 
  match n.transitions with
  h::t -> true
  | [] -> false


  module IntHash = Mfhash.Int

(* prints for graphviz *)
  let print_graphviz root =
  	Printf.printf("digraph finite_state_machine {\n");
  	Printf.printf("rankdir=LR;\n");

  	let printed = IntHash.empty() in
  	let rec print_child_nodes n =

      let print_edge t =
  			Printf.printf "%d -> %d [ label = \" %c-%d  \"];\n" n.id t.target.id t.label t.weight;
  			print_child_nodes t.target;
  		in
  		let print_node () =
  		  Printf.eprintf "printing %d\n" n.id;
  		  Printf.printf "%d [label = \"%d\"] ;\n" n.id  n.id;
  		  if n.is_final then
  		    Printf.printf "%d [shape = doublecircle] ;\n" n.id;

  		  List.iter print_edge n.transitions;

  		in
  		(* mindent csak egyszer irjunk ki, meg ha tobb ut is van hozza *)
      let duplum = ref false in
      let _ = IntHash.update 
                   (fun () -> duplum := false; n.id)
                   (fun id -> duplum := true; id)
                   printed n.id in
      if !duplum then () else
      print_node()       

  	in
  	print_child_nodes root;
  	Printf.printf "\nnode [shape = circle];";
  	Printf.printf "}\n"
  ;;

let iter f root =
	let print_acc (acc,w) =
		let acc = List.rev acc in
			f acc w
		in
	let rec aux (acc, w) node =
	 
		if node.is_final then print_acc (acc, w);
		List.iter (fun t -> aux ((t.label :: acc), w+t.weight) t.target) (List.rev node.transitions)
	in
	aux  ([],0) root


let iter_on_states f state =
    let visited = IntHash.empty () in
    let counter = ref (-1) in
    let rec do_state state =
  	  (* mindent csak egyszer irjunk ki, meg ha tobb ut is van hozza *)
      let duplum = ref false in
      let _ = IntHash.update 
                 (fun () -> duplum := false; incr counter; counter)
                 (fun id -> duplum := true; id)
                 visited state.id in
      if !duplum then 
        () 
      else begin
        f !counter state;
        List.iter (fun t -> do_state t.target) state.transitions
      end;
    in
    do_state state  
;;

