(* Based on  @article{971842,
 author = {Jan Daciuk and Bruce W. Watson and Stoyan Mihov and Richard E. Watson},
 title = {Incremental construction of minimal acyclic finite-state automata},
 journal = {Comput. Linguist.},
 volume = {26},
 number = {1},
 year = {2000},
 issn = {0891-2017},
 pages = {3--16},
 doi = {http://dx.doi.org/10.1162/089120100561601},
 publisher = {MIT Press},
 address = {Cambridge, MA, USA},
 }
*)

let id_counter = ref (-1)

type transition = (char * state) and
state =
  { id : int; 
    mutable is_final : bool; 
    mutable edges : transition list
  }

	
let equal_transition (c1, t1) (c2, t2) = 
	if c1 = c2 && t1.id = t2.id then true else false
;;

let hash_fun_trans (c1, t1) =
	(Hashtbl.hash c1) * 31 + Hashtbl.hash t1.id;;

let hash_fun_state state =
	let sum = if state.is_final then 1 else 0 in
	let add_trans_hash sum trans = 
	  sum + sum * 31 + (hash_fun_trans trans)
	in 
	List.fold_left (add_trans_hash) sum state.edges
;;

(* ket allapot egyenlo, ha ugyanazt a nyelvet fogadjak el, vagyis a kimeno*)
(* eleknel eleg csak a fizikai egyenloseget vizsgalni *)

let equal_state s1 s2 =
	if s1.is_final <> s2.is_final then false else
	if List.length s1.edges <> List.length s2.edges then false else
	let rec aux l1 l2 = match (l1, l2) with
		| [], [] -> true
		| h1 :: t1, h2 :: t2 -> 
		  
		  if equal_transition h1 h2 then aux t1 t2 
		  else false
		
		| _ -> false
	in
	aux s1.edges s2.edges
;;


module Register = Mfhash.Make(
  struct type t = state 
  let equal s1 s2 = equal_state s1 s2 
  let hash = hash_fun_state end)
    
let empty () =
  (incr id_counter; {  id = !id_counter; is_final = false; edges = []; })

let add_child n c child_node =
  (n.edges <- (c, child_node) :: n.edges; child_node)

(* Not_found, ha nem lehet tovabbmenni *)
let advance state c = 
	let rec assoc  = function
    [] -> raise Not_found
  | (a,b)::l -> if a= c then b else assoc  l
	in
	assoc  state.edges

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

(* az utolso hozza adott el *)
let last_edge n =
  if (List.length n.edges) = 0 then raise Not_found else List.hd n.edges

(* van-e gyereke *)
let has_children n = (List.length n.edges) > 0
 
(* lecserelei az utolso kimeno elet egy masik csomopontra mutatora *)
let replace_last_node n q =
  match n.edges with | (ch, nh) :: t -> n.edges <- (ch, q) :: t | _ -> ()



type builder = { root : state; mutable register : state Register.t }

let create_builder = {  root = empty (); register = Register.empty(); }


let rec replace_or_register builder node =
  let (last_c, last_node) = last_edge node
  in
    if has_children last_node
     then replace_or_register builder last_node
     else ();
		let _ = 
		Register.update (fun () -> last_node)
										(fun q  -> replace_last_node node q; q)
										builder.register
										last_node
		in ()
	
let add_string builder s =
  (* elso lepes: megkeressuk, hogy s-nek mekkora prefixe van mar az      *)
  (* automataban                                                         *)
  let l = String.length s in
  let rec common_prefix last_node i =
    try
      if i >= l
      then (last_node, i)
      else (let next = advance last_node s.[i] in common_prefix next (i + 1))
    with | Not_found -> (last_node, i) in
  let (last_node, i) = common_prefix builder.root 0
  in
    (* lastState is LastState, s[0..i-1] = commonPrefix *)
    if (i == l) && last_node.is_final
    then Printf.eprintf "duplicated %s\n" s
    else
      if i == l
      then
        Printf.eprintf
          "input is not sorted, this input is a prefix of a previous one %s\n"
          s
      else
        (let _ =
           if has_children last_node
           then replace_or_register builder last_node
           else () in
         (* add suffix *)
         let rec aux last_node i =
           if i >= l
           then last_node
           else
             (let new_child = add_child last_node s.[i] (empty ())
              in aux new_child (i + 1)) in
         let last_node = aux last_node i in last_node.is_final <- true)


let end_of_words builder  = 
   replace_or_register  builder builder.root;
   builder.root
;;

module IntHash = Mfhash.Int

(* prints for graphviz *)
let print_graphviz root =
	Printf.printf("digraph finite_state_machine {\n");
	Printf.printf("rankdir=LR;\n");
 
	let printed = IntHash.empty() in
	let rec print_child_nodes n =
    
    let print_edge (c, n2) =
			Printf.printf "%d -> %d [ label = \" %c \"];\n" n.id n2.id c;
			print_child_nodes n2;
		in
		let print_node () =
		  if n.is_final then
		    Printf.printf "%d [shape = doublecircle] ;\n" n.id;
		    
		  List.iter print_edge n.edges;
		  n.id
		in
		(* mindent csak egyszer irjunk ki, meg ha tobb ut is van hozza *)
    let _ =
    IntHash.update (fun () -> print_node ())
                   (fun id -> id)
                    printed n.id
    in ()              
	
	in
	print_child_nodes root;
	Printf.printf ";\nnode [shape = circle];";
	Printf.printf "}\n"
;;



let dump root =
	let print_acc acc =
		let acc = List.rev acc in
		List.iter print_char acc;
		print_newline ()
	in
	let rec aux acc node =
		if node.is_final then print_acc acc;
		List.iter (fun (c, n) -> aux (c :: acc) n) node.edges
	in
	aux  [] root

