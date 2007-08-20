open Fsa_builder
open Simple_fsa
module IntHash = Mfhash.Int
  
(* This format is coming from Gyorgy Gyepesi. *)
(* All state have an integer id 0..N-1 where N is the number of states.*)
(* We enumerate all states and all edges in vectors.*)
(* If we have E transitions we have 3 * E length vector:*)
(* 1. the state the transition goes *)
(* 2. the label on the transition*)
(* 3. the weight of the transition *)
(* *)
(* The transitions of state S are stored continuously. The index of the beginning of the transitions*)
(* of state i is in a vector called state_transtions.*)
(* *)
(* The transitions of the state I can be found in the range of*)
(* state_transitions.(i) .. state_transitions.(i+1) -1 *)

type t = {
  transition_targets : int array;
  transition_labels  : char array;
  transition_weights : int array;
  state_transitions  : int array;
  state_finals       : bool array;
  start_state        : int;
}

let create start_state states trans =
	let fsa = {
   transition_targets = Array.make trans 0 ; 
   transition_labels = Array.make trans ' ' ;
   transition_weights = Array.make trans 0 ;
   state_transitions  = Array.make (states+1) 0 ;
   state_finals       = Array.make states false ;
   start_state        = 0; 
  } in 
  fsa
;;

let total_states fsa = Array.length fsa.state_finals

let total_transitions fsa = Array.length fsa.transition_targets

let compact builder =
  let trans  = Fsa_builder.total_transitions builder in
  let states =  Fsa_builder.total_states builder in
	let fsa = create 0 states trans in
  let state_id_map = IntHash.empty() in
  (* hova raktunk utoljara elt *)
  let last_transition = ref 0 in
    
  let process_state ix state =
    IntHash.add_or_replace state_id_map state.id ix;
    
    let add_transition t =
      (* ez meg nagyon nem jo *)
      fsa.transition_targets.(!last_transition) <- t.target.id;
      fsa.transition_labels.(!last_transition) <- t.label;
      fsa.transition_weights.(!last_transition) <- t.weight;
      incr last_transition;
    in
    fsa.state_transitions.(ix) <- !last_transition;
    fsa.state_finals.(ix) <- state.is_final;
    List.iter add_transition (List.rev state.transitions)
  in  
  Simple_fsa.iter_on_states process_state builder.root;
  
  (* hogy az utolso allapotnak is tudjuk a kimeno eleit *)
  fsa.state_transitions.(states) <- !last_transition;
 
  (* now translate state_id -s to indexes*)
  
  for i = 0 to trans - 1 do
    let id =  fsa.transition_targets.(i) in
    fsa.transition_targets.(i) <- IntHash.find state_id_map id;
  done;
  fsa
    

let is_final fsa state = fsa.state_finals.(state)
  
  
let select_transition fsa q c =
 
  let rec bin_search low high  =
    if low > high then raise Not_found
    else
      let mid = (low + high) lsr 1 in
      let cmp = compare fsa.transition_labels.(mid) c in
      if cmp < 0 then
        bin_search (succ mid) high
      else if cmp > 0 then
        bin_search low (pred mid)
      else
        mid
  in  
  let i = fsa.state_transitions.(q) in
  let j = fsa.state_transitions.(q+1) in   
  let x = bin_search i (j - 1) in
  (* x is the transition we can follow *)
  x
;;
  
(* sava the fsa to the output channel in binary format *)
let save_binary fsa oc =
 Io.output_vnatint oc (fsa.start_state);
 Io.output_vnatint oc (Array.length fsa.state_finals );
 Io.output_vnatint oc ((Array.length fsa.transition_labels)  );

 let print_state state =
    let i = fsa.state_transitions.(state) in
    let j = fsa.state_transitions.(state + 1) in
		
		(* only one trick we use: since the number of transitions used*)
		(* to be small we shift it and the last bit signs whether the*)
		(* state is final *)
    let t = j -i in
    let t = (t lsl 1) + (if fsa.state_finals.(state) then 1 else 0) in
    Io.output_vnatint oc t;
      
     for x =  i to (j -1) do
		   Io.output_vnatint oc fsa.transition_targets.(x);
       output_char oc fsa.transition_labels.(x);
       Io.output_vnatint oc fsa.transition_weights.(x);

     done;
  in
     
 for i = 0 to  (Array.length fsa.state_finals  - 1) do
  print_state i;
 done

let load_binary ic =
  let start_state = Io.input_vnatint ic in
	let state_count = Io.input_vnatint ic in
	let transition_count = Io.input_vnatint ic in
	let fsa = create start_state state_count transition_count in
	let ix = ref 0 in
	let load_state state =
		let t = Io.input_vnatint ic in
		let final = if (t mod 2) = 1 then true else false in
		fsa.state_transitions.(state) <- !ix;
    fsa.state_finals.(state) <- final;
		
		let t = (t lsr 1)  in
		
		for i = 1 to t do
			
			(* minden kimeno elere *)
			fsa.transition_targets.(!ix) <- (Io.input_vnatint ic);
      fsa.transition_labels.(!ix) <- (input_char ic);
      fsa.transition_weights.(!ix) <- (Io.input_vnatint ic);
      incr ix;
		done; 
	in
	for i = 0 to state_count -1 do
		load_state i;
	done;
	fsa.state_transitions.(state_count) <- !ix;
	fsa
	
  
let word2index fsa w =
 
  let l = String.length w in
  let rec advance i state acc =
 
    if i >= l then 
      if is_final fsa state then acc
      else raise Not_found
    else
      let t = select_transition fsa state  w.[i] in
      let w = fsa.transition_weights.(t) in
      let s = fsa.transition_targets.(t) in
      advance (succ i) s (acc + w)
  in
  advance 0 (fsa.start_state) 0 
	
	
(* a teljes lexikonon vegigmegy, visszaadja a szavakat betu listakent es az indexuket *)
let iter f fsa =
	let print_acc (acc,w) =
		let acc = List.rev acc in
		f acc w
	in
	let rec aux acc w state =
		if  (is_final fsa state) then print_acc (acc, w);
		
		let i = fsa.state_transitions.(state) in
    let j = fsa.state_transitions.(state + 1) in   
		for x = i to j - 1 do
			let label = fsa.transition_labels.(x)	in
			let weight = fsa.transition_weights.(x) in
			let target = fsa.transition_targets.(x) in
			aux (label :: acc) (w + weight) target
		done;
	in
	aux [] 0 fsa.start_state