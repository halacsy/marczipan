open Simple_fsa
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

module Register = Mfhash.Make(
  struct type t = state 
  let equal s1 s2 = equal_state s1 s2 
  let hash = hash_fun_state end)
    
let empty () =
  (incr id_counter; empty_node !id_counter)




type t = 
  { 
            root : state; 
    mutable register : state Register.t ; 
    mutable wordno : int;
    mutable states : int;
    mutable total_transitions : int;
  }

let start () = 
  {  
    root = empty (); 
    register = Register.empty();  
    wordno = -1;
    states = 1;
    total_transitions = 0;
  }

let total_transitions builder = builder.total_transitions
  
let total_states builder = builder.states
  

  


let rec replace_or_register builder node = 
 
  (* lecserelei az utolso kimeno elet egy masik csomopontra mutatora 
      megjegyzem: csak itt dobunk el csomopontot a teljes epites soran,
    a h.target csomopontra nem mutat majd senki *)
  let replace_last_node n q =
    match n.transitions with 
      | h :: t -> begin
        
        builder.states <- pred builder.states;
        builder.total_transitions <- builder.total_transitions - (List.length h.target.transitions);
        h.target <- q; 
      end
      | _ -> ()
  in
  
  (* az utolso hozza adott el *)
 
  let last_trainsition = List.hd node.transitions in
  let last_node = last_trainsition.target 
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
	
exception Duplum of string
exception NotSorted of string

let last_weight state = match state.transitions with
  h::t -> h.weight
  | _  -> -1
;;

let add_word builder s =
  builder.wordno <- builder.wordno + 1;
  (* elso lepes: megkeressuk, hogy s-nek mekkora prefixe van mar az      *)
  (* automataban                                                         *)
  (* kozben jegyezzuk a path_cost-ot ide *)
  let l = String.length s in
  let path_cost = ref 0 in
  
  let rec common_prefix last_node i =
    if i >= l then (last_node, i)
    else
      try
       
        let next = advance last_node s.[i] in 
        path_cost := !path_cost + (last_weight last_node) ;
        common_prefix next (i + 1)
     
     with Not_found -> (last_node, i) 
  
  in
  
  let (last_node, i) = common_prefix builder.root 0
  in
    (* lastState is LastState, s[0..i-1] = commonPrefix *)
    if (i == l) && last_node.is_final
    then raise (Duplum s)
    else
      if i == l
      then raise (NotSorted s)
      else
          let _ = 
           if has_children last_node
           then replace_or_register builder last_node
           else ()
          in
         (* add suffix *)
         
          
         let rec aux last_node i  w  =
           if i >= l
           then last_node
           else
           (
              
              let new_child = add_child last_node s.[i] (empty ()) w in 
            (*  new_child.weight <- w;
              *)builder.states <- builder.states + 1;
               builder.total_transitions <- builder.total_transitions + 1;
              aux new_child (i + 1) 0;
           )  
           in
          let last_node = aux last_node i (builder.wordno - !path_cost)  in 
         last_node.is_final <- true;
         builder.wordno
        
;;
       


let stop builder  = 
   replace_or_register  builder builder.root;

;;

let fsa builder = builder.root
;;

let print_stat builder = 
  Printf.printf "number of nodes in register %d\n" (Register.size builder.register);
  Printf.printf "number of states %d\n" builder.states;
  Printf.printf "Total transitions: %d\n" builder.total_transitions;
  ()
  


