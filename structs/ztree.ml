
type 'a tree = Node of Int64.t * 'a  * 'a tree * 'a tree | Leaf

type compare_result = LE | GT | NE

let dimen = 2

let print  (x, y) =
   prerr_endline (Printf.sprintf "(%d, %d)" x y)
   
 let compare_coord axis d1 d2 =
   let (d11, d12), (d21, d22) = d1, d2 in
   if axis = 0 then
     compare d11 d21
   else
     compare d12 d22
   
 let kd_compare d1 d2 =  
   let (d11, d12), (d21, d22) = d1, d2 in
   if d11 <= d21 && d12 <= d22 then LE
   else if d11 > d21 && d12 > d22 then GT
   else NE


(* splits a list to two lists and the median. it preserves the order of the elements in the lists  *)
let split_list l len =
 let median = len / 2 in
 let rec aux l left med right ix  =
   match l with
     []   -> (left, med, right)
    | h::t -> if ix < median then
               aux t (h::left)  med right (succ ix)
              else if ix = median then
                (List.rev left, Some h, t) 
              else 
               failwith "nem lehet!"
               ;
 in
 let l, m, r = aux l [] None [] 0   in 
 match m with
  Some m -> l, m, r
  | None -> failwith("assert")

let create_from_list points =
  let resol = 30 in
  prerr_endline "calculating z codes";
  
  let points = List.rev_map (fun (point, data) -> 
    let z = (Zcodec.encode_int_tuple resol point) in
    
      (z, data )
    ) points in
  prerr_endline "sorting";
  
  let points = List.sort ((fun (p1, _) (p2, _) -> compare p1 p2 )) points in
  
 
  (* points is a ( zvalue, data) list *)
  let rec _create_tree_of_list points =
    let n = List.length points in
    if n = 0 then Leaf else

    let left, (med_point, med_value),  right = split_list points n in
      
    Node(med_point, med_value, (_create_tree_of_list left ), _create_tree_of_list right)
  in
  _create_tree_of_list points
;;

exception No_more_node

type 'a command = Walk of 'a tree | Check of 'a tree

let create_reader tree low high =
  let zlow = Zcodec.encode_int_tuple 30 low in
  let zhigh = Zcodec.encode_int_tuple 30 high in
  let queue = ref [( Walk (tree) )] in
            
  
  let rec next () =
    match !queue with
      | [] -> raise No_more_node
      | Check Leaf :: t -> failwith "hiba" 
      
      | Check (Node(z, value, _, _)) :: t -> 
         begin
          queue := t;
          let point = Zcodec.decode_int_tuple 30 z in
          if kd_compare low point = LE && kd_compare high point = GT then
             (point, value)
          else
            next ()
         end
      | Walk (Leaf) :: tail -> 
         begin
          queue := tail;
          next()
         end
      | Walk (Node(z, value, left, right) as n) :: tail ->
       
        begin
         if z < zlow then
           (queue := (Walk (right)) :: tail)
         else if z > zhigh then
           (
           queue := (Walk (left)) :: tail)
         else 
          (
           queue := (Walk (right)) :: tail;
           queue := (Check (n)) ::  !queue;
           queue := (Walk (left)) :: !queue;
           
          );
         next ()
        end
  in
  next
      
  
(* low incluse high exlusive ! *)
let query callback low high tree =
  Printf.printf "query\n";
  let zlow = Zcodec.encode_int_tuple 30 low in
  let zhigh = Zcodec.encode_int_tuple 30 high in
  
  let rec aux  node =
    match node with 
      Leaf -> ()
     | Node (z, value, left, right) -> 
       begin
         if z < zlow then
           aux right
         else if z > zhigh then
          aux left
         else 
          (
           aux left;
         
          let point = Zcodec.decode_int_tuple 30 z in
          if kd_compare low point = LE && kd_compare high point = GT then
          begin
            callback point value;
           
          end;
             aux right;
          )
       end
  in
  aux tree

let print_point = print

let print_tree node value_printer =
  let rec aux node level = match node with
    Node(zvalue, value, left, right) ->
    
              (
                let point = Zcodec.decode_int_tuple 30 zvalue in
                for i = 0 to level do
                    Printf.printf " "
                done;
                print_point point;
                print_string "->";
                value_printer value;
                print_newline ();
                aux left (level + 1);
                aux right (level + 1);
              )
    | Leaf -> begin
               for i = 0 to level do
                    Printf.printf " "
                done;
               Printf.printf "leaf\n";
               end
  in
  aux node 0

(*
let _ =
  let points = [((1,3), 2); ((2,2), 5)] in
  let tree = create_from_list points in
  let cb point value =
    Printf.printf "found:\n";
    print point;
    Printf.printf "\n"
  in
  query cb (1,2) (22,22) tree
  
*)