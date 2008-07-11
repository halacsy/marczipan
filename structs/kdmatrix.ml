type compare_result = LE | GT | NE
  
module type Point =
 sig 
    type t
    val dimen : int
    (* for debuggin reason *)
    val print : t -> unit 
    val compare_coord : int -> t -> t -> int
   
  
    val kd_compare : t -> t -> compare_result
 end

module type S =
  sig
    type point
    type 'a t
    val create_from_list : (point * 'a) list -> 'a t
    val query : (point -> 'a -> unit) -> point -> point -> 'a t -> unit
    val print_tree : 'a t -> ('a -> unit)-> unit
  end
  
module Make (M : Point) : (S with type point = M.t ) =
struct

type point = M.t

type 'a t  = Node of point * 'a * 'a t * 'a t | Nil

let dimen = M.dimen
let compare_coord = M.compare_coord
let print_point = M.print
let kd_compare = M.kd_compare

(* splits a list to two lists and the median. it reverses the elements in the lists  *)
let split_list l len =
 let median = len / 2 in
 let rec aux l left med right ix  =
   match l with
     []   -> (left, med, right)
    | h::t -> if ix < median then
               aux t (h::left)  med right (succ ix)
              else if ix = median then
               aux t left (Some h) right (succ ix) 
              else 
               aux t left med (h::right)  (succ ix)
 in
 let l, m, r = aux l [] None [] 0   in 
 match m with
  Some m -> l, m, r
  | None -> failwith("assert")

let rec _create_tree_of_list points level =
  let n = List.length points in
  if n = 0 then Nil else
  let axis = level mod dimen in
  let points' = List.sort (fun (p1, _) (p2, _) -> compare_coord axis p1 p2) points in
  let left, (med_point, med_value),  right = split_list points' n in
  Node(med_point, med_value, (_create_tree_of_list left (level + 1)), _create_tree_of_list right (level +1))



let create_from_list points = _create_tree_of_list points 0

  
let print_tree node value_printer =
    let rec aux node level = match node with
      Node(point, value, left, right) ->
                (
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
      | Nil -> () 
    in
    aux node 0
        

(* low incluse high exlusive ! *)
let query callback low high tree =
  let rec aux level node =
    match node with 
      Nil -> ()
     | Node (point, value, left, right) -> 
       begin
        
      
          
        let axis = level mod dimen in
        
        if not (compare_coord axis low point > 0) then
          aux  (level + 1) left;
        
        if kd_compare low point = LE && kd_compare high point = GT then
          callback point value;

        if not (compare_coord axis high point < 0) then
          aux (level + 1) right;
       end
  in
  aux 0 tree
(*
(* egy low es high kozott megkeressuk az order_axis alapjan legnagyobb pontot *)
let max low high tree order_axis =
    (* 
       max_point egy olyan pont, ami low <= max_point < high es az order_axis
       tekinteteben eddig a legnagyobb, amit lattunk. Lefele a max_point es high kozott kell 
       tovabb keresnunk.
    *)
    let rec aux level low high max_node max_point node =
      match node with 
        Nil -> max_node, max_point
       | Node (point, value, left, right) as n-> 
         begin
          (* ha az eddiginel nagyobbat talaltunk *)
          let max_point, max_node =
          if kd_compare max_point point = LE && kd_compare high point = GT then
            point, n
          else 
            max_point, max_node
          in
          (*ITT TARTOK! VEGIG KELL GONDOLNI A BALRA JOBBRAT *)
          let axis = level mod dimen in
          let low = max_point in
          if not (compare_coord axis low point > 0) then
            aux  (level + 1) left;

          if not (compare_coord axis high point < 0) then
            aux (level + 1) right;
         end
    in
    aux 0 tree
*)
end 

module TwoDTuple  = struct
  type t = (int * int)

  let dimen = 2
  let print  (x, y) =
    Printf.printf "(%d, %d)" x y
    
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




end