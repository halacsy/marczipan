(** Egy termhez tartozo doc, freq parok es a poziciok. Egy nagy bufferbe irjuk be az adatokat.
	doc_id, freq, pos_1, pos_2 ... pos_freq sorozatok vannak a tombben.
	A doc_id-k novekvo sorrendben kell, hogy erkezzenek.
	*)
	
(** az utolso doc adatait kulon taroljuk *)

type t = {mutable df             : int ;
		  mutable tf             : int ;
          mutable last_doc  : int;
		  mutable last_positions : int list;
	      buffer                 : (int) BlockList.t ;
		}
				
				
let empty () = {
	df              = 0;
	tf              = 0;
	last_doc        = -1;
	last_positions  = [];
	buffer = BlockList.create 0;
	}
	
let last_doc ti = ti.last_doc;;
	
	
let df ti = ti.df ;;
let tf ti = ti.tf;;
	
let flush_last ti = 
		if ti.last_doc > -1 then begin
			BlockList.add ti.buffer ti.last_doc;
			BlockList.add ti.buffer (List.length ti.last_positions);
			List.iter (BlockList.add ti.buffer) (List.rev ti.last_positions)
		end
		
let occurrence ti doc pos =
	ti.tf <- succ ti.tf ; 
	if ti.last_doc != doc then 
		begin
			(* uj dok. vagy elso *)
			ti.df <- succ ti.df; 
			flush_last ti;
			ti.last_doc <- doc;
			ti.last_positions <- pos::[]
			end
	else
		ti.last_positions <- pos :: ti.last_positions	

let append ti1 ti2 =
	assert (ti1.last_doc < ti2.last_doc);
	flush_last ti1;
	ti1.df <- ti1.df + ti2.df;
	ti1.tf <- ti1.tf + ti1.tf;
	ti1.last_doc <- ti2.last_doc;
	ti1.last_positions <- ti2.last_positions;
	BlockList.append ti1.buffer ti2.buffer

(** ha egyszer kiirtad, tobbet nem hasznalhatod!*)
let write oc term ti =
	flush_last ti;
	Io.output_string oc term;
    output_binary_int oc ti.df;
    output_binary_int oc ti.tf;
	BlockList.iter (output_binary_int oc) ti.buffer
		
exception End_of_terminfos

let read ic = 
	let term = try Io.input_string ic with End_of_file -> raise End_of_terminfos in
	let df = input_binary_int ic in
    let tf = input_binary_int ic in 
 	let buffer = BlockList.create 0 in
	for i = 1 to (df-1) do
		(* doc_id *)
		BlockList.add buffer (input_binary_int ic);
		(* freq *)
		let n = input_binary_int ic in
		BlockList.add buffer n ;
		for j = n downto 1 do
			BlockList.add buffer (input_binary_int ic)
		done;
	done;
	(* last_doc-ot kulon olvassuk *)
	let last_doc = input_binary_int ic in
	(* freq *)
	let n = input_binary_int ic in
	let last_positions = ref [] in
	for j = n downto 1 do
		last_positions := (input_binary_int ic) :: !last_positions
	done;
	
	(term, {df = df; tf =tf; last_doc = last_doc; last_positions = !last_positions; buffer = buffer})
	
let pretty_print term ti =
    Printf.printf "%s -> " term;
    Printf.printf "df = %d tf = %d " ti.df ti.tf;
    Printf.printf "last doc id = %d\n" ti.last_doc;
	let state = ref 0 in
	let aux i =
		if !state = 0 then begin	
        	Printf.printf "\n%d: " i;
            state := 1;
		end else
		if !state = 1 then begin
			Printf.printf " %d x" i;	
			state := i + 2 
		end else begin
			Printf.printf " %d" i;
            state := !state - 1;
		end
	in
	BlockList.iter aux ti.buffer;
	Printf.printf "\n%d: " ti.last_doc;
    Printf.printf " %d x" (List.length ti.last_positions);	
	List.iter (Printf.printf " %d") (List.rev ti.last_positions);
	Printf.printf "\n";;
(*	
let _ =
      let ti1 = empty () in
      let ti2 = empty () in
      occurrence ti1 1 2;
      occurrence ti1 1 3;
	  occurrence ti1 1 4;
	occurrence ti1 1 5;
	occurrence ti1 1 6;
	occurrence ti1 1 7;
	occurrence ti1 1 8;

      occurrence ti2 2 2;
      occurrence ti2 2 3;

      pretty_print "a" ti1;
      pretty_print "a" ti2;
	let oc = open_out_bin "g" in
	write oc "a" ti1;
	write oc "a" ti2;
	close_out oc;
	let ic = open_in_bin "g" in
	let (_, ti1) = read ic in
	let (_, ti2) = read ic in
	close_in ic;
	append ti1 ti2;
	  pretty_print "a" ti1;
	
*)