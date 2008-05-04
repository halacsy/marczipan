
exception End_of_terminfos

(* *)
(** Egy termhez tartozo doc, freq parok es a poziciok. Egy nagy bufferbe irjuk be az adatokat.
	doc_id, freq, pos_1, pos_2 ... pos_freq sorozatok vannak a tombben.
	A doc_id-k novekvo sorrendben kell, hogy erkezzenek.
	*)
	
(** az utolso doc adatait kulon taroljuk *)

module Collector =
struct

	type t = { mutable df : int;
	mutable tf : int;
	mutable last_doc : int;
	mutable last_positions : int list;
	buffer : BlockList.Int.t;
	}
 
	let empty () = {
		df = 0;
		tf = 0;
		last_doc = - 1;
		last_positions = [];
		buffer = BlockList.Int.create ();
	}

	let last_doc ti = ti.last_doc;;
	
	let df ti = ti.df;;
	let tf ti = ti.tf;;
	
	let flush_last ti =
		if ti.last_doc > - 1 then begin
			BlockList.Int.add ti.buffer ti.last_doc;
			BlockList.Int.add ti.buffer (List.length ti.last_positions);
			List.iter (BlockList.Int.add ti.buffer) (List.rev ti.last_positions)
		end

	let doclist ti =
		flush_last ti;
		ti.buffer;;

(* letrehoz egy egy hosszu docList-et *)

	let create doc pos =
		{
			df = 1;
			tf = 1;
			last_doc = doc;
			last_positions = pos::[];
			buffer = BlockList.Int.create ();
		}
	
	let occurrence ti doc pos =
		ti.tf <- succ ti.tf;
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
		ti1.tf <- ti1.tf + ti2.tf;
		ti1.last_doc <- ti2.last_doc;
		ti1.last_positions <- ti2.last_positions;
		BlockList.Int.append ti1.buffer ti2.buffer

	let pretty_print term ti =
		Printf.printf "%s -> " term;
		Printf.printf "df = %d tf = %d " ti.df ti.tf;
		Printf.printf "last doc id = %d\n" ti.last_doc;
		let state = ref 1 in
		let aux i =
			match !state with
				| 1 -> state := 0; Printf.printf "\n%d: " i
				| 0 -> state := i + 1; Printf.printf " %d x" i
				| _ -> state := !state - 1; Printf.printf " %d" i
		in
		BlockList.Int.iter aux ti.buffer;
		Printf.printf "\n%d: " ti.last_doc;
		Printf.printf " %d x" (List.length ti.last_positions);
		List.iter (Printf.printf " %d") (List.rev ti.last_positions);
		Printf.printf "\n";;

(** ha egyszer kiirtad, tobbet nem hasznalhatod!*)
	let write oc ti =
		flush_last ti;
		Io.output_vnatint oc ti.df;
		Io.output_vnatint oc ti.tf;
		BlockList.Int.iter (Io.output_vnatint oc) ti.buffer
	
	let read ic =
		let df = try Io.input_vnatint ic with End_of_file -> raise End_of_terminfos in
		let tf = Io.input_vnatint ic in

		let buffer = BlockList.Int.create () in
		for i = 1 to (df - 1) do
		(* doc_id *)
			BlockList.Int.add buffer (Io.input_vnatint ic);
		(* freq *)
			let n = Io.input_vnatint ic in
			BlockList.Int.add buffer n;
			for j = n downto 1 do
				BlockList.Int.add buffer (Io.input_vnatint ic)
			done;
		done;
	(* last_doc-ot kulon olvassuk *)
		let last_doc = Io.input_vnatint ic in
	(* freq *)
		let n = Io.input_vnatint ic in
		let last_positions = ref [] in
		for j = n downto 1 do
			last_positions := (Io.input_vnatint ic) :: !last_positions
		done;
	
		{ df = df; tf = tf; last_doc = last_doc; last_positions = !last_positions; buffer = buffer }

end;;

type t = BlockList.Int.t

let write oc t =
	BlockList.Int.iter (Io.output_vnatint oc) t

let read ic df =
	let buffer = BlockList.Int.create () in
	for i = 1 to (df) do
		(* doc_id *)
		BlockList.Int.add buffer (Io.input_vnatint ic);
		(* freq *)
		let n = Io.input_vnatint ic in
		BlockList.Int.add buffer n;
		for j = n downto 1 do
			BlockList.Int.add buffer (Io.input_vnatint ic)
		done;
	done;
	buffer

type stream = { block_stream : BlockList.Int.stream };;

let open_stream t =
	{ block_stream = BlockList.Int.open_stream t }
;;

exception End_of_stream;;
let next_doc s =
	(* doc_id, freq, pos1, pos2, pos3 *)
let doc_id = try BlockList.Int.next s.block_stream with BlockList.Int.End_of_stream -> raise End_of_stream in
	let freq = BlockList.Int.next s.block_stream in
	BlockList.Int.skip s.block_stream (freq );
	(doc_id, freq)
	
let pretty_print t =
	let state = ref 1 in
	let aux i =
		match !state with
			| 1 -> state := 0; Printf.printf "\n%d: " i
			| 0 -> state := i + 1; Printf.printf " %d x" i
			| _ -> state := !state - 1; Printf.printf " %d" i
	in
	BlockList.Int.iter aux t;
	Printf.printf "\n"
;;
(* let _ = let ti1 = empty () in let ti2 = empty () in occurrence ti1 1 2; *)
(* occurrence ti1 1 3; occurrence ti1 1 4; occurrence ti1 1 5; occurrence  *)
(* ti1 1 6; occurrence ti1 1 7; occurrence ti1 1 8; occurrence ti2 2 2;    *)
(* occurrence ti2 2 3; pretty_print "a" ti1; pretty_print "a" ti2; let oc  *)
(* = open_out_bin "g" in write oc "a" ti1; write oc "a" ti2; close_out oc; *)
(* let ic = open_in_bin "g" in let (_, ti1) = read ic in let (_, ti2) =    *)
(* read ic in close_in ic; append ti1 ti2; pretty_print "a" ti1;           *)
