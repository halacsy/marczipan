(** Egy termhez tartozo doc, freq parok es a poziciok. Egy nagy bufferbe irjuk be az adatokat.
	doc_id, freq, pos_1, pos_2 ... pos_freq sorozatok vannak a tombben.
	A doc_id-k novekvo sorrendben kell, hogy erkezzenek.
	*)
	
(** uj dokumentum eseten a doc_id-t es a freq-et betesszuk a tombbe, de feljegyezzuk, h
	melyik poziciora kerultek, igy modosithatoak *)	
type t = {mutable df             : int ;
		  mutable tf             : int ;
          mutable last_doc_id_pos: int ;
	      buffer                 : (int) Varray.t ;

			}
				
				
let empty () = {
	df              = 0;
	tf              = 0;
	last_doc_id_pos = -1;
	buffer = Varray.create 3 0 ; (*hapaxok 3 intet foglalnak *)
	}
	
	
let last_doc ti =
	(Varray.get ti.buffer ti.last_doc_id_pos)

let first_doc ti = 
	(Varray.get ti.buffer 0)
	
let occurrence ti doc_id pos =
	ti.tf <- succ ti.tf ; 
	if ti.last_doc_id_pos = -1 || (Varray.get ti.buffer ti.last_doc_id_pos) != doc_id then 
	
	begin
		(* uj dok. vagy elso *)
		ti.df <- succ ti.df; 
		
		ti.last_doc_id_pos <- Varray.add ti.buffer doc_id;
		let _ = Varray.add ti.buffer 1 in
		let _ = Varray.add ti.buffer pos in ()
	end
	else
	begin
		(* egyszerubb eset *) 
		Varray.apply ti.buffer (succ) (ti.last_doc_id_pos +1) ;
		let _ = Varray.add ti.buffer pos in ()
	end
	
(** ket terminfo-t osszefuz.*) 
let merge ti1 ti2 =
	(* mivel a doc_id-k egymas utan jonnek, valamelyik elobbre van. Az lehet, h egy 
		dok overlap van. *)
	let last = last_doc_id 
	let (ti1, ti2, overlap) = if t
	let mti = 
let write o ti = 
	output_binary_int o ti.df;
	output_binary_int o ti.tf;
	output_binary_int o (Varray.length ti.buffer);
	let citer = Varray.citer ti.buffer in
	for d = 1 to ti.df do
		output_binary_int o (citer ());
		let n = ref (citer ()) in
		output_binary_int o !n;
		while !n > 0 do
			output_binary_int o (citer ());
			n := !n - 1;
		done
	done


let read i =
	let df = input_binary_int i in
	let tf = input_binary_int i in 
	let l  = input_binary_int i in
	let ti = { df             = df;
			   tf             = tf;
		       last_doc_id_pos = -1;
	 	       buffer = Varray.create l 0 ;
	} in
	for d = 1 to ti.df do
		ti.last_doc_id_pos <- Varray.add ti. buffer (input_binary_int i);
		let n =  (input_binary_int i) in
		let _ = Varray.add ti.buffer n in	
		let x = ref n in
		while !x > 0 do
			let _ = Varray.add ti.buffer (input_binary_int i) in		 
			x := !x - 1;
		done
	done ;
	ti

let pretty_print ti =

	Printf.printf "df = %d tf = %d" ti.df ti.tf;
	Printf.printf "last doc id = %d\n" ((Varray.get ti.buffer ti.last_doc_id_pos));
	let i = ref 0 in
	for d = 1 to ti.df do
		Printf.printf " docid = %d\n" (Varray.get ti.buffer (!i));
		incr i;
		let n = ref (Varray.get ti.buffer (!i)) in
		incr i;	
		Printf.printf " freq = %d\n" !n;
		while !n > 0 do
			Printf.printf " pos %d\n" (Varray.get ti.buffer (!i));
			incr i;
			n := !n - 1;
		done
	done