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
	let (ti1, ti2) = if (last_doc ti1) < (last_doc ti2) then (ti1, ti2) else 
					 if (last_doc ti1) > (last_doc ti2) then (ti2, ti1) else
					 (* ha egyenlo, akkor a posiziciok szamitanak *)
					 let pos1 = Varray.get ti1.buffer (ti1.last_doc_id_pos + 2) in
					 let pos2 = Varray.get ti2.buffer (ti2.last_doc_id_pos + 2) in
					 if pos1 < pos2 then (ti1, ti2) else (ti2, ti1)				 
	in
	let overlap = (last_doc ti1) = (first_doc ti2) in 

	let mti = {df = ti1.df + ti2.df - (if overlap then 1 else 0); 
		       tf = ti1.tf + ti2.tf; 
		       last_doc_id_pos = (Varray.length ti1.buffer) + ti2.last_doc_id_pos;
			   buffer = Varray.create  ((Varray.length ti1.buffer) + 
			        (Varray.length ti2.buffer) - 
			        (if overlap then 2 else 0) (* folosleges doc_id es freq *)
			       ) 0;
			  } in
			
	if(overlap) then
	begin
		(* overlap eseten bonyolult a helyzet, kepzeld el a kovetkezot
			d0 1 1 d1 1 3
			       d1 2 0 1 d2 2 2 4
			bar utobbi kerul elobbre, de az elso d1-jet kell atmasolni
		
		*)	
		(* a  a ket tf-t osszeadjuk *)
		let tf =  Varray.get ti1.buffer (ti1.last_doc_id_pos + 1) +
			      Varray.get ti2.buffer (1)
		in
		let tp1 = Varray.get ti1.buffer (ti1.last_doc_id_pos + 2) in
		let tp2 = Varray.get ti2.buffer 2 in
		if tp1 < tp2 then
		begin
			(* ez nem a fenti eset, hanem a sime *)
			Varray.append mti.buffer ti1.buffer 0 ;
			Varray.set mti.buffer tf (ti1.last_doc_id_pos + 1);
			Varray.append mti.buffer ti2.buffer 2;
			mti.last_doc_id_pos <- mti.last_doc_id_pos -2
	end
		else begin
			(* na akkor ez a trukkosebb*)
			Varray.append_slice mti.buffer ti1.buffer 0 ti1.last_doc_id_pos ;
			let _ = Varray.add mti.buffer tf in
			(* most eloszor a ti2 elso dokumentumainak pozicioi jonnek*)
			Varray.append_slice mti.buffer ti2.buffer (2) ((Varray.get ti2.buffer 1) +1);
			(* most a ti1 pozicioi *) 
			Varray.append mti.buffer ti1.buffer (ti1.last_doc_id_pos + 2);
			(* most a ti2 maradeka *)
			Varray.append mti.buffer ti2.buffer ((Varray.get ti2.buffer 1) + 2);
			mti.last_doc_id_pos <- mti.last_doc_id_pos -2;
		end
	end
	else

		Varray.append mti.buffer ti1.buffer 0 ;
		Varray.append mti.buffer ti2.buffer 0;
	mti	
	
		
let write o term ti = 
    Io.output_string o term;
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
exception End_of_terminfos

let read i =
	let term = try Io.input_string i with End_of_file -> raise End_of_terminfos in
		
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
	(term, ti)

let pretty_print term ti =

    Printf.printf "%s -> " term;
	Printf.printf "df = %d tf = %d " ti.df ti.tf;
	Printf.printf "last_doc_pos = %d " ti.last_doc_id_pos;
	Printf.printf "last doc id = %d\n" ((Varray.get ti.buffer ti.last_doc_id_pos));
	let citer = Varray.citer ti.buffer in
	for d = 1 to ti.df do
		Printf.printf "%d: " (citer ());
		let n = ref ( (citer ())) in
		Printf.printf " %d x" !n;
		while !n > 0 do
			Printf.printf " %d" (citer());
			n := !n - 1;
		done;
		Printf.printf "\n"
	done
	
(*
let _ =
	let ti1 = empty () in
	let ti2 = empty () in
	occurrence ti1 1 2;
	occurrence ti1 1 3;
	
	occurrence ti2 1 1;
	occurrence ti2 2 2;
	occurrence ti2 2 3;
	
	pretty_print "a" ti1 ;
	pretty_print "a" ti2 ;
	pretty_print "a" (merge ti2 ti1)
*)