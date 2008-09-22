module Lex = Mfhash.Int

let create () =
  Lex.create 324
  
let add_posting index term_id postings = 
  Lex.add_or_replace index term_id postings
;;

let save index_dir index = 
  let oc = open_out_bin (index_dir ^ "/" ^ "hashbased_index") in
  Marshal.to_channel oc index [];
  close_out oc;
  
(* Kdindex.Reader extends the usual InvIndex.Reader *)
module Reader(IIndex:InvIndex.Reader):InvIndex.Reader =
 struct 
  type t = {
    index: (int * int) list Lex.t;
    iindex_reader:IIndex.t;
  }
	type lexicon_t = IIndex.lexicon_t
		(** opens a reader  *)

	let open_reader index_dir = 
	  let ic = open_in_bin (index_dir ^ "/" ^ "hashbased_index") in
	  let index = Marshal.from_channel ic in
	  
	  let iindex = IIndex.open_reader index_dir in
	  {index = index;
	   iindex_reader = iindex}
	;;
		
	let term_info r term = 
	  let (df, tf, pos) = IIndex.find_term r.iindex_reader term in
	  let term_ix = IIndex.term2index r.iindex_reader term in
	  let res = (Lex.find r.index term_ix) in
    let res = Array.of_list res in
      Array.stable_sort (fun (a,_) (b,_) ->  a - b) res;
    (* van open, mert diszkre is tudunk dolgozni *)
    let open_stream () =
      let ix = ref 0 in
      let len = Array.length res in
      
      let next () = 
        if !ix >= len then raise  DocList.End_of_stream ;
    
        let temp = res.(!ix) in
        incr  ix;
        temp
      in
      next
   in
   (df, tf, open_stream)
   
	let doc_count r = IIndex.doc_count r.iindex_reader
	let token_count  r = IIndex.token_count r.iindex_reader
	let type_count r = IIndex.type_count r.iindex_reader
	let lexicon r = IIndex.lexicon r.iindex_reader
	let iter_over_terms f r = IIndex.iter_over_terms f r.iindex_reader
	let find_term  r = IIndex.find_term r.iindex_reader
	let term2index r = IIndex.term2index r.iindex_reader
end