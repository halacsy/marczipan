module TwoDMatric = Kdmatrix.Make(Kdmatrix.TwoDTuple)

(* Kdindex.Reader extends the usual InvIndex.Reader *)
module Reader(IIndex:InvIndex.Reader):InvIndex.Reader =
 struct 
  type t = {
    tree:int TwoDMatric.t;
    iindex_reader:IIndex.t;
  }
	type lexicon_t = IIndex.lexicon_t
		(** opens a reader  *)

	let open_reader index_dir = 
	  let ic = open_in_bin (index_dir ^ "/" ^ "kdmatrix" ) in
	  let tree = Marshal.from_channel ic in
	  
	  let iindex = IIndex.open_reader index_dir in
	  {tree = tree;
	   iindex_reader = iindex}
	;;
		
	let term_info r term = 
	  let (df, tf, pos) = IIndex.find_term r.iindex_reader term in
	  let term_ix = IIndex.term2index r.iindex_reader term in
	  let res = ref [] in
    let f (term_id, doc_id) tf =
      res := (doc_id, tf) :: !res
    in
    TwoDMatric.query f (term_ix, 0) (term_ix + 1, 10000000) r.tree;
    let res = Array.of_list !res in
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