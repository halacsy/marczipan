module TermLexicon = FsaLexicon.Make
module Matrix = Kdmatrix.Make(Kdmatrix.TwoDTuple)

let _ = 
  let lexicon = TermLexicon.init "data/" in
  let ix = TermLexicon.index lexicon "az" in
  let ic = open_in_bin "data/kdmatrix" in
  let tree = Marshal.from_channel ic in
  close_in ic;
  let res = ref [] in
  let f (term_id, doc_id) tf =
    res := (doc_id, tf) :: !res
  in
  Matrix.query f (ix, 0) (ix + 5, 1000000) tree;
  let res = List.sort (fun (a,_) (b,_) -> compare a b) !res in
  List.iter (fun (a, b) -> Printf.printf "%d %d\n" a b) res
