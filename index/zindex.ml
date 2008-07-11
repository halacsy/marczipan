module TermLexicon = FsaLexicon.Make

let _ = 
  let t = Timem.init () in
  Timem.start t "loading lexicon" ;
  let lexicon = TermLexicon.init "data/" in
  Timem.finish t;
  
  let ix = TermLexicon.index lexicon "az" in
  prerr_string "term id =";
  prerr_int ix;
  prerr_newline ();
  Timem.start t "load index";
  let ic = open_in_bin "data/zmatrix" in
  
  let tree = Marshal.from_channel ic in
  close_in ic;
  Timem.finish t;
  
  prerr_endline "index is read\n";
  
  let f (term_id, doc_id) tf =
    print_endline (Printf.sprintf "%d %d" doc_id  tf)
  in
  (*
  Timem.start t "query";
  Ztree.query f (ix, 0) (ix + 1, 6000) tree;
  Timem.finish t;
  *)
  Timem.start t "reader";
  let r = Ztree.create_reader tree   (ix, 0) (ix + 5, 6000) in
  try
    while (true) do
      let ((term_id, doc_id), tf) = r () in
      print_endline (Printf.sprintf "%d %d" doc_id  tf)
    done;
  with Ztree.No_more_node -> ();
  
  Timem.finish t; 
  ()
