module TermLexicon = FsaLexicon.Make
module InvIndex = InvIndex.Make(TermLexicon)

module Inverter = Inverter.Make(InvIndex.Writer)
module IndexReader = InvIndex.Reader

module Matrix = Kdmatrix.Make(Kdmatrix.TwoDTuple)

let index_dir = "data/"

let _ =
  let ii = IndexReader.open_reader index_dir in
  let fi = ForIndex.open_reader index_dir in
  let points = ref [] in
  let term s ix tf df doc_stream =
  (*  List.iter print_char s;
    print_char '\t';
    print_int ix;
    print_newline ();
  *)  let doc_stream = doc_stream () in
    try
    while (true) do
      let (doc_id, freq) = DocList.next_doc doc_stream in
      points := ((ix,doc_id) , freq) :: !points
    done ;
    with DocList.End_of_stream -> () 
  in
  
  let lex = IndexReader.lexicon ii in
  IndexReader.iter_over_terms term ii;
  prerr_endline "constructing tree";
  let tree = Matrix.create_from_list !points in
  prerr_endline "constructed";
  let oc = open_out_bin "data/kdmatrix" in
  Marshal.to_channel oc tree [];
  close_out oc;
  