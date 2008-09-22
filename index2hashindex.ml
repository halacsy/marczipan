module TermLexicon = FsaLexicon.Make
module InvIndex = InvIndex.Make(TermLexicon)

module Inverter = Inverter.Make(InvIndex.Writer)
module IndexReader = InvIndex.Reader

module Matrix = Kdmatrix.Make(Kdmatrix.TwoDTuple)

let index_dir = "data/"

let _ =
  let ii = IndexReader.open_reader index_dir in
  let fi = ForIndex.open_reader index_dir in
  let index = Inmemory_invindex.create() in

  let term s ix tf df doc_stream =
    let posting_list = ref [] in
    let doc_stream = doc_stream () in
    try
    while (true) do
      let (doc_id, freq) = DocList.next_doc doc_stream in
      posting_list := (doc_id , freq) :: !posting_list
    done ;
    with DocList.End_of_stream -> 
      Inmemory_invindex.add_posting index ix (!posting_list); 
  in
  
  let lex = IndexReader.lexicon ii in
  IndexReader.iter_over_terms term ii;
  prerr_endline "constructing index";
  Inmemory_invindex.save index_dir index 
 