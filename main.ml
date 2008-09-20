module TermLexicon = FsaLexicon.Make
module InvIndex = InvIndex.Make(TermLexicon)

module Inverter = Inverter.Make(InvIndex.Writer)
module IndexReader = Kdindex.Reader(InvIndex.Reader)
module Searcher = Searcher.Make(Kdindex.Reader(InvIndex.Reader))
