SOURCES = structs/mfhash.ml parsing/tokenizer.ml util/timem.ml util/codec.ml util/io.ml parsing/analyzer.ml structs/blockList.ml index/docList.ml index/docMeta.ml index/forIndex.ml  index/invIndex.ml structs/simple_fsa.ml structs/fsa_builder.ml  structs/compact_fsa.ml index/fsaLexicon.ml structs/heap.ml index/merger.ml index/inverter.ml structs/kdmatrix.ml index/kdindex.ml parsing/xml_tokenizer.ml parsing/xml_parser.ml  parsing/mh_parser.ml index/Searcher.ml clef_tester.ml
#LIBDIRS=/usr/local/lib/ocamorph 
RESULT=vacak
INCDIRS=structs

#all: nc
all: profiling-byte-code
-include OCamlMakefile
