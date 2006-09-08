#SOURCES = lexicon.mli btree.mli bst.ml btree.ml bursttrie.mli bursttrie.ml  #mfhash.ml hashlex.ml io.ml test.ml
SOURCES = io.ml mfhash.ml hashlex.ml varray.ml  docinfo.ml   invIndex.ml  indexer.ml
RESULT = test

#LIBDIRS=/usr/local/lib/ocamorph 
#INCDIRS=/usr/local/lib/ocamorph
export OCAMLNCFLAGS := -unsafe 
all: nc
#all: profiling-byte-code

-include OCamlMakefile
