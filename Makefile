#SOURCES = lexicon.mli btree.mli bst.ml btree.ml bursttrie.mli bursttrie.ml  #mfhash.ml hashlex.ml io.ml test.ml
SOURCES = io.ml mfhash.ml hashlex.ml  invIndex.ml docinfo.ml  indexer.ml
RESULT = test

#LIBDIRS=/usr/local/lib/ocamorph 
#INCDIRS=/usr/local/lib/ocamorph
export OCAMLNCFLAGS := -unsafe
#all: nc
all: profiling-native-code

-include OCamlMakefile
