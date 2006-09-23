#SOURCES = lexicon.mli btree.mli bst.ml btree.ml bursttrie.mli bursttrie.ml  #mfhash.ml hashlex.ml io.ml test.ml
SOURCES = io.ml timem.ml mfhash.ml hashlex.ml varray.mli varray.ml  docinfo.ml terminfo.ml invIndex.ml  indexer.ml
#SOURCES = io.ml timem.ml mfhash.ml hashlex.ml varray.ml  docinfo.ml invIndex.ml  test.ml

RESULT = test

#LIBDIRS=/usr/local/lib/ocamorph 
#INCDIRS=/usr/local/lib/ocamorph
export OCAMLNCFLAGS := -unsafe -inline 100 
all:debug-code
#all: nc
#all: profiling-byte-code

-include OCamlMakefile
