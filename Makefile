SOURCES = lexicon.mli btree.mli bst.ml btree.ml bursttrie.mli bursttrie.ml  hashlex.ml io.ml test.ml
RESULT = test
#LIBDIRS=/usr/local/lib/ocamorph 
#INCDIRS=/usr/local/lib/ocamorph
export OCAMLNCFLAGS := -inline 10
#all: nc
all: profiling-native-code
-include OCamlMakefile
