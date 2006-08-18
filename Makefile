SOURCES = lexicon.mli btree.mli bst.ml btree.ml bursttrie.mli bursttrie.ml  hashlex.ml hashlexhas.ml io.ml test.ml
RESULT = test
#LIBDIRS=/usr/local/lib/ocamorph 
#INCDIRS=/usr/local/lib/ocamorph
export OCAMLNCFLAGS := -unsafe
#all: nc
all: profiling-native-code

-include OCamlMakefile
