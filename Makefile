STRUCTS = structs/mfhash.ml structs/hashlex.ml structs/blockList.mli structs/blockList.ml structs/heap.ml
UTIL = io.ml timem.ml
INDEX = docList.ml docMeta.ml invIndex.ml merger.ml  forIndex.ml inverter.ml 
SOURCES = ${UTIL}  ${STRUCTS}  ${INDEX} main.ml

RESULT = test

#LIBDIRS=/usr/local/lib/ocamorph 
#INCDIRS=/usr/local/lib/ocamorph
#export OCAMLNCFLAGS := -unsafe -inline 100 
#all:debug-code
#all: nc
all: profiling-byte-code

-include OCamlMakefile
