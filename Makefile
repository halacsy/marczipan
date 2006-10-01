STRUCTS = structs/mfhash.ml structs/hashlex.ml structs/blockList.mli structs/blockList.ml structs/heap.ml
UTIL = codec.ml io.ml timem.ml
INDEX = docList.ml docMeta.ml invIndex.ml merger.ml  forIndex.ml inverter.ml 
SOURCES = config.ml ${UTIL}  ${STRUCTS}  ${INDEX} main.ml

RESULT = test


export OCAMLNCFLAGS := -unsafe -inline 100 
all:debug-code
#all: profiling-native-code
#all: nc
#all: profiling-byte-code

-include OCamlMakefile
