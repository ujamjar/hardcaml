iverilog -o temp.vvp $@
LD_LIBRARY_PATH=`ocamlc -where` vvp -M`opam config var lib`/hardcaml -mcosim temp.vvp

