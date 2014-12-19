# NOTE; it's not quite possible to run in native code, as we don't have
# a shared version of libasmrun.

cosim.cmo: cosim.ml
	ocamlfind c -c -package ctypes.foreign cosim.ml

cosim_o.o: cosim.cmo
	ocamlfind c -output-obj -package ctypes.foreign -linkpkg -o cosim_o.o cosim.cmo

cosim_c.o: cosim.c
	gcc -c `iverilog-vpi --cflags` -g cosim.c -o cosim_c.o

cosim_m.o: cosim.c
	gcc -c -g -DMAIN -I/usr/include/iverilog cosim.c -o cosim_m.o

LIB=`opam config var lib`
LIBOCAML=${LIB}/ocaml
LIBCTYPES=${LIB}/ctypes

test.vpi: cosim_o.o cosim_c.o
	gcc -o test.vpi \
		`iverilog-vpi --ldflags` \
		cosim_o.o cosim_c.o \
		-L${LIBOCAML} \
		-L${LIBCTYPES} \
		-lunix -lbigarray -lcamlstr \
		-lctypes_stubs -lctypes-foreign-base_stubs \
		-lcamlrun_shared -lffi -ldl -lm \
		`iverilog-vpi --ldlibs` \
		-Wl,-E

test: test.vpi
	iverilog -o test.vvp test.v
	LD_LIBRARY_PATH=${LIBOCAML} vvp -M. -mtest test.vvp

clean:
	rm -f *.cmi *.cmo *.cmx *.o test test2 *.vpi *.vvp

