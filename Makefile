########################################
# hardcaml - hardware design in OCaml
#
#   (c) 2014 MicroJamJar Ltd
#
# Author(s): andy.ray@ujamjar.com
# Description: 
#
########################################

.PHONY: clean all vpi iocamljs install uninstall 

BUILD_OPTS=

all: setup.data
	ocaml setup.ml -build

setup.ml:
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

# icarus verilog VPI cosim interface
vpi: 
	ocamlbuild -use-ocamlfind $(BUILD_OPTS) cosim_icarus.cmo vpi.cmo
	ocamlfind c -output-obj -package bigarray,num,ctypes.foreign -linkpkg -o cosim_o.o \
		_build/HardCaml.cma _build/vpi/vpi.cmo _build/vpi/cosim_icarus.cmo
	mv cosim_o.o _build/vpi/cosim_o.o
	$(CC) -c `iverilog-vpi --cflags` -g vpi/cosim_c.c -o _build/vpi/cosim_c.o
	$(CC) -o _build/vpi/cosim.vpi \
		`iverilog-vpi --ldflags` \
		_build/vpi/cosim_o.o _build/vpi/cosim_c.o \
		-L`opam config var lib`/ocaml \
		-L`opam config var lib`/ctypes \
		-lunix -lbigarray -lcamlstr \
		-lctypes_stubs -lctypes-foreign-base_stubs \
		-lcamlrun_shared -lffi -ldl -lm \
		`iverilog-vpi --ldlibs` \
		-Wl,-E

# iocamljs notebook kernel
iocamljs:
	jsoo_mktop \
		-verbose \
		-dont-export-unit gc \
		-top-syntax lwt.syntax \
		-top-syntax js_of_ocaml.syntax \
		-top-syntax hardcaml.syntax \
		-export-package lwt \
		-export-package js_of_ocaml \
		-export-package hardcaml \
		-export-package iocamljs-kernel \
		-jsopt +weak.js -jsopt +toplevel.js -jsopt +nat.js \
		-jsopt -I -jsopt ./ \
		-o iocaml.byte
	cat *.cmis.js \
		`opam config var lib`/iocamljs-kernel/kernel.js iocaml.js > \
		`opam config var share`/iocamljs-kernel/profile/static/services/kernels/js/kernel.hardcaml.js

doc:
	ocamlbuild -use-ocamlfind $(BUILD_OPTS) HardCaml.docdir/index.html

####################################################

install: all
	ocaml setup.ml -install

uninstall:
	ocaml setup.ml -uninstall

clean:
	ocaml setup.ml -clean
	- find . -name "*~" | xargs rm

