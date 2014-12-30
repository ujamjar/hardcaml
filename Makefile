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

HARDCAML_TGT = _build/HardCaml.cmi _build/HardCaml.cmxa _build/HardCaml.cma _build/HardCaml.a
PA_HARDCAML_TGT = _build/src/pa_hardcaml.cmo _build/src/pa_hardcaml.cmi
HARDCAML_JS_TGT = _build/HardCamlJS.cmi _build/HardCamlJS.cma 

all: 
	ocamlbuild -use-ocamlfind $(BUILD_OPTS) HardCaml.cma 
	ocamlbuild -use-ocamlfind $(BUILD_OPTS) HardCaml.cmxa
	ocamlbuild -use-ocamlfind $(BUILD_OPTS) pa_hardcaml.cmo
	ocamlbuild -use-ocamlfind $(BUILD_OPTS) HardCamlJS.cma   

# icarus verilog VPI cosim interface
vpi: 
	ocamlbuild -use-ocamlfind $(BUILD_OPTS) cosim_icarus.cmo vpi.cmo
	ocamlfind c -output-obj -package ctypes.foreign -linkpkg -o cosim_o.o \
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
	make -C ../../iocamljs all \
		OPT=1 EXT=".hardcaml" \
		CAMLP4=1 LWT=1 JSOO=1 \
		SYNTAX="js_of_ocaml.syntax lwt.syntax.options lwt.syntax hardcaml.syntax" \
		PACKAGES="hardcaml hardcaml.js" MODULES="HardCaml HardCamlJS"

####################################################

install: all
	$(SUDO) ocamlfind install hardcaml META \
		$(HARDCAML_TGT) $(HARDCAML_JS_TGT) $(PA_HARDCAML_TGT) \
		_build/vpi/cosim.vpi tools/hardcaml_cosim.sh tools/hardcaml_vvp.sh
	
uninstall:
	$(SUDO) ocamlfind remove hardcaml

clean:
	ocamlbuild -clean
	- find . -name "*~" | xargs rm

