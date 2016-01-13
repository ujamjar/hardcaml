########################################
# hardcaml - hardware design in OCaml
#
#   (c) 2014 MicroJamJar Ltd
#
# Author(s): andy.ray@ujamjar.com
# Description: 
#
########################################

.PHONY: clean all iocamljs install uninstall 

BUILD_OPTS=

all: setup.data
	ocaml setup.ml -build

setup.ml:
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

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
		-export-package hardcaml.js \
		-export-package iocamljs-kernel \
		-jsopt +dynlink.js -jsopt +weak.js -jsopt +toplevel.js -jsopt +nat.js \
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
	ocamlfind remove hardcaml

clean:
	ocaml setup.ml -clean
	- find . -name "*~" | xargs rm

distclean:
	ocaml setup.ml -distclean

