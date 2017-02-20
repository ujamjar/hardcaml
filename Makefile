.PHONY: all build clean tag prepare publish

WITH_CAMLP4 = $(shell if ocamlfind query camlp4 >/dev/null 2>&1; then echo true; else echo false; fi)
WITH_CTYPES = $(shell if ocamlfind query ctypes >/dev/null 2>&1; then echo true; else echo false; fi)
WITH_CTYPES_FOREIGN = $(shell if ocamlfind query ctypes-foreign >/dev/null 2>&1; then echo true; else echo false; fi)
WITH_JSOO = $(shell if ocamlfind query js_of_ocaml >/dev/null 2>&1; then echo true; else echo false; fi)
WITH_LWT = $(shell if ocamlfind query lwt >/dev/null 2>&1; then echo true; else echo false; fi)
WITH_DELIMCC = $(shell if ocamlfind query delimcc >/dev/null 2>&1; then echo true; else echo false; fi)

all: build

build:
	cp pkg/META.in pkg/META
	ocaml pkg/pkg.ml build \
		--with-ctypes $(WITH_CTYPES) \
		--with-ctypes-foreign $(WITH_CTYPES_FOREIGN) \
		--with-camlp4 $(WITH_CAMLP4) \
		--with-js_of_ocaml $(WITH_JSOO) \
		--with-lwt $(WITH_LWT) \
		--with-delimcc $(WITH_DELIMCC)

clean:
	ocaml pkg/pkg.ml clean

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

tag:
	git tag -a "v$(VERSION)" -m "v$(VERSION)."
	git push origin v$(VERSION)

prepare:
	opam publish prepare -r hardcaml $(NAME_VERSION) $(ARCHIVE)

publish:
	opam publish submit -r hardcaml $(NAME_VERSION)
	rm -rf $(NAME_VERSION)

