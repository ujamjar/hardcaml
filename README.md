# HardCaml 

[![Build Status](https://travis-ci.org/ujamjar/hardcaml.svg?branch=master)](https://travis-ci.org/ujamjar/hardcaml)

HardCaml is an OCaml library for designing hardware.

* Express hardware designs in OCaml
* Make generic designs using higher order functions, lists, maps, functors...
* Simulate designs in OCaml
* Convert to VHDL, Verilog, C
* Write new modules to transform or analyse circuits, or provide new backends

[Try it online!](http://ujamjar.github.io/hardcaml)

# Build

With opam

```
$ opam install hardcaml
```

The package (optionally) depends on `camlp4` for the syntax extension and `ctypes-foreign` for the C based simulator.

To build locally use

```
$ ocaml pkg/pkg.ml build --with-camlp4 [true|false] --with-ctypes-foreigh [true|false]
```

An `IOcamlJS` notebook kernel can also be built with

```
$ ocamlbuild kernel.hardcaml.js
```

# Related tools

* [Examples and framework](https://github.com/ujamjar/hardcaml-examples) - simple to mildly complex example designs
* [Waveform viewer](https://github.com/ujamjar/hardcaml-waveterm) - terminal based digital waveform viewer 
* [Icarus verilog VPI interface](https://github.com/ujamjar/hardcaml-vpi) - cosimulation with Icarus verilog
* [LLVM simulator](https://github.com/ujamjar/hardcaml-llvmsim) - high speed, native code generating simulator

# Other projects

* [Reed-Solomon CODEC](https://github.com/ujamjar/hardcaml-reedsolomon) configurable Reed-Solomon encoder/decoder
* [OpenRISC](https://github.com/ujamjar/hardcaml-mor1kx) direct port of mork1x cpu _very alpha, not tested as yet_


