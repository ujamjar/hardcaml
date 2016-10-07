# v2.0.0

* Reorganise and merge a bunch of modules (esp _ext_.ml) which were split out
  previously due to js\_of\_ocaml but is no longer needed.
* Replace oasis with ocamlbuild and topkg

# v1.1.1

* add out_port_next function to simulator - update on out_port reverts to old behaviour
* add dynamic simulation plugin back end registering (for llvmsim)
* fix vpi cosim module search path

# v1.1.0

* rework simulation so we get the correct output values (in all cases) after cycle
* various simulation hook points added to correctly support waveforms/combining etc
* add `Recipe` module - generates statemchines from imperative style descriptions
