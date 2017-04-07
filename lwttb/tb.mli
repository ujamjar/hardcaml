(** 
 
    {2 HardCaml LWT testbench framework. }

  Testbenches in HardCaml are synchronised to a [cycle] function
  which updates the simulation state.  The hardware under test may
  have 1 or more relatively distinct input/output interfaces (for
  example multiple input/output fifos, register interfaces, ddr
  interfaces etc).

  Tranditionally, if more than 1 interface needed modelling in the
  testbench, then each process would have to be converted into a
  statemachine that was updated as part of the simulation cycle.

  This code allows for multiple processes to be run in parallel
  while still synchronising around the clock.

    {2 Programming model}

  A testbench consists of a number of tasks of type [t -> t Lwt.t].
  The type [t] holds the simulation state and allows each Lwt 
  thread to be synchronised with the simulation cycle.  [t] must
  be passed through each API function (it is purely functional
  and gets updated by successive API functions).
  
  The functions [cycle1] and [spawn] provide the core API.
  [cycle1] synchronises all tasks together, collects circuit
  inputs, runs the simulation cycle and distributes circuit outputs.
  [spawn] allows nww tasks to start running (within the current
  simulation cycle).  Tasks are arranged in a tree like fashion.
  The inital task is the root, and spawns within spawns are
  (nearer to) the leaves.

  The [set] function (along with [setsome] and [setall]) allow
  tasks to set circuit inputs.  Multiple tasks can set the same
  input and sets nearer leaves take priority.

  note; this is the current behaviour and may change or be
        extended ie to allow priority to be specified.
        The priority among tasks at the same level is the
        latest spawned task takes precedence.

*)
open HardCaml

module type State = sig
  type state
end

module type S = sig

  (** simulation state type *)
  type state

  (** bit vector type used to implement simulator calculations *)
  type b

  (** circuit inputs *)
  type 'a i

  (** circuit outputs *)
  type 'a o

  (* simulator reset function *)
  type reset = unit -> state

  (* simulator cycle function *)
  type cycle = state -> b i -> state * b o * b o
    
  type task_req  

  (** simulation testbench data type *)
  type t = private
    {
      (* mailbox variables used to synchronise to the clock cycle *)
      vreq : task_req Lwt_mvar.t;
      vresp : (b o * b o) Lwt_mvar.t;
      (* child tasks *)
      children : t list;
      (* inputs *)
      inputs : b option i;
      (* cycle logging function *)
      log : log option;
    }

  and log = t -> unit Lwt.t

  (** type of simulation tasks synchronised to the clock *)
  type task = t -> t Lwt.t

  (* {2 cycles, task spawning and utility functions} *)

  (** cycle the clock, return circuit outputs *)
  val cycle1 : t -> (t * b o * b o) Lwt.t

  (** cycle the clock n>=1 times *)
  val cycle : ?n:int -> t -> (t * b o * b o) Lwt.t

  (** spawn a new simulation task synchronised to each cycle *)
  val spawn : ?log:log -> task -> t -> t Lwt.t

  (** [repeat n task sim] repeats the task n times *)
  val repeat : int -> task -> t -> t Lwt.t

  (** [delay n task sim] delay for n cycles then run task *)
  val delay : int -> task -> t -> t Lwt.t

  (** [Lwt.return] *)
  val return : 'a -> 'a Lwt.t

  (** perform a simulation cycle and return *)
  val return_cycle : t -> t Lwt.t

  (** {2 setting circuit inputs} *)

  (** input field accessors *)
  val i : bool i i

  (** input structure with all fields set to none *)
  val inone : b option i

  (** set an input field *)
  val set : bool i -> b -> t -> t Lwt.t

  (** set some inputs *)
  val setsome : b option i -> t -> t Lwt.t

  (** set all inputs *)
  val setall : b i -> t -> t Lwt.t

  (** {2 testbench simulation} *)

  (** run testbench *)
  val run : ?log:log -> (reset * cycle) -> task -> unit Lwt.t

end

module Make(State : State) (B : Comb.S)(I : Interface.S)(O : Interface.S) : 
  S with type state = State.state
     and type b = B.t 
     and type 'a i = 'a I.t 
     and type 'a o = 'a O.t

