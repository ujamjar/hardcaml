(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** event driven simulator *)

(** event driven simulation.
    
    We use the following conceptual model

    - 'values' are Comb.S.t
    - 'signals' (point to) have a 'value'
    - 'events' assign a new 'value' to a 'signal' at the 
       given time
    - 'processes' are (optionally) sensitive to 'events' 
       occuring on a monitored set of 'signals'.  'events' are
       only raised if the data value actually changes.
    - 'processes' return new 'events'.  This is equivalent to assignment.
    - 'time' may only advance
    - 'time' is made of two parts - real 'time' and 'delta' step
    - an 'event' generated at the current 'time' is scheduled at the
      next 'delta'
    - 'time' may not decrease

    TODO

    - check if time decreases
    - VCD
    - testbench API (simpler process model)
    - memories

*)
open HardCaml

module Make :
  functor (B : Comb.S) ->
    sig
      type time = int
      type value = B.t
      type id = int
        val verbose' : bool ref
      
      module Event :
        sig
          type t = { time : time; value : value; signal : id; }
          val compare : t -> t -> int
          val time : t -> time
          val value : t -> value
          val signal : t -> id
          val mk : time -> value -> id -> t
          val to_string : t -> string
        end

      module type OrderedEventsSig =
        sig
          type t
          val empty : t
          val lowest_time : t -> time option
          val pop_lowest : t -> t * Event.t list
          val count : t -> int
          val add : t -> Event.t -> t
          val add_list : Event.t list -> t -> t
          val to_list : t -> Event.t list
        end
      
      module OrderedEvents : OrderedEventsSig

      module SignalDatabase :
        sig
          type t = { name : string; value : value ref; id : int; }
          val empty : t
          type database = { mutable data : t array; mutable length : int; }
          val database : database
          val compare : 'a -> 'a -> int
          val mk_id : unit -> int
          val add : t -> unit
          val get_signals : unit -> t array
          val mk : value -> string -> t
          val value : t -> value
          val ref : t -> value ref
          val name : t -> string
          val id : t -> int
          val set : t -> value -> unit
        end

      module Process :
        sig
          
          module Control :
            sig
              type t = {
                event : time -> value -> SignalDatabase.t -> Event.t;
                active : SignalDatabase.t -> bool;
                value : SignalDatabase.t -> value;
              }
              val mk :
                (time -> value -> SignalDatabase.t -> Event.t) ->
                (SignalDatabase.t -> bool) ->
                (SignalDatabase.t -> value) -> t
            end
          
          type t = {
            name : string;
            sensitivity_list : SignalDatabase.t list;
            run : Control.t -> time -> time -> Event.t list;
          }
          
          val sensitivity_list : t -> SignalDatabase.t list
          
          val run : t -> Control.t -> time -> time -> Event.t list
          
          val mk :
            string ->
            SignalDatabase.t list ->
            (Control.t -> time -> time -> Event.t list) -> t

        end

      module Simulator :
        sig

          exception SimOver
          exception SimError of string
          
          type simulation
          
          val make : Process.t list -> simulation

          val init : ?active:bool -> ?initial:bool -> simulation -> simulation

          val run : simulation -> time -> simulation

          val add : simulation -> Event.t list -> simulation

          module Circuit :
            sig

              val build :
                Circuit.t ->
                Process.t list *
                (Signal.Types.signal * SignalDatabase.t) list *
                (Signal.Types.signal * SignalDatabase.t) list

              val find_signal :
                (Signal.Types.signal * 'a) list -> string -> 'a

            end
        end

        module Cyclesim :
            sig
                type t = B.t
                type cyclesim = t Cyclesim.Api.cyclesim
                val make : Circuit.t -> cyclesim
            end

        module Vcd :
            sig
            end

    end

module Test :
  sig
    val test_0 : unit -> unit
    val test_1 : unit -> unit
    val test_2 : unit -> unit
    val test_3 : unit -> unit
  end


