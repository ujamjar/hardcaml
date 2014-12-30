open Ctypes

val u64_of_u32 : Unsigned.UInt32.t -> Unsigned.UInt64.t
val u32_of_u64 : Unsigned.UInt64.t -> Unsigned.UInt32.t
val get_time : unit -> Unsigned.UInt64.t
val set_time : Unsigned.UInt64.t -> Vpi.Time.t structure
val vpi_fold : Vpi.vpiHandle -> int32 -> ('a -> Vpi.vpiHandle -> 'a) -> 'a -> 'a
val once_only : ('a -> 'b) -> 'a -> 'b
val at_time_0 : ('a -> 'b) -> 'a -> 'b
val register_task : string -> Vpi.Systf_data.callback_t -> unit
val register_callback : 
  ?user_data:unit ptr -> ?obj:Vpi.vpiHandle -> ?value:Vpi.Value.t structure ptr ->
  reason:int32 -> time:Vpi.Time.t structure ptr -> (Vpi.Cb_data.t structure ptr -> int32) -> unit
val time_0 : Vpi.Time.t structure

module SMap : Map.S with type key = string
type cosim_state = Vpi.vpiHandle SMap.t

val set_values : cosim_state -> HardCaml.Cosim.delta_message -> unit
val get_values : cosim_state -> HardCaml.Cosim.delta_message -> HardCaml.Cosim.response_message

val init_state : HardCaml.Cosim.init_message -> cosim_state

val run_cosim : (Unix.file_descr * cosim_state) -> Vpi.Cb_data.t structure ptr -> int32
val hardcaml_cosim : Vpi.Systf_data.callback_t

val init_vpi : unit -> unit

