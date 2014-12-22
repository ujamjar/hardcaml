open Ctypes

val u64_of_u32 : Unsigned.UInt32.t -> Unsigned.UInt64.t
val u32_of_u64 : Unsigned.UInt64.t -> Unsigned.UInt32.t
val get_time : unit -> Unsigned.UInt64.t
val set_time : Unsigned.UInt64.t -> Vpi.Time.t structure
val vpi_fold : Vpi.vpiHandle -> int32 -> ('a -> Vpi.vpiHandle -> 'a) -> 'a -> 'a
val once_only : ('a -> 'b) -> 'a -> 'b
val at_time_0 : ('a -> 'b) -> 'a -> 'b
val register_task : bytes -> Vpi.Systf_data.callback_t -> unit
val register_callback : 
  ?user_data:unit ptr -> ?obj:Vpi.vpiHandle -> ?value:Vpi.Value.t structure ptr ->
  reason:int32 -> time:Vpi.Time.t structure ptr -> (Vpi.Cb_data.t structure ptr -> int32) -> unit

type globals = 
  {
    mutable from_handle : Vpi.vpiHandle;
    mutable to_handle : Vpi.vpiHandle;
    mutable vpi_time : Unsigned.UInt64.t;
    mutable vlog_time : Unsigned.UInt64.t;
    mutable remote_time : Unsigned.UInt64.t;
    mutable delta : int;
    mutable changeFlag : bool array;
  }

val time_0 : Vpi.Time.t structure
val time_1 : Vpi.Time.t structure

val change_callback : Vpi.Cb_data.t structure ptr -> int32
val readonly_callback : Vpi.Cb_data.t structure ptr -> int32
val delta_callback : Vpi.Cb_data.t structure ptr -> int32
val delay_callback : Vpi.Cb_data.t structure ptr -> int32

val from_task : 'a -> int32
val to_task : 'a -> int32

val init_vpi : unit -> unit

