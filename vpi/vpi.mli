open Ctypes

val pli_int64 : int64 typ
val pli_uint64 : Unsigned.uint64 typ
val pli_int32 : int32 typ
val pli_uint32 : Unsigned.uint32 typ
val pli_int16 : int typ
val pli_uint16 : Unsigned.uint16 typ
val pli_byte8 : int typ
val pli_ubyte8 : Unsigned.uint8 typ

module Constants : sig
  val vpiSysTask : int32
  val vpiSysFunc : int32
  val vpiScaledRealTime : int32
  val vpiSimTime : int32
  val vpiSuppressTime : int32
  val vpiBinStrVal : int32
  val vpiOctStrVal : int32
  val vpiDecStrVal : int32
  val vpiHexStrVal : int32
  val vpiScalarVal : int32
  val vpiIntVal : int32
  val vpiRealVal : int32
  val vpiStringVal : int32
  val vpiVectorVal : int32
  val vpiStrengthVal : int32
  val vpiTimeVal : int32
  val vpiObjTypeVal : int32
  val vpiSuppressVal : int32
  val vpi0 : int32
  val vpi1 : int32
  val vpiZ : int32
  val vpiX : int32
  val vpiH : int32
  val vpiL : int32
  val vpiDontCare : int32
  val vpiSupplyDrive : int32
  val vpiStrongDrive : int32
  val vpiPullDrive : int32
  val vpiLargeCharge : int32
  val vpiWeakDrive : int32
  val vpiMediumCharge : int32
  val vpiSmallCharge : int32
  val vpiHiZ : int32
  val vpiConstant : int32
  val vpiFunction : int32
  val vpiIntegerVar : int32
  val vpiIterator : int32
  val vpiMemory : int32
  val vpiMemoryWord : int32
  val vpiModPath : int32
  val vpiModule : int32
  val vpiNamedBegin : int32
  val vpiNamedEvent : int32
  val vpiNamedFork : int32
  val vpiNet : int32
  val vpiParameter : int32
  val vpiPartSelect : int32
  val vpiPathTerm : int32
  val vpiRealVar : int32
  val vpiReg : int32
  val vpiSysFuncCall : int32
  val vpiSysTaskCall : int32
  val vpiTask : int32
  val vpiTimeVar : int32
  val vpiNetArray : int32
  val vpiIndex : int32
  val vpiLeftRange : int32
  val vpiParent : int32
  val vpiRightRange : int32
  val vpiScope : int32
  val vpiSysTfCall : int32
  val vpiArgument : int32
  val vpiInternalScope : int32
  val vpiModPathIn : int32
  val vpiModPathOut : int32
  val vpiVariables : int32
  val vpiExpr : int32
  val vpiCallback : int32
  val vpiUndefined : int32
  val vpiType : int32
  val vpiName : int32
  val vpiFullName : int32
  val vpiSize : int32
  val vpiFile : int32
  val vpiLineNo : int32
  val vpiTopModule : int32
  val vpiCellInstance : int32
  val vpiDefName : int32
  val vpiTimeUnit : int32
  val vpiTimePrecision : int32
  val vpiDefFile : int32
  val vpiDefLineNo : int32
  val vpiNetType : int32
  val vpiWire : int32
  val vpiWand : int32
  val vpiWor : int32
  val vpiTri : int32
  val vpiTri0 : int32
  val vpiTri1 : int32
  val vpiTriReg : int32
  val vpiTriAnd : int32
  val vpiTriOr : int32
  val vpiSupply1 : int32
  val vpiSupply0 : int32
  val vpiArray : int32
  val vpiEdge : int32
  val vpiNoEdge : int32
  val vpiEdge01 : int32
  val vpiEdge10 : int32
  val vpiEdge0x : int32
  val vpiEdgex1 : int32
  val vpiEdge1x : int32
  val vpiEdgex0 : int32
  val vpiPosedge : int32
  val vpiNegedge : int32
  val vpiAnyEdge : int32
  val vpiConstType : int32
  val vpiDecConst : int32
  val vpiRealConst : int32
  val vpiBinaryConst : int32
  val vpiOctConst : int32
  val vpiHexConst : int32
  val vpiStringConst : int32
  val vpiFuncType : int32
  val vpiIntFunc : int32
  val vpiRealFunc : int32
  val vpiTimeFunc : int32
  val vpiSizedFunc : int32
  val vpiSizedSignedFunc : int32
  val vpiSysFuncType : int32
  val vpiSysFuncInt : int32
  val vpiSysFuncReal : int32
  val vpiSysFuncTime : int32
  val vpiSysFuncSized : int32
  val vpiAutomatic : int32
  val vpiConstantSelect : int32
  val vpiSigned : int32
  val _vpiNexusId : int32
  val vpiNoDelay : int32
  val vpiInertialDelay : int32
  val vpiTransportDelay : int32
  val vpiPureTransportDelay : int32
  val vpiForceFlag : int32
  val vpiReleaseFlag : int32
  val vpiReturnEvent : int32
  val cbValueChange : int32
  val cbStmt : int32
  val cbForce : int32
  val cbRelease : int32
  val cbAtStartOfSimTime : int32
  val cbReadWriteSynch : int32
  val cbReadOnlySynch : int32
  val cbNextSimTime : int32
  val cbAfterDelay : int32
  val cbEndOfCompile : int32
  val cbStartOfSimulation : int32
  val cbEndOfSimulation : int32
  val cbError : int32
  val cbTchkViolation : int32
  val cbStartOfSave : int32
  val cbEndOfSave : int32
  val cbStartOfRestart : int32
  val cbEndOfRestart : int32
  val cbStartOfReset : int32
  val cbEndOfReset : int32
  val cbEnterInteractive : int32
  val cbExitInteractive : int32
  val cbInteractiveScopeChange : int32
  val cbUnresolvedSystf : int32
  val vpiStop : int32
  val vpiFinish : int32
  val vpiReset : int32
  val vpiSetInteractiveScope : int32
  val __ivl_legacy_vpiStop : int32
  val __ivl_legacy_vpiFinish : int32
  val vpiCompile : int32
  val vpiPLI : int32
  val vpiRun : int32
  val vpiNotice : int32
  val vpiWarning : int32
  val vpiError : int32
  val vpiSystem : int32
  val vpiInternal : int32
  val _vpiFromThr : int32
  val _vpiNoThr : int32
  val _vpiString : int32
  val _vpiVThr : int32
  val _vpiWord : int32
  val _vpi_at_PV : int32
  val _vpi_at_A : int32
  val _vpi_at_APV : int32
end

type ('a,'b) struct_field = ('a, ('b, [ `Struct ]) structured) field
type ('a,'b) union_field = ('a, ('b, [ `Union ]) structured) field

module Handle : sig
  type __t
  val __t : __t structure typ
  val t : __t structure ptr typ
  val null : __t structure ptr
end
module Systf_data : sig
  type t
  val t : t structure typ
  val type_ : (int32, t) struct_field
  val sysfunctype : (int32, t) struct_field
  val tfname : (string, t) struct_field
  type callback_t = (unit ptr -> int32)
  val callback_t : callback_t typ
  val calltf : (callback_t, t) struct_field
  val compiletf : (callback_t, t) struct_field
  val sizetf : (callback_t, t) struct_field
  val user_data : (unit ptr, t) struct_field
  val empty : (t, [ `Struct ]) structured
end
module Vlog_info : sig
  type t
  val t : t structure typ
  val argc : (int32, t) struct_field
  val argv : (string ptr, t) struct_field
  val product : (string, t) struct_field
  val version : (string, t) struct_field
end
module Time : sig
  type t
  val t : t structure typ
  val type_ : (int32, t) struct_field
  val high : (Unsigned.uint32, t) struct_field
  val low : (Unsigned.uint32, t) struct_field
  val real : (float, t) struct_field
  val null : t structure ptr
end
module Vecval : sig
  type t
  val t : t structure typ
  val aval : (int32, t) struct_field
  val bval : (int32, t) struct_field
end
module Strengthval : sig
  type t
  val t : t structure typ
  val logic : (int32, t) struct_field
  val s0 : (int32, t) struct_field
  val s1 : (int32, t) struct_field
end
module Value : sig
  type v
  val v : v union typ
  val str : (string, v) union_field
  val scalar : (int32, v) union_field
  val integer : (int32, v) union_field
  val real : (float, v) union_field
  val time : (Time.t structure ptr, v) union_field
  val vector : (Vecval.t structure ptr, v) union_field
  val strength : (Strengthval.t structure ptr, v) union_field
  val misc : (char ptr, v) union_field type t
  val t : t structure typ
  val format : (int32, t) struct_field
  val value : (v union, t) struct_field
  val null : t structure ptr
end
module Delay : sig
  type t
  val t : t structure typ
  val da : (Time.t structure ptr, t) struct_field
  val no_of_delays : (int32, t) struct_field
  val time_type : (int32, t) struct_field
  val mtm_flag : (int32, t) struct_field
  val append_flag : (int32, t) struct_field
  val plusere_flag : (int32, t) struct_field
end
module Cb_data : sig
  type t
  val t : t structure typ
  val reason : (int32, t) struct_field
  val cb_rtn : (t structure ptr -> int32, t) struct_field
  val obj : (Handle.__t structure ptr, t) struct_field
  val time : (Time.t structure ptr, t) struct_field
  val value : (Value.t structure ptr, t) struct_field
  val index : (int32, t) struct_field
  val user_data : (unit ptr, t) struct_field
end
module Error_info : sig
  type t
  val t : t structure typ
  val state : (int32, t) struct_field
  val level : (int32, t) struct_field
  val message : (string, t) struct_field
  val product : (string, t) struct_field
  val code : (string, t) struct_field
  val file : (string, t) struct_field
  val line : (int32, t) struct_field
end

type vpiHandle = Handle.__t structure ptr 
val vpiHandle : vpiHandle typ
type cfile_t
val cfile_t : cfile_t structure typ

val vpi_register_systf : Systf_data.t structure ptr -> unit
val vpi_mcd_open : string -> Unsigned.uint32
val vpi_mcd_close : Unsigned.uint32 -> Unsigned.uint32
val vpi_mcd_name : Unsigned.uint32 -> string
val vpi_flush : unit -> int32
val vpi_mcd_flush : Unsigned.uint32 -> int32
val vpi_fopen : string -> string -> int32
val vpi_get_file : int32 -> cfile_t structure ptr
val vpi_register_cb : Cb_data.t structure ptr -> vpiHandle
val vpi_remove_cb : vpiHandle -> int32
val vpi_control : int32 -> unit
val vpi_sim_control : int32 -> unit
val vpi_handle : int32 -> vpiHandle -> vpiHandle
val vpi_iterate : int32 -> vpiHandle -> vpiHandle
val vpi_scan : vpiHandle -> vpiHandle
val vpi_handle_by_index : vpiHandle -> int32 -> vpiHandle
val vpi_handle_by_name : string -> vpiHandle -> vpiHandle
val vpi_get_time : vpiHandle -> Time.t structure ptr -> unit
val vpi_get : int -> vpiHandle -> int32
val vpi_get_str : int32 -> vpiHandle -> string
val vpi_get_value : vpiHandle -> Value.t structure ptr -> unit
val vpi_put_value : vpiHandle -> Value.t structure ptr -> Time.t structure ptr -> int32 -> vpiHandle
val vpi_free_object : vpiHandle -> int32
val vpi_get_vlog_info : Vlog_info.t structure ptr -> int32
val vpi_get_delays : vpiHandle -> Delay.t structure ptr -> unit
val vpi_put_delays : vpiHandle -> Delay.t structure ptr -> unit
val vpi_put_userdata : vpiHandle -> unit ptr -> int32
val vpi_get_userdata : vpiHandle -> unit ptr
val vpi_chk_error : Error_info.t structure ptr -> int32
val vpip_format_strength : string -> Value.t structure ptr -> Unsigned.uint32 -> unit
val vpip_set_return_value : int -> unit
val vpip_calc_clog2 : vpiHandle -> Vecval.t structure
