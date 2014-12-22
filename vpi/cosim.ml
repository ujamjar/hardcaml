open Ctypes
open PosixTypes
open Foreign

(* VPI bindings *)

module Vpi = struct

  let pli_int64 = int64_t
  let pli_uint64 = uint64_t
  let pli_int32 = int32_t
  let pli_uint32 = uint32_t
  let pli_int16 = int16_t
  let pli_uint16 = uint16_t
  let pli_byte8 = int8_t
  let pli_ubyte8 = uint8_t

  module Constants = struct
    let find = foreign "vpi_get_constant" (string @-> int @-> returning uint32_t)
    let find n v = Unsigned.UInt32.to_int32 (find n v)
    let find n = 
      if find n 0 = 0l then failwith ("Constant not found: " ^ n)
      else find n 1
    let vpiSysTask                = find "vpiSysTask"
    let vpiSysFunc                = find "vpiSysFunc"
    let vpiScaledRealTime         = find "vpiScaledRealTime"
    let vpiSimTime                = find "vpiSimTime"
    let vpiSuppressTime           = find "vpiSuppressTime"
    let vpiBinStrVal              = find "vpiBinStrVal"
    let vpiOctStrVal              = find "vpiOctStrVal"
    let vpiDecStrVal              = find "vpiDecStrVal"
    let vpiHexStrVal              = find "vpiHexStrVal"
    let vpiScalarVal              = find "vpiScalarVal"
    let vpiIntVal                 = find "vpiIntVal"
    let vpiRealVal                = find "vpiRealVal"
    let vpiStringVal              = find "vpiStringVal"
    let vpiVectorVal              = find "vpiVectorVal"
    let vpiStrengthVal            = find "vpiStrengthVal"
    let vpiTimeVal                = find "vpiTimeVal"
    let vpiObjTypeVal             = find "vpiObjTypeVal"
    let vpiSuppressVal            = find "vpiSuppressVal"
    let vpi0                      = find "vpi0"
    let vpi1                      = find "vpi1"
    let vpiZ                      = find "vpiZ"
    let vpiX                      = find "vpiX"
    let vpiH                      = find "vpiH"
    let vpiL                      = find "vpiL"
    let vpiDontCare               = find "vpiDontCare"
    let vpiSupplyDrive            = find "vpiSupplyDrive"
    let vpiStrongDrive            = find "vpiStrongDrive"
    let vpiPullDrive              = find "vpiPullDrive"
    let vpiLargeCharge            = find "vpiLargeCharge"
    let vpiWeakDrive              = find "vpiWeakDrive"
    let vpiMediumCharge           = find "vpiMediumCharge"
    let vpiSmallCharge            = find "vpiSmallCharge"
    let vpiHiZ                    = find "vpiHiZ"
    let vpiConstant               = find "vpiConstant"
    let vpiFunction               = find "vpiFunction"
    let vpiIntegerVar             = find "vpiIntegerVar"
    let vpiIterator               = find "vpiIterator"
    let vpiMemory                 = find "vpiMemory"
    let vpiMemoryWord             = find "vpiMemoryWord"
    let vpiModPath                = find "vpiModPath"
    let vpiModule                 = find "vpiModule"
    let vpiNamedBegin             = find "vpiNamedBegin"
    let vpiNamedEvent             = find "vpiNamedEvent"
    let vpiNamedFork              = find "vpiNamedFork"
    let vpiNet                    = find "vpiNet"
    let vpiParameter              = find "vpiParameter"
    let vpiPartSelect             = find "vpiPartSelect"
    let vpiPathTerm               = find "vpiPathTerm"
    let vpiRealVar                = find "vpiRealVar"
    let vpiReg                    = find "vpiReg"
    let vpiSysFuncCall            = find "vpiSysFuncCall"
    let vpiSysTaskCall            = find "vpiSysTaskCall"
    let vpiTask                   = find "vpiTask"
    let vpiTimeVar                = find "vpiTimeVar"
    let vpiNetArray               = find "vpiNetArray"
    let vpiIndex                  = find "vpiIndex"
    let vpiLeftRange              = find "vpiLeftRange"
    let vpiParent                 = find "vpiParent"
    let vpiRightRange             = find "vpiRightRange"
    let vpiScope                  = find "vpiScope"
    let vpiSysTfCall              = find "vpiSysTfCall"
    let vpiArgument               = find "vpiArgument"
    let vpiInternalScope          = find "vpiInternalScope"
    let vpiModPathIn              = find "vpiModPathIn"
    let vpiModPathOut             = find "vpiModPathOut"
    let vpiVariables              = find "vpiVariables"
    let vpiExpr                   = find "vpiExpr"
    let vpiCallback               = find "vpiCallback"
    let vpiUndefined              = find "vpiUndefined"
    let vpiType                   = find "vpiType"
    let vpiName                   = find "vpiName"
    let vpiFullName               = find "vpiFullName"
    let vpiSize                   = find "vpiSize"
    let vpiFile                   = find "vpiFile"
    let vpiLineNo                 = find "vpiLineNo"
    let vpiTopModule              = find "vpiTopModule"
    let vpiCellInstance           = find "vpiCellInstance"
    let vpiDefName                = find "vpiDefName"
    let vpiTimeUnit               = find "vpiTimeUnit"
    let vpiTimePrecision          = find "vpiTimePrecision"
    let vpiDefFile                = find "vpiDefFile"
    let vpiDefLineNo              = find "vpiDefLineNo"
    let vpiNetType                = find "vpiNetType"
    let vpiWire                   = find "vpiWire"
    let vpiWand                   = find "vpiWand"
    let vpiWor                    = find "vpiWor"
    let vpiTri                    = find "vpiTri"
    let vpiTri0                   = find "vpiTri0"
    let vpiTri1                   = find "vpiTri1"
    let vpiTriReg                 = find "vpiTriReg"
    let vpiTriAnd                 = find "vpiTriAnd"
    let vpiTriOr                  = find "vpiTriOr"
    let vpiSupply1                = find "vpiSupply1"
    let vpiSupply0                = find "vpiSupply0"
    let vpiArray                  = find "vpiArray"
    let vpiEdge                   = find "vpiEdge"
    let vpiNoEdge                 = find "vpiNoEdge"
    let vpiEdge01                 = find "vpiEdge01"
    let vpiEdge10                 = find "vpiEdge10"
    let vpiEdge0x                 = find "vpiEdge0x"
    let vpiEdgex1                 = find "vpiEdgex1"
    let vpiEdge1x                 = find "vpiEdge1x"
    let vpiEdgex0                 = find "vpiEdgex0"
    let vpiPosedge                = find "vpiPosedge"
    let vpiNegedge                = find "vpiNegedge"
    let vpiAnyEdge                = find "vpiAnyEdge"
    let vpiConstType              = find "vpiConstType"
    let vpiDecConst               = find "vpiDecConst"
    let vpiRealConst              = find "vpiRealConst"
    let vpiBinaryConst            = find "vpiBinaryConst"
    let vpiOctConst               = find "vpiOctConst"
    let vpiHexConst               = find "vpiHexConst"
    let vpiStringConst            = find "vpiStringConst"
    let vpiFuncType               = find "vpiFuncType"
    let vpiIntFunc                = find "vpiIntFunc"
    let vpiRealFunc               = find "vpiRealFunc"
    let vpiTimeFunc               = find "vpiTimeFunc"
    let vpiSizedFunc              = find "vpiSizedFunc"
    let vpiSizedSignedFunc        = find "vpiSizedSignedFunc"
    let vpiSysFuncType            = find "vpiSysFuncType"
    let vpiSysFuncInt             = find "vpiSysFuncInt"
    let vpiSysFuncReal            = find "vpiSysFuncReal"
    let vpiSysFuncTime            = find "vpiSysFuncTime"
    let vpiSysFuncSized           = find "vpiSysFuncSized"
    let vpiAutomatic              = find "vpiAutomatic"
    let vpiConstantSelect         = find "vpiConstantSelect"
    let vpiSigned                 = find "vpiSigned"
    let _vpiNexusId               = find "_vpiNexusId"
    let vpiNoDelay                = find "vpiNoDelay"
    let vpiInertialDelay          = find "vpiInertialDelay"
    let vpiTransportDelay         = find "vpiTransportDelay"
    let vpiPureTransportDelay     = find "vpiPureTransportDelay"
    let vpiForceFlag              = find "vpiForceFlag"
    let vpiReleaseFlag            = find "vpiReleaseFlag"
    let vpiReturnEvent            = find "vpiReturnEvent"
    let cbValueChange             = find "cbValueChange"
    let cbStmt                    = find "cbStmt"
    let cbForce                   = find "cbForce"
    let cbRelease                 = find "cbRelease"
    let cbAtStartOfSimTime        = find "cbAtStartOfSimTime"
    let cbReadWriteSynch          = find "cbReadWriteSynch"
    let cbReadOnlySynch           = find "cbReadOnlySynch"
    let cbNextSimTime             = find "cbNextSimTime"
    let cbAfterDelay              = find "cbAfterDelay"
    let cbEndOfCompile            = find "cbEndOfCompile"
    let cbStartOfSimulation       = find "cbStartOfSimulation"
    let cbEndOfSimulation         = find "cbEndOfSimulation"
    let cbError                   = find "cbError"
    let cbTchkViolation           = find "cbTchkViolation"
    let cbStartOfSave             = find "cbStartOfSave"
    let cbEndOfSave               = find "cbEndOfSave"
    let cbStartOfRestart          = find "cbStartOfRestart"
    let cbEndOfRestart            = find "cbEndOfRestart"
    let cbStartOfReset            = find "cbStartOfReset"
    let cbEndOfReset              = find "cbEndOfReset"
    let cbEnterInteractive        = find "cbEnterInteractive"
    let cbExitInteractive         = find "cbExitInteractive"
    let cbInteractiveScopeChange  = find "cbInteractiveScopeChange"
    let cbUnresolvedSystf         = find "cbUnresolvedSystf"
    let vpiStop                   = find "vpiStop"
    let vpiFinish                 = find "vpiFinish"
    let vpiReset                  = find "vpiReset"
    let vpiSetInteractiveScope    = find "vpiSetInteractiveScope"
    let __ivl_legacy_vpiStop      = find "__ivl_legacy_vpiStop"
    let __ivl_legacy_vpiFinish    = find "__ivl_legacy_vpiFinish"
    let vpiCompile                = find "vpiCompile"
    let vpiPLI                    = find "vpiPLI"
    let vpiRun                    = find "vpiRun"
    let vpiNotice                 = find "vpiNotice"
    let vpiWarning                = find "vpiWarning"
    let vpiError                  = find "vpiError"
    let vpiSystem                 = find "vpiSystem"
    let vpiInternal               = find "vpiInternal"
    let _vpiFromThr               = find "_vpiFromThr"
    let _vpiNoThr                 = find "_vpiNoThr"
    let _vpiString                = find "_vpiString"
    let _vpiVThr                  = find "_vpiVThr"
    let _vpiWord                  = find "_vpiWord"
    let _vpi_at_PV                = find "_vpi_at_PV"
    let _vpi_at_A                 = find "_vpi_at_A"
    let _vpi_at_APV               = find "_vpi_at_APV"
  end

  type ('a,'b) struct_field = ('a, ('b, [ `Struct ]) structured) field
  type ('a,'b) union_field = ('a, ('b, [ `Union ]) structured) field

  module Handle = struct
    type __t
    let __t : __t structure typ = structure "__vpiHandle"
    let t = ptr __t
    let null = from_voidp __t null
  end

  module Systf_data = struct
    type t
    let t : t structure typ = structure "t_vpi_systf_data"
    let type_ = field t "type" pli_int32
    let sysfunctype = field t "sysfunctype" pli_int32
    let tfname = field t "tfname" string
    type callback_t = (unit ptr -> int32)
    let callback_t = funptr (ptr void @-> returning pli_int32)
    let calltf = field t "calltf" callback_t
    let compiletf = field t "compiletf" callback_t
    let sizetf = field t "sizetf" callback_t
    let user_data = field t "user_data" (ptr void)
    let () = seal t

    let empty = 
      let task = make t in
      let () = setf task type_ Constants.vpiSysTask in
      let () = setf task sysfunctype 0l in
      let () = setf task tfname "" in
      let () = setf task calltf (fun _ -> 0l) in
      let () = setf task compiletf (fun _ -> 0l) in
      let () = setf task sizetf (fun _ -> 0l) in
      let () = setf task user_data null in
      task

  end

  module Vlog_info = struct
    type t
    let t : t structure typ = structure "t_vpi_vlog_info"
    let argc = field t "argc" pli_int32
    let argv = field t "argv" (ptr string) (* xxx array string? *)
    let product = field t "product" string
    let version = field t "version" string
    let () = seal t
  end

  module Time = struct
    type t 
    let t : t structure typ = structure "t_vpi_time"
    let type_ = field t "type" pli_int32
    let high = field t "high" pli_uint32
    let low = field t "low" pli_uint32
    let real = field t "real" double
    let () = seal t
  end

  module Vecval = struct
    type t 
    let t : t structure typ = structure "t_vpi_vecval"
    let aval = field t "aval" pli_int32
    let bval = field t "bval" pli_int32
    let () = seal t
  end

  module Strengthval = struct
    type t 
    let t : t structure typ = structure "t_vpi_strengthval"
    let logic = field t "logic" pli_int32
    let s0 = field t "s0" pli_int32
    let s1 = field t "s1" pli_int32
    let () = seal t
  end

  module Value = struct
    type v
    let v : v union typ = union "u_value"
    let str = field v "str" string
    let scalar = field v "scalar" pli_int32
    let integer = field v "integer" pli_int32
    let real = field v "real" double
    let time = field v "time" (ptr Time.t)
    let vector = field v "vector" (ptr Vecval.t)
    let strength = field v "strength" (ptr Strengthval.t)
    let misc = field v "misc" (ptr char)
    let () = seal v

    type t
    let t : t structure typ = structure "t_vpi_value"
    let format = field t "format" pli_int32
    let value = field t "value" v
    let () = seal t

    let null = from_voidp t null
  end

  module Delay = struct
    type t 
    let t : t structure typ = structure "t_vpi_delay"
    let da = field t "da" (ptr Time.t)
    let no_of_delays = field t "no_of_delays" pli_int32
    let time_type = field t "time_type" pli_int32
    let mtm_flag = field t "mtm_flag" pli_int32
    let append_flag = field t "append_flag" pli_int32
    let plusere_flag = field t "plusere_flag" pli_int32
    let () = seal t
  end

  module Cb_data = struct
    type t
    let t : t structure typ = structure "t_cb_data"
    let reason = field t "reason" pli_int32
    let cb_rtn = field t "cb_rtn" (funptr (ptr t @-> returning pli_int32))
    let obj = field t "obj" Handle.t
    let time = field t "time" (ptr Time.t)
    let value = field t "value" (ptr Value.t)
    let index = field t "index" pli_int32
    let user_data = field t "user_data" (ptr void)
    let () = seal t
  end

  module Error_info = struct
    type t
    let t : t structure typ = structure "t_vpi_error_info"
    let state = field t "state" pli_int32
    let level = field t "level" pli_int32
    let message = field t "message" string
    let product = field t "product" string
    let code = field t "code" string
    let file = field t "file" string
    let line = field t "line" pli_int32
    let () = seal t
  end

  (* types in vpi interface *)
  let typedef x = x, ptr x
  type vpiHandle = Handle.__t structure ptr 
  let vpiHandle = Handle.t
  let s_vpi_systf_data, p_vpi_systf_data = typedef Systf_data.t
  let s_vpi_vlog_info, p_vpi_vlog_info = typedef Vlog_info.t
  let s_vpi_time, p_vpi_time = typedef Time.t
  let s_vpi_vecval, p_vpi_vecval = typedef Vecval.t
  let s_vpi_strengthval, p_vpi_strengthval = typedef Strengthval.t
  let s_vpi_value, p_vpi_value = typedef Value.t
  let s_vpi_delay, p_vpi_delay = typedef Delay.t
  let s_cb_data, p_cb_data = typedef Cb_data.t
  let s_vpi_error_info, p_vpi_error_info = typedef Error_info.t

  let vpi_register_systf = foreign "vpi_register_systf" (p_vpi_systf_data @-> returning void)

  let vpi_mcd_open = foreign "vpi_mcd_open" (string @-> returning pli_uint32)
  let vpi_mcd_close = foreign "vpi_mcd_close" (pli_uint32 @-> returning pli_uint32)
  let vpi_mcd_name = foreign "vpi_mcd_name" (pli_uint32 @-> returning string)
  (*extern PLI_INT32  vpi_mcd_printf(PLI_UINT32 mcd, const char*fmt, ...)
        __attribute__((format (printf,2,3)));

  extern PLI_INT32  vpi_printf(const char*fmt, ...)
        __attribute__((format (printf,1,2)));

  extern PLI_INT32  vpi_vprintf(const char*fmt, va_list ap);
  extern PLI_INT32  vpi_mcd_vprintf(PLI_UINT32 mcd, const char*fmt, va_list ap);*)

  let vpi_flush = foreign "vpi_flush" (void @-> returning pli_int32)
  let vpi_mcd_flush = foreign "vpi_mcd_flush" (pli_uint32 @-> returning pli_int32)

  type cfile_t
  let cfile_t : cfile_t structure typ = structure "FILE"

  let vpi_fopen = foreign "vpi_fopen" (string @-> string @-> returning pli_int32)
  let vpi_get_file = foreign "vpi_get_file" (pli_int32 @-> returning (ptr cfile_t))

  let vpi_register_cb = foreign "vpi_register_cb" (p_cb_data @-> returning vpiHandle)
  let vpi_remove_cb = foreign "vpi_remove_cb" (vpiHandle @-> returning pli_int32)

  (* XXX: note; these are actually variadic ... *)
  let vpi_control = foreign "vpi_control" (pli_int32 @-> returning void)
  let vpi_sim_control = foreign "vpi_sim_control" (pli_int32 @-> returning void)

  let vpi_handle = foreign "vpi_handle" (pli_int32 @-> vpiHandle @-> returning vpiHandle)
  let vpi_iterate = foreign "vpi_iterate" (pli_int32 @-> vpiHandle @-> returning vpiHandle )
  let vpi_scan = foreign "vpi_scan" (vpiHandle @-> returning vpiHandle)
  let vpi_handle_by_index = foreign "vpi_handle_by_index" 
    (vpiHandle @-> pli_int32 @-> returning vpiHandle)
  let vpi_handle_by_name = foreign "vpi_handle_by_name" 
    (string @-> vpiHandle @-> returning vpiHandle)

  let vpi_get_time = foreign "vpi_get_time" (vpiHandle @-> p_vpi_time @-> returning void)
  let vpi_get = foreign "vpi_get" (int @-> vpiHandle @-> returning pli_int32)
  let vpi_get_str = foreign "vpi_get_str" (pli_int32 @-> vpiHandle @-> returning string)
  let vpi_get_value = foreign "vpi_get_value" (vpiHandle @-> p_vpi_value @-> returning void)

  let vpi_put_value = foreign "vpi_put_value" 
    (vpiHandle @-> p_vpi_value @-> p_vpi_time @-> pli_int32 @-> returning vpiHandle)

  let vpi_free_object = foreign "vpi_free_object" (vpiHandle @-> returning pli_int32)
  let vpi_get_vlog_info = foreign "vpi_get_vlog_info" (p_vpi_vlog_info @-> returning pli_int32)

  let vpi_get_delays = foreign "vpi_get_delays" (vpiHandle @-> p_vpi_delay @-> returning void)
  let vpi_put_delays = foreign "vpi_put_delays" (vpiHandle @-> p_vpi_delay @-> returning void)

  let vpi_put_userdata = foreign "vpi_put_userdata" (vpiHandle @-> ptr void @-> returning pli_int32)
  let vpi_get_userdata =  foreign "vpi_get_userdata" (vpiHandle @-> returning (ptr void))

  let vpi_chk_error = foreign "vpi_chk_error" (p_vpi_error_info @-> returning pli_int32)

  (* icarus verilog only *)
  let vpip_format_strength = foreign "vpip_format_strength" 
    (string @-> p_vpi_value @-> uint32_t @-> returning void)
  let vpip_set_return_value = foreign "vpip_set_return_value" (int @-> returning void)
  let vpip_calc_clog2 = foreign "vpip_calc_clog2" (vpiHandle @-> returning s_vpi_vecval)

end

let u64_of_u32 v = 
  let v = Unsigned.(UInt64.of_int64 @@ Int64.of_int32 @@ UInt32.to_int32 v) in
  let mask = Unsigned.UInt64.of_int64 0xFFFFFFFFL in
  Unsigned.UInt64.Infix.(v land mask)

let u32_of_u64 v = 
  let v = Unsigned.(UInt32.of_int32 @@ Int64.to_int32 @@ UInt64.to_int64 v) in
  v

let get_time () =
  let open Vpi in
  let time = make Time.t in
  let () = setf time Time.type_ Constants.vpiSimTime in
  let () = vpi_get_time Handle.null (addr time) in
  let high = u64_of_u32 (getf time Time.high) in
  let low = u64_of_u32 (getf time Time.low) in
  Unsigned.UInt64.Infix.((high lsl 32) lor low)

let set_time time = 
  let open Vpi in
  let t = make Time.t in
  let () = setf t Time.type_ Constants.vpiSimTime in
  let () = setf t Time.low (u32_of_u64 time) in
  let () = setf t Time.high (u32_of_u64 Unsigned.UInt64.Infix.(time lsr 32)) in
  t

let vpi_fold handle const f arg = 
  let open Vpi in
  let iter = vpi_iterate const handle in
  let rec scan arg = 
    let handle = vpi_scan iter in
    if handle = Handle.null then arg
    else scan (f arg handle)
  in
  if iter = Handle.null then arg else scan arg

let once_only f = 
  let first = ref true in
  (fun a -> 
    if !first then begin
      first := false;
      f a
    end else begin
      failwith "task must be called once only"
    end)

let at_time_0 f = 
  (fun a ->
    if get_time () = Unsigned.UInt64.zero then f a
    else failwith "task must be called at time 0 only")

let register_task name fcall = 
  let open Vpi in
  let open Constants in
  let task = make Systf_data.t in
  let () = setf task Systf_data.type_ vpiSysTask in
  let () = setf task Systf_data.sysfunctype 0l in
  let () = setf task Systf_data.tfname name in
  let () = setf task Systf_data.calltf fcall in
  let () = setf task Systf_data.compiletf (fun _ -> 0l) in
  let () = setf task Systf_data.sizetf (fun _ -> 0l) in
  let () = setf task Systf_data.user_data null in
  ignore @@ Vpi.vpi_register_systf (addr task) 

let register_callback 
  ?(user_data=null) ?(obj=Vpi.Handle.null) ?(value=Vpi.Value.null) 
  ~reason ~time fn =
  let open Vpi in
  let cb_data = make Cb_data.t in
  let () = setf cb_data Cb_data.reason reason in
  let () = setf cb_data Cb_data.time time in
  let () = setf cb_data Cb_data.obj obj in
  let () = setf cb_data Cb_data.value value in
  let () = setf cb_data Cb_data.index 0l in
  let () = setf cb_data Cb_data.user_data user_data in
  let () = setf cb_data Cb_data.cb_rtn fn in
  ignore @@ vpi_register_cb (addr cb_data)

module Cosim = struct

  (* globals *)
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

  let globals = 
    {
      from_handle = Vpi.Handle.null;
      to_handle = Vpi.Handle.null;
      vpi_time = Unsigned.UInt64.zero;
      vlog_time = Unsigned.UInt64.zero;
      remote_time = Unsigned.UInt64.zero;
      delta = 0;
      changeFlag = [||];
    }

  let from_task _ = 
    let open Vpi in
    let open Constants in
    globals.from_handle <- vpi_handle vpiSysTfCall Handle.null;
    let signals = vpi_fold globals.from_handle vpiArgument
      (fun l h ->
        (vpi_get_str vpiName h,
        vpi_get (Int32.to_int vpiSize) h,
        vpi_get (Int32.to_int vpiType) h) :: l)
      []
    in
    List.iter (fun (n,s,t) -> Printf.printf "from_task: %s[%li]:%li\n%!" n s t) (List.rev signals);
    0l

  let time_0 = set_time Unsigned.UInt64.zero
  let time_1 = set_time Unsigned.UInt64.one
  
  let change_callback cb_data = 
    let open Vpi in
    let open Constants in
    Printf.printf "change_callback %Li\n%!" Unsigned.UInt64.(to_int64 (get_time())); 
    let index = !@ (from_voidp int (getf (!@ cb_data) Cb_data.user_data)) in
    globals.changeFlag.(index) <- true;
    0l
  
  let rec readonly_callback _ = 
    let open Vpi in
    let open Constants in
    Printf.printf "readonly_callback %Li\n%!" Unsigned.UInt64.(to_int64 (get_time())); 
    (* XXX startup *)
    globals.vlog_time <- get_time();
    let _ = vpi_fold globals.to_handle vpiArgument 
      (fun n h -> 
        if globals.changeFlag.(n) then begin
          (* get value *)
          globals.changeFlag.(n) <- false;
        end;
        n+1)
      0
    in
    (* XXX send data *)
    (* store data for later callback *)
    (* read time, calculate delay *)
    let delay = Unsigned.UInt64.zero in
    if delay <> Unsigned.UInt64.zero then begin
      let time = set_time delay in
      register_callback ~reason:cbAfterDelay ~time:(addr time) delay_callback
    end else begin
      globals.delta <- globals.delta + 1
    end;
    0l
  
  and delta_callback _ = 
    let open Vpi in
    let open Constants in
    if globals.delta <> 0 then begin
      Printf.printf "delta_callback %Li\n%!" Unsigned.UInt64.(to_int64 (get_time())); 
      (* XXX set inputs *)
      register_callback ~reason:cbReadOnlySynch ~time:(addr time_0) readonly_callback;
      register_callback ~reason:cbAfterDelay ~time:(addr time_1) delta_callback;
    end;
    0l
  
  and delay_callback _ = 
    let open Vpi in
    let open Constants in
    Printf.printf "delay_callback %Li\n%!" Unsigned.UInt64.(to_int64 (get_time())); 
    register_callback ~reason:cbReadOnlySynch ~time:(addr time_0) readonly_callback;
    register_callback ~reason:cbAfterDelay ~time:(addr time_1) delta_callback;
    0l

  let to_task _ = 
    let open Vpi in
    let open Constants in
    globals.to_handle <- vpi_handle vpiSysTfCall Handle.null;
    globals.vlog_time <- get_time ();
    let time = make Time.t in
    let () = setf time Time.type_ vpiSuppressTime in
    let count = vpi_fold globals.to_handle vpiArgument
      (fun n h ->
        let user_data = to_voidp @@ allocate int n in
        register_callback ~obj:h ~user_data ~reason:cbValueChange ~time:(addr time) change_callback;
        n+1) 
      0
    in
    globals.changeFlag <- Array.make count false;
    Printf.printf "to_task: count=%i\n%!" count;
    register_callback ~reason:cbReadOnlySynch ~time:(addr time_0) readonly_callback;
    register_callback ~reason:cbAfterDelay ~time:(addr time_1) delta_callback;
    0l

  (* function called at vpi startup.  
  * here we get to register functions *)
  let init_vpi () = 
    let () = register_task "$from_task" (once_only @@ at_time_0 @@ from_task) in
    let () = register_task "$to_task" (once_only @@ at_time_0 @@ to_task) in
    ()

end

let () = Callback.register "init_vpi" Cosim.init_vpi

