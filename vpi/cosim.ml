open Ctypes
open PosixTypes
open Foreign

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

(* function called at vpi startup.  
* here we get to register functions *)
let init_vpi () = 
  let () = register_task "$from_task" (once_only @@ at_time_0 @@ from_task) in
  let () = register_task "$to_task" (once_only @@ at_time_0 @@ to_task) in
  ()

let () = Callback.register "init_vpi" init_vpi

