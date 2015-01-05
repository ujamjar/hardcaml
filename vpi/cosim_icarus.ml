(*
 * * hardcaml spawns icarus with testbench and vpi module
 * * vpi installs a callback at time 0 which does set up
 * * comms channel set up to host over tcp socket
 * * sim object queried to find ports
 * * hardcaml sends control messages repeatedly which ...
 *  - set inputs (ie clock, module inputs)
 *  - read outputs and send values back
 *  - advance time to the next callback
 *  - quit
 * * simulation progresses step by step under control of hardcaml
 *
 * The types of messages exchanged are defined in HardCaml.Cosim.
 *)

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

let time_0 = set_time Unsigned.UInt64.zero

open Vpi
open Constants
open HardCaml.Cosim

module SMap = Map.Make(struct
  type t = string
  let compare = compare
end)

type cosim_state = Vpi.vpiHandle SMap.t

let vpi_recv_ctrl socket = Comms.recv socket
let vpi_get_ctrl data = 
  match (Marshal.from_bytes data 0 : control_message) with
  | Finish -> ignore @@ vpi_control vpiFinish; failwith "finished"
  | Run(control) -> control

(* set values *)
let set_values state message = 
  List.iter 
    (fun (name,bin_value) ->
      let handle = try SMap.find name state with Not_found -> failwith ("set_values: " ^ name) in
      let value = make Value.t in
      let () = setf value Value.format vpiBinStrVal in
      let data = make Value.v in
      let () = setf data Value.str bin_value in
      let () = setf value Value.value data in
      let _ = vpi_put_value handle (addr value) (addr time_0) vpiNoDelay in (* IntertailDelay? *)
      ()) 
    message.sets

(* get values *)
let get_values state message = 
  List.map 
    (fun name ->
      let handle = try SMap.find name state with Not_found -> failwith ("get_values: " ^ name) in
      let value = make Value.t in
      let () = setf value Value.format vpiBinStrVal in
      let () = vpi_get_value handle (addr value) in
      let value = getf (getf value Value.value) Value.str in
      name, value) 
    message.gets

let init_state ports = 
  let add_port map handle = 
    let name = vpi_get_str vpiName handle in
    if List.mem name ports then SMap.add name handle map
    else map
  in
  (* find all nets and regs in the (supposed to be only) top level module *)
  vpi_fold Handle.null vpiModule
    (fun map h -> 
      (* Note; may need to filter on name of top module ... *)
      let map = vpi_fold h vpiNet add_port map in
      let map = vpi_fold h vpiReg add_port map in
      map
    ) SMap.empty

let reason = cbAfterDelay (* cbReadWriteSynch? *)

let rec run_cosim (client,state) _ = 
  let open Printf in
  (* get message from server *)
  let message = vpi_get_ctrl (vpi_recv_ctrl client) in
  (* set values *)
  let () = set_values state message in
  (* get values *)
  let gets = get_values state message in
  (* response message to server *)
  let _ = 
    if gets <> [] then  
      ignore @@ Comms.send client (Marshal.to_bytes (gets : response_message) []) 
  in
  (* schedule next callback *)
  let time = addr (set_time (Unsigned.UInt64.of_int64 message.delta_time)) in
  register_callback ~reason ~time (run_cosim (client,state));
  0l

let hardcaml_cosim _ = 
  let client = Comms.create_client net_addr net_port in
  let _ = at_exit (fun _ -> Unix.close client) in
  (* say hello *)
  let _ = Comms.send_string client "hello hardcaml" in
  (* get back list of ports from server *)
  let ports = (Marshal.from_bytes (Comms.recv client) 0 : init_message) in
  (* find the ports in simulation object *)
  let state = init_state ports in
  (* start simulation at time 0 *)
  register_callback ~reason ~time:(addr time_0) (run_cosim (client,state));
  0l

(* function called at vpi startup. *)
let init_vpi () = 
  register_callback ~reason ~time:(addr time_0) hardcaml_cosim

let () = Callback.register "init_vpi" init_vpi

