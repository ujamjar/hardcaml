(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Circuit
open Signal.Types
open Signal.Comb
open Utils
open Printf

(***********************************************************************)
(* OCAML interface *)
let ocaml dut = "#ifdef OCAML
#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#ifdef Custom_tag
#include <caml/custom.h>
#include <caml/bigarray.h>
#endif
#include <caml/camlidlruntime.h>

void camlidl_ml2c_" ^ dut ^ "_struct__port(value _v1, struct _port * _c2, camlidl_ctx _ctx)
{
  value _v3;
  value _v4;
  value _v5;
  _v3 = Field(_v1, 0);
  (*_c2).name = camlidl_malloc_string(_v3, _ctx);
  _v4 = Field(_v1, 1);
  (*_c2).width = Int_val(_v4);
  _v5 = Field(_v1, 2);
  (*_c2).bits = Bigarray_val(_v5)->data;
}

value camlidl_c2ml_" ^ dut ^ "_struct__port(struct _port * _c1, camlidl_ctx _ctx)
{
  value _v2;
  value _v3[3];
  _v3[0] = _v3[1] = _v3[2] = 0;
  Begin_roots_block(_v3, 3)
    _v3[0] = copy_string((*_c1).name);
    _v3[1] = Val_int((*_c1).width);
    _v3[2] = alloc_bigarray_dims(
            BIGARRAY_INT32 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL,
            1, (*_c1).bits, (((*_c1).width + 31) / 32));
    _v2 = camlidl_alloc_small(3, 0);
    Field(_v2, 0) = _v3[0];
    Field(_v2, 1) = _v3[1];
    Field(_v2, 2) = _v3[2];
  End_roots()
  return _v2;
}

void camlidl_ml2c_" ^ dut ^ "_port(value _v1, port * _c2, camlidl_ctx _ctx)
{
  camlidl_ml2c_" ^ dut ^ "_struct__port(_v1, &(*_c2), _ctx);
}

value camlidl_c2ml_" ^ dut ^ "_port(port * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_c2ml_" ^ dut ^ "_struct__port(&(*_c2), _ctx);
  return _v1;
}

void camlidl_ml2c_" ^ dut ^ "_struct__simulator(value _v1, struct _simulator * _c2, camlidl_ctx _ctx)
{
  value _v3;
  value _v4;
  value _v5;
  value _v6;
  value _v7;
  mlsize_t _c8;
  mlsize_t _c9;
  value _v10;
  value _v11;
  mlsize_t _c12;
  mlsize_t _c13;
  value _v14;
  _v3 = Field(_v1, 0);
  (*_c2).data = (int *) Field(_v3, 0);
  _v4 = Field(_v1, 1);
  (*_c2).regs = (int *) Field(_v4, 0);
  _v5 = Field(_v1, 2);
  (*_c2).mems = (int *) Field(_v5, 0);
  _v6 = Field(_v1, 3);
  (*_c2).muxs = (int *) Field(_v6, 0);
  _v7 = Field(_v1, 4);
  _c8 = Wosize_val(_v7);
  (*_c2).in_ports = camlidl_malloc(_c8 * sizeof(port ), _ctx);
  for (_c9 = 0; _c9 < _c8; _c9++) {
    _v10 = Field(_v7, _c9);
    camlidl_ml2c_" ^ dut ^ "_port(_v10, &(*_c2).in_ports[_c9], _ctx);
  }
  (*_c2).num_in_ports = _c8;
  _v11 = Field(_v1, 5);
  _c12 = Wosize_val(_v11);
  (*_c2).out_ports = camlidl_malloc(_c12 * sizeof(port ), _ctx);
  for (_c13 = 0; _c13 < _c12; _c13++) {
    _v14 = Field(_v11, _c13);
    camlidl_ml2c_" ^ dut ^ "_port(_v14, &(*_c2).out_ports[_c13], _ctx);
  }
  (*_c2).num_out_ports = _c12;
}

value camlidl_c2ml_" ^ dut ^ "_struct__simulator(struct _simulator * _c1, camlidl_ctx _ctx)
{
  value _v2;
  value _v3[6];
  mlsize_t _c4;
  value _v5;
  mlsize_t _c6;
  value _v7;
  memset(_v3, 0, 6 * sizeof(value));
  Begin_roots_block(_v3, 6)
    _v3[0] = camlidl_alloc_small(1, Abstract_tag);
    Field(_v3[0], 0) = (value) (*_c1).data;
    _v3[1] = camlidl_alloc_small(1, Abstract_tag);
    Field(_v3[1], 0) = (value) (*_c1).regs;
    _v3[2] = camlidl_alloc_small(1, Abstract_tag);
    Field(_v3[2], 0) = (value) (*_c1).mems;
    _v3[3] = camlidl_alloc_small(1, Abstract_tag);
    Field(_v3[3], 0) = (value) (*_c1).muxs;
    _v3[4] = camlidl_alloc((*_c1).num_in_ports, 0);
    Begin_root(_v3[4])
      for (_c4 = 0; _c4 < (*_c1).num_in_ports; _c4++) {
        _v5 = camlidl_c2ml_" ^ dut ^ "_port(&(*_c1).in_ports[_c4], _ctx);
        modify(&Field(_v3[4], _c4), _v5);
      }
    End_roots()
    _v3[5] = camlidl_alloc((*_c1).num_out_ports, 0);
    Begin_root(_v3[5])
      for (_c6 = 0; _c6 < (*_c1).num_out_ports; _c6++) {
        _v7 = camlidl_c2ml_" ^ dut ^ "_port(&(*_c1).out_ports[_c6], _ctx);
        modify(&Field(_v3[5], _c6), _v7);
      }
    End_roots()
    _v2 = camlidl_alloc_small(6, 0);
    { mlsize_t _c8;
      for (_c8 = 0; _c8 < 6; _c8++) Field(_v2, _c8) = _v3[_c8];
    }
  End_roots()
  return _v2;
}

void camlidl_ml2c_" ^ dut ^ "_simulator(value _v1, simulator * _c2, camlidl_ctx _ctx)
{
  camlidl_ml2c_" ^ dut ^ "_struct__simulator(_v1, &(*_c2), _ctx);
}

value camlidl_c2ml_" ^ dut ^ "_simulator(simulator * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_c2ml_" ^ dut ^ "_struct__simulator(&(*_c2), _ctx);
  return _v1;
}

value camlidl_" ^ dut ^ "_init(value _unit)
{
  simulator *_res;
  value _v1;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _res = " ^ dut ^ ".init();
  if (_res == NULL) {
    _vres = Val_int(0);
  } else {
    _v1 = camlidl_c2ml_" ^ dut ^ "_simulator(&*_res, _ctx);
    Begin_root(_v1)
      _vres = camlidl_alloc_small(1, 0);
      Field(_vres, 0) = _v1;
    End_roots();
  }
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_" ^ dut ^ "_cycle(
	value _v_sim)
{
  simulator *sim; /*in*/
  value _v1;
  simulator _c2;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  if (_v_sim == Val_int(0)) {
    sim = NULL;
  } else {
    _v1 = Field(_v_sim, 0);
    sim = &_c2;
    camlidl_ml2c_" ^ dut ^ "_simulator(_v1, &_c2, _ctx);
  }
  " ^ dut ^ ".cycle(sim);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_" ^ dut ^ "_reset(
	value _v_sim)
{
  simulator *sim; /*in*/
  value _v1;
  simulator _c2;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  if (_v_sim == Val_int(0)) {
    sim = NULL;
  } else {
    _v1 = Field(_v_sim, 0);
    sim = &_c2;
    camlidl_ml2c_" ^ dut ^ "_simulator(_v1, &_c2, _ctx);
  }
  " ^ dut ^ ".reset(sim);
  camlidl_free(_ctx);
  return Val_unit;
}
#endif//OCAML
"

(***********************************************************************)
(* C testbench *)
let c dut = "#ifdef TEST

#include <stdio.h>
#include <string.h>

void error(char *msg) {
    fprintf(stderr, \"%s\\n\", msg);
    exit(-1);
}

int words(int n) { return (n+31) >> 5; }

uint32_t mask(int n) {
    n = n&31;
    if (n == 0) return 0xFFFFFFFF;
    else return 0xFFFFFFFF >> (32 - n);
}

void maska(uint32_t *a, int n) {
    int w = words(n) - 1;
    a[w] = a[w] & mask(n);
}

char *read_line(FILE *f) {
    int len = 32;
    char *s = malloc(len);
    char c;
    int i=0;
    do {
        // extend string
        if (i == len) {
            len *= 2;
            s = realloc(s, len);
        }
        // get char
        c = fgetc(f);
        // check for end of line
        if (c == '\\n' || c == '\\r' || c == '\\0' || c == EOF) {
            // return string
            s[i] = '\\0';
            return s;
        }
        // insert char
        s[i] = c;
        i++;
    } while (1);
}

struct sim_ports {
    port **ports;
    int *io;
    int num_ports;
};
char *delim = \";,\\t \";

char *copy(char *s) {
    int l = strlen(s) + 1;
    char *p = malloc(l);
    strcpy(p, s);
    return p;
}

port *port_by_name(port *p, int n, char *t) {
    int i;
    for (i=0; i<n; i++) {
        if (0 == strcmp(p[i].name, t)) return p + i;
    }
    return 0;
}

// read 1st line of csv file and look up ports in simulator
struct sim_ports get_ports(simulator *sim, FILE *f) {
    struct sim_ports ports;
    char *l = read_line(f), *p, *s;
    int cnt, i;

    // count ports
    s = copy(l);
    p = s;
    cnt = 0;
    while (strtok(p,delim)) { p = 0; cnt++; }
    free(s);

    // allocate port datastucture
    ports.ports = malloc(sizeof(port *) * cnt);
    ports.io = malloc(sizeof(int) * cnt);
    ports.num_ports = cnt;

    // look up ports
    s = copy(l);
    i = 0;
    p = l;
    while (i < cnt) {
        port *port;
        char *t = strtok(p,delim);
        p = 0;
        // check for input port
        if (port = port_by_name(sim->in_ports, sim->num_in_ports, t)) {
            ports.ports[i] = port;
            ports.io[i] = 1;
        // check for output port
        } else if (port = port_by_name(sim->out_ports, sim->num_out_ports, t)) {
            ports.ports[i] = port;
            ports.io[i] = 0;
        } else {
            error(\"couldnt find port\");
        }
        i++;
    }
    free(s);
    free(l);
    return ports;
}

uint32_t hex(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    else if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    else if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    else error(\"bad hex value\");
}

void set_value(simulator *sim, char *t, port *p) {
    int w = words(p->width);
    int w8 = w * 8;
    int l = strlen(t);
    int i,j;
    uint32_t *d = p->bits;

    // clip to length of array (before masking)
    if (l > w8) {
        t += (l-w8);
        l = w8;
    }
    for (i=0; i<w; i++) d[i] = 0;

    for (i=l-1,j=0; i>=0; i--,j++) {
        char c = t[i];
        d[j>>3] |= hex(t[i]) << ((j&7) << 2);
    }

    maska(d, p->width);
}

int set_input_ports(simulator *sim, struct sim_ports ports, char *s) {
    char *p, *l = copy(s);
    int i;

    i = 0;
    p = l;
    while (i < ports.num_ports) {
        // get token
        char *t = strtok(p, delim);
        if (t == NULL || t[0] == '\\0') return 0;
        p = 0;
        // set port value if its and input 
        if (ports.io[i] == 1) 
            set_value(sim, t, ports.ports[i]);
        i++;
    }

    free(l);
    return 1;
}

int check_output_ports(simulator *sim, struct sim_ports ports, char *s) {
    char *p, *l = copy(s);
    int i;

    i = 0;
    p = l;
    while (i < ports.num_ports) {
        // get token
        char *t = strtok(p, delim);
        if (t == NULL || t[0] == '\\0') return 0;
        p = 0;
        // check port value if its an output
        if (ports.io[i] == 0) {
            int j;
            int w = words(ports.ports[i]->width);
            uint32_t *d = malloc(sizeof(uint32_t)*w);
            uint32_t *q = ports.ports[i]->bits;
            memcpy(d, q, sizeof(uint32_t) * w);
            set_value(sim, t, ports.ports[i]);
            for (j=0; j<w; j++)
                if (d[j] != q[j]) error(\"check failed\");
        }
        i++;
    }

    free(l);
    return 1;
}

void write_output_port_names(simulator *sim, FILE *f) {
    int i=0;
    while (sim->out_ports[i].name) {
        fprintf(f,\"%s\", sim->out_ports[i].name);
        if (sim->out_ports[i+1].name) fprintf(f, \",\");
        i++;
    }
    fprintf(f, \"\\n\");
}

void write_output_ports(simulator *sim, FILE *f) {
    int i=0;
    while (sim->out_ports[i].name) {
        int w = words(sim->out_ports[i].width);
        int j;
        uint32_t *d = sim->out_ports[i].bits;
        for (j=w-1; j>=0; j--) fprintf(f,\"%.8x\", d[j]);
        if (sim->out_ports[i+1].name) fprintf(f, \",\");
        i++;
    }
    fprintf(f, \"\\n\");
}

int main(int argc, char *argv[]) {

    FILE *fin = stdin;
    FILE *fout = stdout;

    simulator *sim = " ^ dut ^ ".init();
    struct sim_ports ports;
    
    // write output names
    write_output_port_names(sim, fout);

    // find ports from csv file and associate with simulation
    ports = get_ports(sim, fin);

    // run the simulator
    " ^ dut ^ ".reset(sim);
    do {
        char *l = read_line(fin);
        if (set_input_ports(sim, ports, l)) {
            " ^ dut ^ ".cycle(sim);
            check_output_ports(sim, ports, l);
            write_output_ports(sim, fout);
        } else break;
    } while (1);

    return 0;
}

#endif//TEST

"

(***********************************************************************)
(***********************************************************************)
(***********************************************************************)

let array_mul
    os
    signed                  (* signed/unsigned multiplication *)
    words_r words_a words_b (* words in args *)
    bits_r bits_a bits_b    (* bits in args *)
    r a b                   (* offset of result and args in memory array *)
    =
    let soi = string_of_int in

    let mem_at a = "sim->data[" ^ soi a ^ "]" in
    let int_to_hex a = sprintf "%lx" a in
    
    let (<<.) = Int32.shift_left in

    (* return a string giving the value of the look up on the arg *)
    let access a words bits i =
        if i = words-1 then
            (* last word, sign extend if necessary *)
            if signed && (bits mod 32 <> 0) then (
                ("((" ^ mem_at (a + i) ^ " & 0x" ^ 
                    int_to_hex (1l <<. ((bits-1) mod 32)) ^ 
                    ") != 0 ? " ^ mem_at (a + i) ^ " | 0x" ^ 
                    int_to_hex (0xffffffffl <<. (bits mod 32)) ^ 
                    " : " ^ mem_at (a + i) ^ ")")
            ) else
                (mem_at (a + i))
        else if i >= words then
            if signed then 
                ("((" ^ mem_at (a + words - 1) ^ 
                    " & 0x" ^ int_to_hex (1l <<. ((bits-1) mod 32)) ^ 
                    ") != 0 ? 0xffffffff : 0)") 
            else "0"
        else
        (mem_at (a + i))
    in
    let access_a = access a words_a bits_a in
    let access_b = access b words_b bits_b in
      
    if words_r = 1 then begin
        os ("  " ^ mem_at r ^ " = " ^ 
                   access_a 0 ^ " * " ^ 
                   access_b 0 ^ ";")  
    end else begin
        for i = 0 to words_r - 1 do 
            os ("    " ^ mem_at (r + i) ^ " = 0;\n")
        done;
        os ("    {\n    uint64_t tmp = 0;\n");
        for i = 0 to words_r - 1 do 
            let ib = ref 0 in
            for ia = i downto 0 do 
                os ("    tmp = ((uint64_t) " ^ access_a ia ^ 
                        ") * ((uint64_t) " ^ access_b !ib ^ ");\n");
                for ic = i to words_r - 1 do
                    os ("    tmp += (uint64_t) " ^ mem_at (r+ic) ^ ";\n");
                    os ("  " ^ mem_at (r + ic) ^ 
                        " = (uint32_t) (tmp & 0xffffffff);\n");
                    os ("  tmp = tmp >> 32;\n");
                done;
                ib := !ib + 1;
            done;
        done;
        os ("    }\n")
    end

let write ?(name="") os circuit = 
    let name = if name = "" then Circuit.name circuit else name in
    let soi = string_of_int in
    
    (* schedule the simulation *)
    let regs, mems, consts, inputs, remaining = Cyclesim.find_elements circuit in
    let ready = regs @ inputs @ consts in
    let deps' s = 
        match s with 
        | Signal_mem(_, _,  _, m) -> [m.mem_read_address]
        | _ -> deps s
    in
    let schedule = Cyclesim.scheduler deps' (mems @ remaining) ready in
    
    (* the state of the simulator is represented by an array of uint32_t in c.
     * Each signal is at a certain offset within that array.  Create a map
     * of signals to the offset index *)

    let words n = (n + 31) / 32 in

    let mk_map f = 
        List.fold_left (fun (map,ofs) signal ->
            UidMap.add (uid signal) ofs map, ofs + (f signal)
        ) 
    in
    let m0 = UidMap.empty, 0 in
    (* data map *)
    let data_map, data_length = 
        (* XXX do we need mems?  what about internal_ports *)
        List.fold_left (mk_map (words << width)) m0 [ ready; mems; remaining ]
    in
    (* shadow maps for state elements *)
    let reg_map, reg_length = mk_map (words << width) m0 regs in
    let mem_size s = 
        match s with 
        | Signal_mem(_,_,_,m) -> words (m.mem_size * width s)
        | _ -> failwith "expecting memory"
    in
    let mem_map, mem_length = mk_map mem_size m0 mems in
    (* XXX muxes map? *)
    let mux_size s = List.length (deps s) - 1 in
    let muxes = List.filter 
        (function Signal_op(_, Signal_mux) -> true | _ -> false) remaining
    in
    let mux_map, mux_length = mk_map mux_size m0 muxes in

    (* write key *)
    os "/* key:\n";
    let key l = List.iter (fun s ->
            os (sprintf "[%i] %s\n"
                (UidMap.find (uid s) data_map) (to_string s))
        ) l
    in
    List.iter key [ inputs; schedule; regs; mems ];
    os "*/\n";

    (* write init function
     * NOTE: 1 extra IO port so we can 'detect' how many *)
    let num_in_ports = List.length (Circuit.inputs circuit) in
    let num_out_ports = List.length (Circuit.outputs circuit) in
    os ("#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct _port {
    char *name;
    int width;
    uint32_t *bits;
} port;

typedef struct _simulator {
    uint32_t *data, *regs, *mems, *muxs;
    port *in_ports;
    int num_in_ports;
    port *out_ports;
    int num_out_ports;
} simulator;

typedef struct _simulator_interface {
    // create simulator
    simulator *(*init)(void);
    // reset simulation registers (as appropriate)
    void (*reset)(simulator *);
    // perform simulation cycle
    void (*cycle)(simulator *);
} simulator_interface;

static simulator *init() {
    simulator *sim = malloc(sizeof(simulator));
    sim->data = calloc(sizeof(uint32_t), " ^ soi data_length ^ ");
    sim->regs = calloc(sizeof(uint32_t), " ^ soi reg_length ^ ");
    sim->mems = calloc(sizeof(uint32_t), " ^ soi mem_length ^ ");
    sim->muxs = calloc(sizeof(uint32_t), " ^ soi mux_length ^ ");
    sim->in_ports = calloc(sizeof(port), " ^ soi num_in_ports ^ ");
    sim->num_in_ports = " ^ soi num_in_ports ^ ";
    sim->out_ports = calloc(sizeof(port), " ^ soi num_out_ports ^ ");
    sim->num_out_ports = " ^ soi num_out_ports ^ ";
");

    (* set inputs and outputs *)
    let io_port io i s = 
        let prt = if io then "in_ports" else "out_ports" in
        let offset s = UidMap.find (uid s) data_map in
        os ("    sim->" ^ prt ^ "[" ^ soi i ^ "].name = \"" ^ 
            List.hd (names s) ^ "\";\n");
        os ("    sim->" ^ prt ^ "[" ^ soi i ^ "].width = " ^ 
            soi (width s) ^ ";\n");
        os ("    sim->" ^ prt ^ "[" ^ soi i ^ "].bits = sim->data + " ^ 
            soi (offset s) ^ ";\n");
    in
    os ("    // inputs\n");
    iteri (io_port true) (Circuit.inputs circuit);
    os ("    // outputs\n");
    iteri (io_port false) (Circuit.outputs circuit);

    (* initialize muxes *)
    os ("    // mux tables\n");
    let _ = List.fold_left (fun off s ->
        let deps = List.tl (deps s) in
        iteri (fun i s ->
            let d = UidMap.find (uid s) data_map in
            os (sprintf "    sim->muxs[%i] = %i;\n" (off+i) d)
        ) deps;
        off + List.length deps) 0 muxes
    in

    (* set constants *)
    os ("    // constants\n");
    let write_const s = 
        let ofs = UidMap.find (uid s) data_map in
        let x,w = Bits.Comb.ArraybitsInt32.const (const_value s) in
        Array.iteri (fun i x ->
            os ("    sim->data[" ^ soi (ofs+i) ^ "] = (uint32_t) (" ^
                Int32.to_string x ^ ");\n")
        ) x
    in
    List.iter write_const consts;
    os "    return sim;\n}\n\n";

    (* reset function *)
    os "static void reset(simulator *sim) {\n";
    let compile_reset signal =
        match signal with
        | Signal_reg(_,r) ->
            if r.reg_reset <> Signal.Comb.empty then begin
                let tgt0 = UidMap.find (uid signal) data_map in
                let tgt1 = UidMap.find (uid signal) reg_map in
                let value = UidMap.find (uid r.reg_reset_value) data_map in
                os (sprintf "    copy(sim->data+%i, sim->data+%i, %i);\n"
                    value tgt0 (width signal));
                os (sprintf "    copy(sim->data+%i, sim->regs+%i, %i);\n"
                    value tgt1 (width signal));
            end
        | _ -> failwith "expecting reg"
    in
    List.iter compile_reset regs;
    os "}\n\n";

    (* cycle function *)
    os "static void cycle(simulator *sim) {\n";

    let mask w tgt=  
        let q = w / 32 in
        if w mod 32 <> 0 then
            os (sprintf "    sim->data[%i] &= 0x%lx;\n"
                (tgt+q)
                (Int32.shift_right_logical (-1l) (32 - (w mod 32))))
    in

    (* combinatorial logic *)
    let compile signal = 
        os (sprintf "// %s\n" (to_string signal));
        let tgt = UidMap.find (uid signal) data_map in
        let doff n = UidMap.find (uid (List.nth (deps signal) n)) data_map in
    
        let data i = "sim->data[" ^ soi i ^ "]" in

        let mask() = mask (width signal) tgt in

        let addop op = 
            let w = words (width signal) in
            let d0,d1 = doff 0, doff 1 in
            os "    {\n    uint64_t carry=0;\n    uint64_t sum=0;\n";
            for i=0 to w-1 do
                os (sprintf "    sum = (uint64_t)%s %s (uint64_t)%s %s carry;\n" 
                    (data (d0+i)) op (data (d1+i)) op);
                os ("    carry = (sum >> 32) & 1;\n");
                os (sprintf "    %s = (uint32_t) sum;\n" (data (tgt+i)));
            done;
            mask();
            os "    }\n"
        in

        let eqop () = 
            let w = words (width (List.hd (deps signal))) in
            let d0,d1 = doff 0, doff 1 in
            os (sprintf "    %s = 1;\n" (data tgt));
            for i=0 to w-1 do
                os (sprintf "    %s &= %s == %s;\n" (data tgt)
                    (data (d0+i)) (data (d1+i)));
            done
        in

        let ltop () = 
            let w = words (width (List.hd (deps signal))) in
            let d0,d1 = doff 0, doff 1 in
            os (sprintf "    %s = \n" (data tgt));
            for i=w-1 downto 0 do
                os (sprintf "        %s < %s ? 1 :\n" 
                    (data (d0+i)) (data (d1+i)));
                os (sprintf "        %s > %s ? 0 :\n" 
                    (data (d0+i)) (data (d1+i)));
            done;
            os (sprintf "        0;\n")
        in

        let binop op =
            let d0,d1 = doff 0, doff 1 in
            let n = words (width signal) in
            for i=0 to n-1 do
                let s = sprintf 
                    "    %s = %s %s %s;\n"
                    (data (tgt+i)) (data (d0+i)) op (data (d1+i))
                in 
                os s
            done
        in
        let notop () = 
            let d = doff 0 in
            let n = words (width signal) in
            for i=0 to n-1 do
                let s = sprintf 
                    "    %s = ~ %s;\n"
                    (data (tgt+i)) (data (d+i))
                in
                os s
            done;
            mask ()
        in

        let copy f t n = 
            let w = words n in
            for i=0 to w-1 do
                os (sprintf "    %s = %s;\n"
                    (data (t+i)) (data (f+i)))
            done
        in

        let cpy () = copy (doff 0) tgt (width signal) in

        let rec ins s m tbit sbit = 
            let bits_left = m - tbit in
            if bits_left > 0 then
                let bits_left_t = 32 - (tbit mod 32) in
                let bits_left_s = 32 - (sbit mod 32) in
                let bits = min bits_left_t bits_left_s in
                let bits = min bits_left bits in
                let mask = 
                    sprintf "0x%lx" 
                        (Int32.shift_right_logical (-1l) (32-bits))
                in
                os (sprintf "    %s %s= (((%s >> %i) & %s) << %i);\n"
                    (data (tgt+(tbit/32)))
                    (if tbit mod 32 = 0 then "" else "|")
                    (data (s + (sbit/32)))
                    (sbit mod 32)
                    mask
                    (tbit mod 32));
                ins s m (tbit+bits) (sbit+bits)
            else
                ()
        in
        
        let sel hi lo =
            let w = width signal in
            let s = doff 0 in
            if lo mod 32 = 0 then
                let loword = lo / 32 in
                copy (s + loword) tgt w;
                mask ()
            else
                ins s w 0 lo
        in

        let cat() = 
            let d = List.rev (deps signal) in
            assert (List.length d > 1);
            let insert o s = 
                let w = width s in
                let s = UidMap.find (uid s) data_map in
                ins s (o+w) o 0;
                o+w
            in
            let _ = List.fold_left insert 0 d in
            ()
        in

        let reg r = 
            let f s = 
                if s = empty then None
                else Some(UidMap.find (uid s) data_map)
            in
            let data'' s i = data (i+(match s with Some(x) -> x | None -> 0)) in
            let data' s = data'' s 0 in
            let reg i = "sim->regs[" ^ soi i ^ "]" in
            let words = words (width signal) in
            let clr = f r.reg_clear in
            let clr_val = f r.reg_clear_value in
            let clr_lev = f r.reg_clear_level in
            let ena = f r.reg_enable in
            let d = UidMap.find (uid (List.hd (deps signal))) data_map in
            let q = UidMap.find (uid signal) reg_map in
            let do_clear() = 
                for i=0 to words-1 do
                    os (sprintf "        %s = %s;\n" 
                        (reg (q+i)) (data'' clr_val i))
                done
            in
            let do_update() = 
                for i=0 to words-1 do
                    os (sprintf "        %s = %s;\n" (reg (q+i)) (data (d+i)))
                done
            in
            match clr,ena with
            | None,None -> 
                 do_update()
            | None,Some(_) ->
                os (sprintf "    if (%s) {\n" (data' ena));
                do_update();
                os (sprintf "    }\n")
            | Some(_),None ->
                os (sprintf "    if (%s == %s) {\n" (data' clr) (data' clr_lev));
                do_clear();
                os (sprintf "    } else {\n");
                do_update();
                os (sprintf "    }\n")
            | Some(_),Some(_) ->
                os (sprintf "    if (%s == %s) {\n" (data' clr) (data' clr_lev));
                do_clear();
                os (sprintf "    } else if (%s) {\n" (data' ena));
                do_update();
                os (sprintf "    }\n")
        in

        let mem m =
            let words = words (width signal) in
            let mem = UidMap.find (uid signal) mem_map in
            let addr = UidMap.find (uid m.mem_read_address) data_map in
            for i=0 to words-1 do
                os (sprintf 
                    "    sim->mems[%i+(sim->data[%i]*%i)+%i] = sim->data+%i);\n"
                    mem addr words i (tgt+i))
            done
        in

        let mux () = 
            let d = deps signal in
            let sel,d = List.hd d, List.tl d in
            let len = List.length d in
            let sel = UidMap.find (uid sel) data_map in
            let mux = UidMap.find (uid signal) mux_map in
            os ("    {\n");
            os (sprintf "    uint32_t *base = sim->muxs+%i;\n" mux);
            os (sprintf "    uint32_t addr = sim->data[%i];\n" sel);
            os (sprintf "    addr = addr > %i ? %i : addr;\n" 
                (len-1) (len-1));
                os (sprintf "    addr = base[addr];\n");
            for i=0 to words (width signal) - 1 do
                os (sprintf "    sim->data[%i] = sim->data[addr+%i];\n"
                    (tgt+i) i)
            done;
            os "    }\n"
        in

        let mul signed = 
            let d0 = List.nth (deps signal) 0 in
            let d1 = List.nth (deps signal) 1 in
            let o0,o1 = doff 0, doff 1 in
            array_mul os signed 
                (words (width signal)) (words (width d0)) (words (width d1))
                (width signal) (width d0) (width d1)
                tgt o0 o1;
            mask();
        in

        match signal with
        | Signal_empty -> failwith "cant compile empty signal"
        | Signal_const(_) -> failwith "cant compile const - already done!"
        | Signal_op(_,op) ->
        begin
            match op with
            | Signal_add -> addop "+"
            | Signal_sub -> addop "-"
            | Signal_mulu -> mul false
            | Signal_muls -> mul true
            | Signal_and -> binop "&"
            | Signal_or -> binop "|"
            | Signal_xor -> binop "^"
            | Signal_eq -> eqop ()
            | Signal_not -> notop()
            | Signal_lt -> ltop()
            | Signal_cat -> cat()
            | Signal_mux -> mux()

        end
        | Signal_wire(_,d) -> cpy ()
        | Signal_select(_,h,l) -> sel h l
        | Signal_reg(_,r) -> reg r
        | Signal_mem(_,_,_,m) -> mem m
        | Signal_inst(_) -> failwith 
            "Instantiations are not supported in simulation"
    in

    let compile_reg_update signal = 
        match signal with
        | Signal_reg(_,_) ->
            let tgt = UidMap.find (uid signal) data_map in
            let src = UidMap.find (uid signal) reg_map in
            for i=0 to words(width signal)-1 do
                os (sprintf "    sim->data[%i] = sim->regs[%i];\n" 
                    (tgt+i) (src+i))
            done
        | _ -> failwith "expecting register"
    in

    let compile_mem_update signal = 
        match signal with
        | Signal_mem(_,_,r,m) ->
            let words = words (width signal) in
            let mem = UidMap.find (uid signal) mem_map in
            let we = UidMap.find (uid r.reg_enable) data_map in
            let w = UidMap.find (uid m.mem_write_address) data_map in
            let d = UidMap.find (uid (List.hd (deps signal))) data_map in
            os (sprintf "    if (sim->data[%i]) {\n" we);
            for i=0 to words-1 do
                os (sprintf "        sim->data[%i] = sim->mems[%i+(%i*%i)+%i];\n"
                    (d+i) mem w words i)
            done;
            os (sprintf "    }\n")
        | _ -> failwith "error while compiling mem update"
    in

    let mask_inputs s = mask (width s) (UidMap.find (uid s) data_map) in

    List.iter mask_inputs (Circuit.inputs circuit);
    List.iter compile schedule;
    List.iter compile regs;
    List.iter compile_mem_update mems;
    List.iter compile_reg_update regs;
    os "}\n\n";

    (* generate interface structure *)
    os ("simulator_interface " ^ name ^ " = {\n");
    os ("  init, reset, cycle\n");
    os ("};\n\n");
    os (c name);
    os (ocaml name)

    
(*****************************************************************)

