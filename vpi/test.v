`timescale 1ns / 1ns

module hello_world ();
  
  reg clock;
  reg a;
  reg [1:0] b;
  wire [2:0] c;
  reg [2:0] d;

  initial begin
    $dumpfile("dump.vcd");
    $dumpvars(0, hello_world);
    $hardcaml_cosim(clock, a, b, c);
    //$from_task(a, b);
    //$to_task(c);
    #100 $finish;
  end

  assign c = b + a;

  integer i=0;
  initial begin
    while (i<10) begin
      #10;
      i=i+1;
    end
  end

  always @(posedge clock) begin
    d <= c;
  end

endmodule

