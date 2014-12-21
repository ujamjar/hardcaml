module hello_world ();
  
  reg a;
  reg [1:0] b;
  wire [2:0] c;

  initial begin
    $from_task(a, b, c);
  end

endmodule

