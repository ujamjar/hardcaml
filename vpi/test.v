module hello_world ();
  
  reg a;
  reg [1:0] b;
  wire [2:0] c;

  initial begin
    $from_task(a, b);
    $to_task(c);
    #10 $finish;
  end

  initial begin
    while (1) begin
      #1;
      $write("hello\n");
    end
  end

endmodule

