// http://www.eg.bucknell.edu/~cs320/1995-fall/verilog-manual.html#RTFToC33

// Digital model of a traffic light
// By Dan Hyde August 10, 1995
module traffic;
parameter on = 1, off = 0, red_tics = 35, 
          amber_tics = 3, green_tics = 20;
reg clock, red, amber, green;

// will stop the simulation after 1000 time units
initial begin: stop_at
   #1000; $stop;    
end

// initialize the lights and set up monitoring of registers
initial begin: Init
    red = off; amber = off; green = off;
    $display("                 Time green amber red");  
    $monitor("%3d    %b     %b    %b", $time, green, amber, red);
end

// task to wait for 'tics' positive edge clocks
// before turning light off
task light;
   output color;
   input [31:0] tics;
   begin
      repeat(tics)  // wait to detect tics positive edges on clock
         @(posedge clock);
      color = off;
   end
endtask

// waveform for clock period of 2 time units
always begin: clock_wave
  #1 clock = 0;
  #1 clock = 1;
end

always begin: main_process
   red = on;
   light(red, red_tics);  // call task to wait 
   green = on;
   light(green, green_tics);
   amber = on;
   light(amber, amber_tics);
end

endmodule
