// Taken from http://www.europa.com/~celiac/fsm_samp.html

// These are the symbolic names for states
parameter  [1:0]       //synopsys enum state_info
  S0 = 2'h0,
  S1 = 2'h1,
  S2 = 2'h2,
  S3 = 2'h3;

// These are the current state and next state variables
reg [1:0] /* synopsys enum state_info */ state;
reg [1:0] /* synopsys enum state_info */ next_state;
 
 
// synopsys state_vector state

always @ (state or y or x)
begin 
  next_state = state;
  case (state)                 // synopsys full_case parallel_case
    S0: begin 
      if (x) begin 
        next_state = S1;
      end
      else begin 

        next_state = S2;
      end
    end
    S1: begin 
      if (y) begin 
        next_state = S2;
      end
      else begin 
        next_state = S0;
      end
    end
    S2: begin 
      if (x & y) begin 
        next_state = S3;
      end
      else begin 
        next_state = S0;
      end
    end
    S3: begin 
      next_state = S0;
    end
  endcase
end


always @ (posedge clk or posedge reset)
begin 
  if (reset) begin 
    state <= S0;
  end
  else begin 
    state <= next_state;
  end
end
