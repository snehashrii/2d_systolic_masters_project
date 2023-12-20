package mac_row;
import mac::*;
import Vector :: * ;
import FIFO ::*;
import FloatingPoint::*;
import fpu_common::*;

interface Ifc_mac_row;
method Action take_input(Vector#(4, Reg#(FloatingPoint#(11,52))) weight1, Vector#(4, Reg#(FloatingPoint#(11,52))) psum_in1);
method Bit#(64) give(int index);
method Action load_input(  Vector#(4, FloatingPoint#(11,52)) input1, Bool load, Bool clear);
method Bool valid_signal();
endinterface


module mkmac_row(Ifc_mac_row);
Ifc_mac mac1 <- mkmac;
Ifc_mac mac2 <- mkmac;
Ifc_mac mac3 <- mkmac;
Ifc_mac mac4 <- mkmac;

Reg#(FloatingPoint#(11,52)) rg_w11 <- mkReg(0);
Reg#(FloatingPoint#(11,52)) rg_w12 <- mkReg(0);
Reg#(FloatingPoint#(11,52)) rg_w13 <- mkReg(0);
Reg#(FloatingPoint#(11,52)) rg_w14 <- mkReg(0);

Reg#(FloatingPoint#(11,52)) rg_a11 <- mkReg(0);
Reg#(FloatingPoint#(11,52)) rg_a12 <- mkReg(0);
Reg#(FloatingPoint#(11,52)) rg_a13 <- mkReg(0);
Reg#(FloatingPoint#(11,52)) rg_a14 <- mkReg(0);

Reg#(FloatingPoint#(11,52)) rg_psum_in11 <- mkReg(0);
Reg#(FloatingPoint#(11,52)) rg_psum_in12 <- mkReg(0);
Reg#(FloatingPoint#(11,52)) rg_psum_in13 <- mkReg(0);
Reg#(FloatingPoint#(11,52)) rg_psum_in14 <- mkReg(0);
Reg#(UInt#(8))  n      <- mkReg(0);

Vector#(4, Reg#(Bit#(64))) psum_out <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) weight  <- replicateM( mkReg( 0 ) );// Initialize an 8-element array with specific values
Vector#(4, Reg#(FloatingPoint#(11,52))) _input<- replicateM( mkReg( 0 ) );


FIFO#(FloatingPoint#(11,52))  inputFifo <-  mkSizedFIFO(4);
FIFO#(Bit#(64)) output_fifo <- mkSizedFIFO(1);

Reg#(Bit#(1)) rg_inputs_rx <- mkReg(0);
Reg#(Bit#(1)) rg_inputs_tx <- mkReg(0);
Reg#(Bool) rg_input_load <- mkReg(False);
Reg#(int) rg_cycle <- mkReg(0);
Reg#(Bool) rg_ready <- mkReg(False);
Reg#(int) rg_cycle_psum <- mkReg(0);
Reg#(Bool) rg_fifo_clear<-mkReg(False);
Reg#(Bool) rg_start_input <-mkReg(False);

// Pipeline fifo
rule rl_loop (rg_input_load==True );
    FloatingPoint#(11,52) f=0;
    if (n==4) begin
    inputFifo.enq(f);
    if (rg_fifo_clear) begin
        n<=0;
     end
     end
    else begin
        inputFifo.enq(_input[n]);
        n<=n+1;
        end
endrule
 
    
// Preloading weights
rule rl_weight_stationary;
     rg_w11<=weight[0];
     rg_w12<=weight[1];
     rg_w13<=weight[2];
     rg_w14<=weight[3];

endrule
   
 // input flow from left to right
rule rl_input_flow (rg_inputs_rx==1 );
     rg_inputs_tx<=1;
     rg_a11 <= inputFifo.first;
     rg_a12<=rg_a11;
     rg_a13<=rg_a12;
     rg_a14<=rg_a13;
     inputFifo.deq;
endrule

// Psum flow from top to bottom
rule rl_psum_display  ( rg_inputs_rx==1);
    let p <-mac1.mav_psumout(rg_w11,rg_a11,rg_psum_in11);
    let q <-mac2.mav_psumout(rg_w12,rg_a12,rg_psum_in12);
    let r <-mac3.mav_psumout(rg_w13,rg_a13,rg_psum_in13);
    let s <-mac4.mav_psumout(rg_w14,rg_a14,rg_psum_in14);
    psum_out[0]<= pack(p);
    psum_out[1]<= pack(q);
    psum_out[2]<= pack(r);
    psum_out[3] <= pack(s);
endrule

// Timing each row
rule rl_valid_signal (rg_inputs_tx==1);
     if (rg_cycle>12) begin
        rg_ready<=True;
     else begin
       rg_cycle<=rg_cycle+1;
     end
endrule
     
method Action ma_take_input(Vector#(4, Reg#(FloatingPoint#(11,52))) weight1, Vector#(4, Reg#(FloatingPoint#(11,52))) psum_in1);
      rg_inputs_rx<=1;
      for( int i=0;i<4;i=i+1) begin
       weight[i]<=weight1[i];
       
       end
      rg_psum_in11 <= psum_in1[0];
      rg_psum_in12 <= psum_in1[1];
      rg_psum_in13 <= psum_in1[2];
      rg_psum_in14 <= psum_in1[3];
endmethod
  
method Action ma_load_input(Vector#(4, FloatingPoint#(11,52)) input1, Bool load,Bool clear);
   rg_input_load<=load;
   writeVReg(_input, input1);
       rg_fifo_clear<=clear;
endmethod

method  Bit#(64) mv_give(int index) ;
      return psum_out[index];
endmethod

method Bool mv_valid_signal();
     return rg_ready;
endmethod

endmodule: mkmac_row
endpackage:mac_row
