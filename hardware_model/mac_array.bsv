package mac_array;
import mac_row::*;
import Vector :: * ;
import fpu_common::*;

import FIFO::*;
import FloatingPoint::*;
import FIFOF::*;

`define a1 51
`define a2 52
`define a3 62
`define a4 63


interface Ifc_conv;
method Action ma_top_cnn_input(Reg#(int) _new_input, Bool load, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight1, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight2, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight3, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight4, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight5,  Vector#(4, Reg#(FloatingPoint#(11,52))) _weight6, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight7, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight8, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight9, Vector#(4, Reg#(FloatingPoint#(11, 52))) in1, Vector#(4, Reg#(FloatingPoint#(11, 52))) in2, Vector#(4, Reg#(FloatingPoint#(11, 52))) in3, Vector#(4, Reg#(FloatingPoint#(11, 52))) in4, Vector#(4, Reg#(FloatingPoint#(11, 52))) in5, Vector#(4, Reg#(FloatingPoint#(11, 52))) in6, Vector#(4, Reg#(FloatingPoint#(11, 52))) in7, Vector#(4, Reg#(FloatingPoint#(11, 52))) in8, Vector#(4, Reg#(FloatingPoint#(11, 52))) in9);
endinterface

module mkmac_array(Ifc_conv);
     Ifc_mac_row mac_r1 <- mkmac_row();
     Ifc_mac_row mac_r2 <- mkmac_row();
     Ifc_mac_row mac_r3 <- mkmac_row();
     Ifc_mac_row mac_r4 <- mkmac_row();
     Ifc_mac_row mac_r5 <- mkmac_row();
     Ifc_mac_row mac_r6 <- mkmac_row();
     Ifc_mac_row mac_r7 <- mkmac_row();
     Ifc_mac_row mac_r8 <- mkmac_row();
     Ifc_mac_row mac_r9 <- mkmac_row();
// Weight inputs
Vector#(4, Reg#(FloatingPoint#(11,52))) weight  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) weight2  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) weight3  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) weight4  <- replicateM( mkReg( 0 ) );

Vector#(4, Reg#(FloatingPoint#(11,52))) weight5  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) weight6  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) weight7  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) weight8  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) weight9  <- replicateM( mkReg( 0 ) );

// activation inputs
Vector#(4, Reg#(FloatingPoint#(11,52))) _input <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input2 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input3 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input4 <- replicateM( mkReg( 0 ) );

Vector#(4, Reg#(FloatingPoint#(11,52))) _input5 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input6 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input7 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input8 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input9 <- replicateM( mkReg( 0 ) );

// ROW I FIFOs
FIFO#(Vector#(4, FloatingPoint#(11, 52))) fifo_input <- mkSizedFIFO(500);
FIFO#(Vector#(4, FloatingPoint#(11, 52))) fifo_input2 <- mkSizedFIFO(500);
FIFO#(Vector#(4, FloatingPoint#(11, 52))) fifo_input3 <- mkSizedFIFO(500);
FIFO#(Vector#(4, FloatingPoint#(11, 52))) fifo_input4 <- mkSizedFIFO(500);

FIFO#(Vector#(4, FloatingPoint#(11, 52))) fifo_input5 <- mkSizedFIFO(500);
FIFO#(Vector#(4, FloatingPoint#(11, 52))) fifo_input6 <- mkSizedFIFO(500);
FIFO#(Vector#(4, FloatingPoint#(11, 52))) fifo_input7 <- mkSizedFIFO(500);
FIFO#(Vector#(4, FloatingPoint#(11, 52))) fifo_input8 <- mkSizedFIFO(500);
FIFO#(Vector#(4, FloatingPoint#(11, 52))) fifo_input9 <- mkSizedFIFO(500);

Vector#(4, Reg#(FloatingPoint#(11,52))) psum_in <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(Bit#(64))) psum_out <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(Bit#(64))) psum_out1 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(Bit#(64))) psum_out2 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(Bit#(64))) psum_out3 <- replicateM( mkReg( 0 ) );

Vector#(4, Reg#(Bit#(64))) psum_out4 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(Bit#(64))) psum_out5 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(Bit#(64))) psum_out6 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(Bit#(64))) psum_out7 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(Bit#(64))) psum_out8 <- replicateM( mkReg( 0 ) );

// Converted registers from Bit#(64) psum to FPU
Vector#(4, Reg#(FloatingPoint#(11,52))) conv_psum  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) conv_psum1  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) conv_psum2  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) conv_psum3  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) conv_psum4  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) conv_psum5  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) conv_psum6  <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) conv_psum7  <- replicateM( mkReg( 0 ) );

// Pipeline registers
Reg#(int) rg_new_input <-mkReg(0);
Reg#(int) rg_next_stage<-mkReg(0);
Reg#(int) rg_previous_new_input<-mkReg(0);
Reg#(Bool) rg_input_load <- mkReg(False);
Reg#(Bit#(1)) rg_psum_received <- mkReg(0); 
Reg#(Bool) rg_multi_fifo_loaded<-mkReg(False);

FIFOF#(Bit#(64)) psum_fifo <- mkSizedFIFOF(4);
Vector#(2000, Reg#(Bit#(64))) final_output<-replicateM(mkReg(0)) ;


rule rl_array_weight;
     mac_r1.ma_take_input(weight,psum_in);
endrule
     
rule rl_row1_input ((rg_next_stage==1||rg_previous_new_input==1)&&(rg_multi_fifo_loaded==True));
     mac_r1.ma_load_input(fifo_input.first, rg_input_load,(rg_next_stage==1));
     fifo_input.deq();
endrule

rule rl_getoutput;
     
     psum_out[0] <= mac_r1.mv_give(0);
     psum_out[1] <= mac_r1.mv_give(1);
     psum_out[2] <= mac_r1.mv_give(2);
     psum_out[3] <= mac_r1.mv_give(3);
     conv_psum[0] <= FloatingPoint{sign : unpack(mac_r1.mv_give(0)[`a4]), exp: mac_r1.mv_give(0)[`a3:`a2], sfd: mac_r1.mv_give(0)[`a1:0]};
     conv_psum[1] <= FloatingPoint{sign : unpack(mac_r1.mv_give(1)[`a4]), exp: mac_r1.mv_give(1)[`a3:`a2], sfd: mac_r1.mv_give(1)[`a1:0]};
     conv_psum[2] <= FloatingPoint{sign : unpack(mac_r1.mv_give(2)[`a4]), exp: mac_r1.mv_give(2)[`a3:`a2], sfd: mac_r1.mv_give(2)[`a1:0]};
     conv_psum[3] <= FloatingPoint{sign : unpack(mac_r1.mv_give(3)[`a4]), exp: mac_r1.mv_give(3)[`a3:`a2], sfd: mac_r1.mv_give(3)[`a1:0]};
endrule

(* fire_when_enabled *)
rule rl_array_weight_round2 (mac_r1.mv_valid_signal());
      mac_r2.ma_take_input(weight2,conv_psum);
endrule

rule rl_row2_input ((rg_next_stage==1||rg_previous_new_input==1)&&(rg_multi_fifo_loaded==True));
      mac_r2.ma_load_input(fifo_input2.first, rg_input_load,(rg_next_stage==1));
      fifo_input2.deq();
endrule

rule rl_getoutput2;

     psum_out1[0] <= mac_r2.mv_give(0);
     psum_out1[1] <= mac_r2.mv_give(1);
     psum_out1[2] <= mac_r2.mv_give(2);
     psum_out1[3] <= mac_r2.mv_give(3);
     conv_psum1[0] <= FloatingPoint{sign : unpack(mac_r2.mv_give(0)[`a4]), exp: mac_r2.mv_give(0)[`a3:`a2], sfd: mac_r2.mv_give(0)[`a1:0]};
     conv_psum1[1] <= FloatingPoint{sign : unpack(mac_r2.mv_give(1)[`a4]), exp: mac_r2.mv_give(0)[`a3:`a2], sfd: mac_r2.mv_give(1)[`a1:0]};
     conv_psum1[2] <= FloatingPoint{sign : unpack(mac_r2.mv_give(2)[`a4]), exp: mac_r2.mv_give(0)[`a3:`a2], sfd: mac_r2.mv_give(2)[`a1:0]};
     conv_psum1[3] <= FloatingPoint{sign : unpack(mac_r2.mv_give(3)[`a4]), exp: mac_r2.mv_give(0)[`a3:`a2], sfd: mac_r2.mv_give(3)[`a1:0]};
endrule

(*fire_when_enabled*)
rule rl_row3_weight (mac_r2.mv_valid_signal());
     mac_r3.ma_take_input(weight3,conv_psum1);
endrule
     
rule rl_row3_input ((rg_next_stage==1||rg_previous_new_input==1)&&(rg_multi_fifo_loaded==True));
        mac_r3.ma_load_input(fifo_input3.first, rg_input_load,(rg_next_stage==1));
        fifo_input3.deq();
endrule

rule rl_get_output3;
     psum_out2[0] <= mac_r3.mv_give(0);
     psum_out2[1] <= mac_r3.mv_give(1);
     psum_out2[2] <= mac_r3.mv_give(2);
     psum_out2[3] <= mac_r3.mv_give(3);
     
     conv_psum2[0] <= FloatingPoint{sign : unpack(mac_r3.mv_give(0)[`a4]), exp: mac_r3.mv_give(0)[`a3:`a2], sfd: mac_r3.mv_give(0)[`a1:0]};
     conv_psum2[1] <= FloatingPoint{sign : unpack(mac_r3.mv_give(1)[`a4]), exp: mac_r3.mv_give(1)[`a3:`a2], sfd: mac_r3.mv_give(1)[`a1:0]};
     conv_psum2[2] <= FloatingPoint{sign : unpack(mac_r3.mv_give(2)[`a4]), exp: mac_r3.mv_give(2)[`a3:`a2], sfd: mac_r3.mv_give(2)[`a1:0]};
     conv_psum2[3] <= FloatingPoint{sign : unpack(mac_r3.mv_give(3)[`a4]), exp: mac_r3.mv_give(3)[`a3:`a2], sfd: mac_r3.mv_give(3)[`a1:0]};
endrule

(*fire_when_enabled*)
rule rl_row4_weight (mac_r3.mv_valid_signal());
     mac_r4.ma_take_input(weight4,conv_psum2);
endrule
     
rule rl_row4_input ((rg_next_stage==1||rg_previous_new_input==1)&&(rg_multi_fifo_loaded==True));
        mac_r4.ma_load_input(fifo_input4.first, rg_input_load,(rg_next_stage==1));
        fifo_input4.deq();
endrule

rule rl_get_output4;
     psum_out3[0] <= mac_r4.mv_give(0);
     psum_out3[1] <= mac_r4.mv_give(1);
     psum_out3[2] <= mac_r4.mv_give(2);
     psum_out3[3] <= mac_r4.mv_give(3);

     conv_psum3[0] <= FloatingPoint{sign : unpack(mac_r4.mv_give(0)[`a4]), exp: mac_r4.mv_give(0)[`a3:`a2], sfd: mac_r4.mv_give(0)[`a1:0]};
     conv_psum3[1] <= FloatingPoint{sign : unpack(mac_r4.mv_give(1)[`a4]), exp: mac_r4.mv_give(1)[`a3:`a2], sfd: mac_r4.mv_give(1)[`a1:0]};
     conv_psum3[2] <= FloatingPoint{sign : unpack(mac_r4.mv_give(2)[`a4]), exp: mac_r4.mv_give(2)[`a3:`a2], sfd: mac_r4.mv_give(2)[`a1:0]};
     conv_psum3[3] <= FloatingPoint{sign : unpack(mac_r4.mv_give(3)[`a4]), exp: mac_r4.mv_give(3)[`a3:`a2], sfd: mac_r4.mv_give(3)[`a1:0]};
endrule

rule rl_row5_weight (mac_r4.mv_valid_signal());
     mac_r5.ma_take_input(weight5,conv_psum3);
endrule
     
rule rl_row5_input ((rg_next_stage==1||rg_previous_new_input==1)&&(rg_multi_fifo_loaded==True));
        mac_r5.ma_load_input(fifo_input5.first, rg_input_load,(rg_next_stage==1));
        fifo_input5.deq();
endrule

rule rl_get_output5;
     psum_out4[0] <= mac_r5.mv_give(0);
     psum_out4[1] <= mac_r5.mv_give(1);
     psum_out4[2] <= mac_r5.mv_give(2);
     psum_out4[3] <= mac_r5.mv_give(3);


     conv_psum4[0] <= FloatingPoint{sign : unpack(mac_r5.mv_give(0)[`a4]), exp: mac_r5.mv_give(0)[`a3:`a2], sfd: mac_r5.mv_give(0)[`a1:0]};
     conv_psum4[1] <= FloatingPoint{sign : unpack(mac_r5.mv_give(1)[`a4]), exp: mac_r5.mv_give(1)[`a3:`a2], sfd: mac_r5.mv_give(1)[`a1:0]};
     conv_psum4[2] <= FloatingPoint{sign : unpack(mac_r5.mv_give(2)[`a4]), exp: mac_r5.mv_give(2)[`a3:`a2], sfd: mac_r5.mv_give(2)[`a1:0]};
     conv_psum4[3] <= FloatingPoint{sign : unpack(mac_r5.mv_give(3)[`a4]), exp: mac_r5.mv_give(3)[`a3:`a2], sfd: mac_r5.mv_give(3)[`a1:0]};
endrule

rule rl_row6_weight (mac_r5.mv_valid_signal());
     mac_r6.ma_take_input(weight6,conv_psum4);
endrule
     
rule rl_row6_input ((rg_next_stage==1||rg_previous_new_input==1)&&(rg_multi_fifo_loaded==True));
        mac_r6.ma_load_input(fifo_input6.first, rg_input_load,(rg_next_stage==1));
        fifo_input6.deq();
endrule

rule rl_get_output6;
     psum_out5[0] <= mac_r6.mv_give(0);
     psum_out5[1] <= mac_r6.mv_give(1);
     psum_out5[2] <= mac_r6.mv_give(2);
     psum_out5[3] <= mac_r6.mv_give(3);


     conv_psum5[0] <= FloatingPoint{sign : unpack(mac_r6.mv_give(0)[`a4]), exp: mac_r6.mv_give(0)[`a3:`a2], sfd: mac_r6.mv_give(0)[`a1:0]};
     conv_psum5[1] <= FloatingPoint{sign : unpack(mac_r6.mv_give(1)[`a4]), exp: mac_r6.mv_give(1)[`a3:`a2], sfd: mac_r6.mv_give(1)[`a1:0]};
     conv_psum5[2] <= FloatingPoint{sign : unpack(mac_r6.mv_give(2)[`a4]), exp: mac_r6.mv_give(2)[`a3:`a2], sfd: mac_r6.mv_give(2)[`a1:0]};
     conv_psum5[3] <= FloatingPoint{sign : unpack(mac_r6.mv_give(3)[`a4]), exp: mac_r6.mv_give(3)[`a3:`a2], sfd: mac_r6.mv_give(3)[`a1:0]};
endrule

rule row7_weight (mac_r6.mv_valid_signal());
     mac_r7.ma_take_input(weight7,conv_psum5);
endrule
     
rule row7_input ((rg_next_stage==1||rg_previous_new_input==1)&&(rg_multi_fifo_loaded==True));
        mac_r7.ma_load_input(fifo_input7.first, rg_input_load,(rg_next_stage==1));
        fifo_input7.deq();
endrule

rule rl_get_output7;
     psum_out6[0] <= mac_r7.mv_give(0);
     psum_out6[1] <= mac_r7.mv_give(1);
     psum_out6[2] <= mac_r7.mv_give(2);
     psum_out6[3] <= mac_r7.mv_give(3);


     conv_psum6[0] <= FloatingPoint{sign : unpack(mac_r7.mv_give(0)[`a4]), exp: mac_r7.mv_give(0)[`a3:`a2], sfd: mac_r7.mv_give(0)[`a1:0]};
     conv_psum6[1] <= FloatingPoint{sign : unpack(mac_r7.mv_give(1)[`a4]), exp: mac_r7.mv_give(1)[`a3:`a2], sfd: mac_r7.mv_give(1)[`a1:0]};
     conv_psum6[2] <= FloatingPoint{sign : unpack(mac_r7.mv_give(2)[`a4]), exp: mac_r7.mv_give(2)[`a3:`a2], sfd: mac_r7.mv_give(2)[`a1:0]};
     conv_psum6[3] <= FloatingPoint{sign : unpack(mac_r7.mv_give(3)[`a4]), exp: mac_r7.mv_give(3)[`a3:`a2], sfd: mac_r7.mv_give(3)[`a1:0]};
endrule

rule row8_weight (mac_r7.mv_valid_signal());
     mac_r8.ma_take_input(weight8,conv_psum6);
endrule
     
rule row8_input ((rg_next_stage==1||rg_previous_new_input==1)&&(rg_multi_fifo_loaded==True));
        mac_r8.ma_load_input(fifo_input8.first, rg_input_load,(rg_next_stage==1));
        fifo_input8.deq();
endrule

rule rl_get_output8;
     psum_out7[0] <= mac_r8.mv_give(0);
     psum_out7[1] <= mac_r8.mv_give(1);
     psum_out7[2] <= mac_r8.mv_give(2);
     psum_out7[3] <= mac_r8.mv_give(3);


     conv_psum7[0] <= FloatingPoint{sign : unpack(mac_r8.mv_give(0)[`a4]), exp: mac_r8.mv_give(0)[`a3:`a2], sfd: mac_r8.mv_give(0)[`a1:0]};
     conv_psum7[1] <= FloatingPoint{sign : unpack(mac_r8.mv_give(0)[`a4]), exp: mac_r8.mv_give(0)[`a3:`a2], sfd: mac_r8.mv_give(0)[`a1:0]};
     conv_psum7[2] <= FloatingPoint{sign : unpack(mac_r8.mv_give(2)[`a4]), exp: mac_r8.mv_give(2)[`a3:`a2], sfd: mac_r8.mv_give(2)[`a1:0]};
     conv_psum7[3] <= FloatingPoint{sign : unpack(mac_r8.mv_give(3)[`a4]), exp: mac_r8.mv_give(3)[`a3:`a2], sfd: mac_r8.mv_give(3)[`a1:0]};
endrule

rule row9_weight (mac_r8.mv_valid_signal());
     mac_r9.ma_take_input(weight9,conv_psum7);
endrule
     
rule row9_input ((rg_next_stage==1||rg_previous_new_input==1)&&(rg_multi_fifo_loaded==True));
        mac_r9.ma_load_input(fifo_input9.first, rg_input_load,(rg_next_stage==1));
        fifo_input9.deq();
endrule

rule rl_get_output9   ;
     psum_out8[0] <= mac_r9.mv_give(0);
     psum_out8[1] <= mac_r9.mv_give(1);
     psum_out8[2] <= mac_r9.mv_give(2);
     psum_out8[3] <= mac_r9.mv_give(3);
     if (psum_out8[0]!=0) begin
               psum_fifo.enq(psum_out8[0]);
               $display(" %0h ", psum_out8[0]);
               end
endrule

rule rl_pipeline_array;
            Vector::Vector#(4, FloatingPoint#(11, 52)) i1, i2,i3,i4,i5,i6,i7,i8,i9;
            for (int r=0;r<4;r=r+1) begin
             i1[r]=_input[r];
             i2[r]=_input2[r];
             i3[r]=_input3[r];
             i4[r]=_input4[r];
             i5[r]=_input5[r];
             i6[r]=_input6[r];
             i7[r]=_input7[r];
             i8[r]=_input8[r];
             i9[r]=_input9[r];
             end
            if (rg_new_input>rg_previous_new_input || rg_previous_new_input==1) begin
               fifo_input.enq(i1);
               fifo_input2.enq(i2);
               fifo_input3.enq(i3);
               fifo_input4.enq(i4);
               fifo_input5.enq(i5);
               fifo_input6.enq(i6);
               fifo_input7.enq(i7);
               fifo_input8.enq(i8);
               fifo_input9.enq(i9);
               rg_previous_new_input<=rg_new_input;
               rg_multi_fifo_loaded<=True;
               end
               if(!psum_fifo.notFull) begin
                  rg_next_stage<=1;
                  end
                  else
                  rg_next_stage<=0;
               
endrule
      
method Action ma_top_cnn_input(Reg#(int) _new_input, Bool load, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight1, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight2, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight3, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight4, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight5,  Vector#(4, Reg#(FloatingPoint#(11,52))) _weight6, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight7, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight8, Vector#(4, Reg#(FloatingPoint#(11,52))) _weight9, Vector#(4, Reg#(FloatingPoint#(11, 52))) in1, Vector#(4, Reg#(FloatingPoint#(11, 52))) in2, Vector#(4, Reg#(FloatingPoint#(11, 52))) in3, Vector#(4, Reg#(FloatingPoint#(11, 52))) in4, Vector#(4, Reg#(FloatingPoint#(11, 52))) in5, Vector#(4, Reg#(FloatingPoint#(11, 52))) in6, Vector#(4, Reg#(FloatingPoint#(11, 52))) in7, Vector#(4, Reg#(FloatingPoint#(11, 52))) in8, Vector#(4, Reg#(FloatingPoint#(11, 52))) in9);
      rg_input_load<= load;
      rg_new_input<=_new_input;
        for (int m=0;m<4;m=m+1) begin
         weight[m]<=_weight1[m];
         weight2[m]<=_weight2[m];
         weight3[m]<=_weight3[m];
         weight4[m]<=_weight4[m];
         weight5[m]<=_weight5[m];
         weight6[m]<=_weight6[m];
         weight7[m]<=_weight7[m];
         weight8[m]<=_weight8[m];
         weight9[m]<=_weight9[m];
         _input[m]<=in1[m];
         _input2[m]<=in2[m];
         _input3[m]<=in3[m];
         _input4[m]<=in4[m];
         _input5[m]<=in5[m];
         _input6[m]<=in6[m];
         _input7[m]<=in7[m];
         _input8[m]<=in8[m];
         _input9[m]<=in9[m];
         end
   endmethod

endmodule: mkmac_array

endpackage:mac_array
