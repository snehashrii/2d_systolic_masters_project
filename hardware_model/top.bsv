import Vector ::*;
import mac_array ::*;
import pooling ::*;
import im2col_gemm ::*;
import BRAM::*;
import StmtFSM::*;
import Clocks::*;
import FIFOF::*;
import FIFO::*;
import FloatingPoint::*;
import fpu_common::*;
import padding::*;
import RegFile::*;
import fpu_convert_pipelined::*;

`define a1 51
  `define a2 52
  `define a3 62
  `define a4 63


function BRAMRequest#(Bit#(12), Bit#(64)) makeRequest(Bool write, Bit#(12) addr, Bit#(64) data); 
return BRAMRequest{ write: write,
responseOnWrite:True,
address: addr,
datain: data
};
endfunction

(*synthesize*)
module top(Empty);
 // Several instantiations - Alternative 2
 Ifc_conv systolic1 <- mkmac_array();
 Ifc_conv systolic2 <- mkmac_array();

 Ifc_conv systolic3 <- mkmac_array();
 Ifc_conv systolic4 <- mkmac_array();

 Ifc_conv systolic5 <- mkmac_array();
 Ifc_conv systolic6 <- mkmac_array();
 Ifc_conv systolic7 <- mkmac_array();

 Ifc_conv systolic8 <- mkmac_array();
 Ifc_conv systolic9 <- mkmac_array();

 Ifc_conv systolic10 <- mkmac_array();
 Ifc_conv systolic11 <- mkmac_array();
 Ifc_conv systolic12 <- mkmac_array();

// RegFile for weights
RegFile#(Bit#(12) , FloatingPoint#(11,52)) stimulus <- mkRegFileLoad("weights.txt", 0, 1024);
Reg#(Bit#(12)) rg_read_index<-mkReg(0);

// Inputs to the systolic array
Vector#(4, Reg#(FloatingPoint#(11,52))) _input <- replicateM( mkReg( 0 ) ) ;
Vector#(4, Reg#(FloatingPoint#(11,52))) _input2 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input3 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input4 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input5 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input6 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input7 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input8 <- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) _input9 <- replicateM( mkReg( 0 ) );

Vector#(4, Reg#(FloatingPoint#(11,52))) w1<- replicateM( mkReg( 0 ) ); 
Vector#(4, Reg#(FloatingPoint#(11,52))) w2<- replicateM( mkReg( 0 ) ); 
Vector#(4, Reg#(FloatingPoint#(11,52))) w3<- replicateM( mkReg( 0 ) ); 
Vector#(4, Reg#(FloatingPoint#(11,52))) w4<- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) w5<- replicateM( mkReg( 0 ) ); 
Vector#(4, Reg#(FloatingPoint#(11,52))) w6<- replicateM( mkReg( 0 ) ); 
Vector#(4, Reg#(FloatingPoint#(11,52))) w7<- replicateM( mkReg( 0 ) ); 
Vector#(4, Reg#(FloatingPoint#(11,52))) w8<- replicateM( mkReg( 0 ) );
Vector#(4, Reg#(FloatingPoint#(11,52))) w9<- replicateM( mkReg( 0 ) );

// BRAM for the image pixels
BRAM_Configure cfg = defaultValue;
cfg.memorySize=3073;
cfg.allowWriteResponseBypass = False;
cfg.loadFormat = tagged Hex "image0.txt";
BRAM2Port#(Bit#(12), Bit#(64)) dut1 <- mkBRAM2Server(cfg);

//counters and flags
Reg#(int) i  <-  mkReg(0);
Reg#(int) j  <-  mkReg(0);
Reg#(int) k  <-  mkReg(0);
Reg#(int) rg_input_count <-mkReg(0);
Reg#(int) _index <-mkReg(0);
Reg#(int) g<-mkReg(0);
Reg#(int) conv_cycle <-mkReg(0);
Reg#(int) rg_new_input <- mkReg(0);
Reg#(int) x<-mkReg(1);

FIFOF#(FloatingPoint#(11,52))  inputdataFifo <-  mkSizedFIFOF(36);
FIFOF#(int) indexFifo <-mkSizedFIFOF(36);

Reg#(Bool) rg_load_done <- mkReg(False);
Reg#(Bool) rg_fifo_valid <- mkReg(False);

FloatingPoint#(11,52) input_data[32][32];
FloatingPoint#(11,52) weight_kernel[3][3];
int input2[34][34];
Inps i1;
int temp[256][36];

// FPU instantiation to convert into to fp
let fconv <- mk_fpu_int_to_dp();

//zero padding around the borders
input2=zero_padding(input_data);

//GEMM
i1=fn_im2col(input2);

// 9 rows of 4 index values
for (int e=0;e<256;e=e+1) begin
for (int b=0;b<9;b=b+1) begin
for (int m=0;m<4;m=m+1) begin
  temp[e][b*4+m]=i1.windows[b][m+(4*e)];
  end
  end
  end

rule rl_index_fifo (_index<256);
   indexFifo.enq(temp[_index][i]);
   if (i==35) begin
  _index<=_index+1;
  i<=0;
   end
   else
     i<=i+1; 
endrule

// Accessing BRAM
rule rl_datain (!indexFifo.notFull && g <36);
  Bit#(12) data_addr = truncate(pack(indexFifo.first));
  dut1.portA.request.put(makeRequest(False, data_addr, 0));
  if (g==35)
    g<=0;
  else
    g<=g+1;
  indexFifo.deq();
endrule

rule rl_weights (rg_read_index<9);
    let _e = stimulus.sub(rg_read_index);
    case(rg_read_index) matches
    0: w1[0]<=_e;
    1: w2[0]<=_e;
    2: w3[0]<=_e;
    3: w4[0]<=_e;
    4: w5[0]<=_e;
    5: w6[0]<=_e;
    6: w7[0]<=_e;
    7: w8[0]<=_e;
    8: w9[0]<=_e;
    endcase
    rg_read_index<=rg_read_index+1;
endrule

// Get Values for image pixels
rule rl_dataout  ;
  let data1<- dut1.portA.response.get;
  fconv.start (data1, 1'b1, 1'b1, 3'b000);
  j<=j+1;
  if (j>=35) rg_load_done<=True;
   endrule

// FPU convert into to fp
rule rl_int_to_fpu(j>0 && conv_cycle<2);
  let x =  fconv.receive();  
  let valid = x.valid;
  let out = x.value;
  let flags = x.ex;
  Bit#(64) _out = {pack(out.sign), out.exp, out.sfd}; 
  let data2=FloatingPoint{sign : unpack(_out[`a4]), exp: _out[`a3:`a2], sfd: _out[`a1:0]};  
  
  if (conv_cycle==1) begin
  inputdataFifo.enq(data2);
  conv_cycle<=0;
  end
  else 
  conv_cycle<=conv_cycle+1;
endrule

// Arranging in arrays
rule rl_clearing_fifo ;
   if (rg_input_count<4)
   _input[rg_input_count]<=inputdataFifo.first;
   else if (rg_input_count<8)
   _input2[rg_input_count-4]<=inputdataFifo.first;
   else if (rg_input_count<12)
   _input3[rg_input_count-8]<=inputdataFifo.first;
   else if (rg_input_count<16)
   _input4[rg_input_count-12]<=inputdataFifo.first;
   else if (rg_input_count<20)
   _input5[rg_input_count-16]<=inputdataFifo.first;
   else if (rg_input_count<24)
   _input6[rg_input_count-20]<=inputdataFifo.first;
   else if (rg_input_count<28)
   _input7[rg_input_count-24]<=inputdataFifo.first;
   else if (rg_input_count<32)
   _input8[rg_input_count-28]<=inputdataFifo.first;
   else if (rg_input_count<36)
   _input9[rg_input_count-32]<=inputdataFifo.first;
   
   
if (rg_input_count==36 ) begin
   rg_input_count<=0;
   rg_new_input<=rg_new_input+1;
  rg_fifo_valid<=True;
  end
else begin
  inputdataFifo.deq();
   rg_input_count<=rg_input_count+1;
   rg_fifo_valid<=False;
   end
   endrule

rule rl_computation_engine (rg_load_done==True && rg_fifo_valid );
   k<=k+1;
  Bit#(64) _final[1024];
  systolic1.ma_top_cnn_input(rg_new_input, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
  if(k>50) $finish(0);
endrule


rule rl_c2 (rg_fifo_valid && rg_new_input==38);
systolic2.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule

rule rl_c3 (rg_fifo_valid && rg_new_input==39);
systolic3.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule

rule rl_c4 (rg_fifo_valid && rg_new_input==40);
systolic4.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule

rule rl_c5 (rg_fifo_valid && rg_new_input==41);
systolic5.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule

rule rl_c6 (rg_fifo_valid && rg_new_input==42);
systolic6.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule

rule rl_c7 (rg_fifo_valid && rg_new_input==43);
systolic7.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule

rule rl_c8 (rg_fifo_valid && rg_new_input==44);
systolic8.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule

rule rl_c9 (rg_fifo_valid && rg_new_input==45);
systolic9.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule

rule rl_c10 (rg_fifo_valid && rg_new_input==46);
systolic10.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule

rule rl_c11 (rg_fifo_valid && rg_new_input==47);
systolic11.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule

rule rl_c12 (rg_fifo_valid && rg_new_input==48);
systolic12.ma_top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
endmodule