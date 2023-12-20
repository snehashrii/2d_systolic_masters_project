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

function Array#(Bit#(64)) searchable( Vector#(2000, Reg#(Bit#(64))) final_output);
Bool flag=False;
Bit#(64) _final[1024];
int counter=0;

for (int u=0;u<2000;u=u+1) begin
   for(int z=0; z<counter;z=z+1) begin
       if (_final[z]==final_output[u]) begin
           flag=True;
           end
            end
            if(flag!=True) begin
               _final[counter]=final_output[u];
               counter=counter+1;
               end
               else
               flag=False;
           end
return _final;
endfunction

(*synthesize*)
module top(Empty);
     Ifc_conv systolic1 <- mkmac_array();
     Ifc_conv systolic2 <- mkmac_array();
     Ifc_conv systolic3 <- mkmac_array();
     Ifc_conv systolic4 <- mkmac_array();
     Ifc_conv systolic5 <- mkmac_array();
     Ifc_conv systolic6 <- mkmac_array();
   //  Ifc_conv systolic7 <- mkmac_array();
  //   Ifc_conv systolic8 <- mkmac_array();
   //  Ifc_conv systolic9 <- mkmac_array();
    // Ifc_conv systolic10 <- mkmac_array();
    /* Ifc_conv systolic11 <- mkmac_array();
     Ifc_conv systolic12 <- mkmac_array();
     Ifc_conv systolic13 <- mkmac_array();
     Ifc_conv systolic14 <- mkmac_array();
     Ifc_conv systolic15 <- mkmac_array();
     Ifc_conv systolic16 <- mkmac_array();
     Ifc_conv systolic17 <- mkmac_array();
     Ifc_conv systolic18 <- mkmac_array();
     Ifc_conv systolic19 <- mkmac_array();
     Ifc_conv systolic20 <- mkmac_array();
     Ifc_conv systolic21 <- mkmac_array();
     Ifc_conv systolic22 <- mkmac_array();
     Ifc_conv systolic23 <- mkmac_array();
     Ifc_conv systolic24 <- mkmac_array();
     Ifc_conv systolic25 <- mkmac_array();
     Ifc_conv systolic26 <- mkmac_array();
     Ifc_conv systolic27 <- mkmac_array();
     Ifc_conv systolic28 <- mkmac_array();
     Ifc_conv systolic29 <- mkmac_array();
     Ifc_conv systolic30 <- mkmac_array();
     Ifc_conv systolic31 <- mkmac_array();
     Ifc_conv systolic32 <- mkmac_array();
     Ifc_conv systolic33 <- mkmac_array();
     Ifc_conv systolic34 <- mkmac_array();
     Ifc_conv systolic35 <- mkmac_array();
     Ifc_conv systolic36 <- mkmac_array();
     Ifc_conv systolic37 <- mkmac_array();
     Ifc_conv systolic38 <- mkmac_array();
     Ifc_conv systolic39 <- mkmac_array();
     Ifc_conv systolic40 <- mkmac_array();
     Ifc_conv systolic41 <- mkmac_array();
     Ifc_conv systolic42 <- mkmac_array();
     Ifc_conv systolic43 <- mkmac_array();
     Ifc_conv systolic44 <- mkmac_array();
     Ifc_conv systolic45 <- mkmac_array();
     Ifc_conv systolic46 <- mkmac_array();
     Ifc_conv systolic47 <- mkmac_array();
     Ifc_conv systolic48 <- mkmac_array();

     Ifc_conv systolic49 <- mkmac_array();
     Ifc_conv systolic50 <- mkmac_array();
     Ifc_conv systolic51 <- mkmac_array();
     Ifc_conv systolic52 <- mkmac_array();
     Ifc_conv systolic53 <- mkmac_array();
     Ifc_conv systolic54 <- mkmac_array();
     Ifc_conv systolic55 <- mkmac_array();
     Ifc_conv systolic56 <- mkmac_array();
     Ifc_conv systolic57 <- mkmac_array();
     Ifc_conv systolic58 <- mkmac_array();
     Ifc_conv systolic59 <- mkmac_array();
     Ifc_conv systolic60 <- mkmac_array();
     Ifc_conv systolic61 <- mkmac_array();
     Ifc_conv systolic62 <- mkmac_array();
     Ifc_conv systolic63 <- mkmac_array();
     Ifc_conv systolic64 <- mkmac_array();*/
    /* Ifc_conv systolic65 <- mkmac_array();
     Ifc_conv systolic66 <- mkmac_array();
     Ifc_conv systolic67 <- mkmac_array();
     Ifc_conv systolic68 <- mkmac_array();
     Ifc_conv systolic69 <- mkmac_array();
     Ifc_conv systolic70 <- mkmac_array();
     Ifc_conv systolic71 <- mkmac_array();
     Ifc_conv systolic72 <- mkmac_array();
     Ifc_conv systolic73 <- mkmac_array();
     Ifc_conv systolic74 <- mkmac_array();
     Ifc_conv systolic75 <- mkmac_array();
     Ifc_conv systolic76 <- mkmac_array();
     Ifc_conv systolic77 <- mkmac_array();
     Ifc_conv systolic78 <- mkmac_array();
     Ifc_conv systolic79 <- mkmac_array();
     Ifc_conv systolic80 <- mkmac_array();
     Ifc_conv systolic81 <- mkmac_array();
     Ifc_conv systolic82 <- mkmac_array();
     Ifc_conv systolic83 <- mkmac_array();
     Ifc_conv systolic84 <- mkmac_array();
     Ifc_conv systolic85 <- mkmac_array();
      Ifc_conv systolic86 <- mkmac_array();
      Ifc_conv systolic87 <- mkmac_array();
      Ifc_conv systolic88 <- mkmac_array();
      Ifc_conv systolic89 <- mkmac_array();
      Ifc_conv systolic90 <- mkmac_array();
      Ifc_conv systolic91 <- mkmac_array();
      Ifc_conv systolic92 <- mkmac_array();
      Ifc_conv systolic93 <- mkmac_array();
      Ifc_conv systolic94 <- mkmac_array();
      Ifc_conv systolic95 <- mkmac_array();
      Ifc_conv systolic96 <- mkmac_array();
      Ifc_conv systolic97 <- mkmac_array();
      Ifc_conv systolic98 <- mkmac_array();
      Ifc_conv systolic99 <- mkmac_array();
      Ifc_conv systolic100 <- mkmac_array();
      Ifc_conv systolic101 <- mkmac_array();
      Ifc_conv systolic102 <- mkmac_array();
      Ifc_conv systolic103 <- mkmac_array();
      Ifc_conv systolic104 <- mkmac_array();
      Ifc_conv systolic105 <- mkmac_array();
      Ifc_conv systolic106 <- mkmac_array();
      Ifc_conv systolic107 <- mkmac_array();
      Ifc_conv systolic108 <- mkmac_array();
      Ifc_conv systolic109 <- mkmac_array();
      Ifc_conv systolic110 <- mkmac_array();
      Ifc_conv systolic111 <- mkmac_array();
      Ifc_conv systolic112 <- mkmac_array();
      Ifc_conv systolic113 <- mkmac_array();
      Ifc_conv systolic114 <- mkmac_array();
      Ifc_conv systolic115 <- mkmac_array();
      Ifc_conv systolic116 <- mkmac_array();
      Ifc_conv systolic117 <- mkmac_array();
      Ifc_conv systolic118 <- mkmac_array();
      Ifc_conv systolic119 <- mkmac_array();
      Ifc_conv systolic120 <- mkmac_array();
      Ifc_conv systolic121 <- mkmac_array();
      Ifc_conv systolic122 <- mkmac_array();
      Ifc_conv systolic123 <- mkmac_array();
      Ifc_conv systolic124 <- mkmac_array();
      Ifc_conv systolic125 <- mkmac_array();
      Ifc_conv systolic126 <- mkmac_array();
      Ifc_conv systolic127 <- mkmac_array();
      Ifc_conv systolic128 <- mkmac_array();
      Ifc_conv systolic129 <- mkmac_array();
      Ifc_conv systolic130 <- mkmac_array();
      Ifc_conv systolic131 <- mkmac_array();
      Ifc_conv systolic132 <- mkmac_array();
      Ifc_conv systolic133 <- mkmac_array();
      Ifc_conv systolic134 <- mkmac_array();
      Ifc_conv systolic135 <- mkmac_array();
      Ifc_conv systolic136 <- mkmac_array();
      Ifc_conv systolic137 <- mkmac_array();
      Ifc_conv systolic138 <- mkmac_array();
      Ifc_conv systolic139 <- mkmac_array();
      Ifc_conv systolic140 <- mkmac_array();
      Ifc_conv systolic141 <- mkmac_array();
      Ifc_conv systolic142 <- mkmac_array();
      Ifc_conv systolic143 <- mkmac_array();
      Ifc_conv systolic144 <- mkmac_array();
      Ifc_conv systolic145 <- mkmac_array();
      Ifc_conv systolic146 <- mkmac_array();
      Ifc_conv systolic147 <- mkmac_array();
      Ifc_conv systolic148 <- mkmac_array();
      Ifc_conv systolic149 <- mkmac_array();
      Ifc_conv systolic150 <- mkmac_array();
      Ifc_conv systolic151 <- mkmac_array();
      Ifc_conv systolic152 <- mkmac_array();
      Ifc_conv systolic153 <- mkmac_array();
      Ifc_conv systolic154 <- mkmac_array();
      Ifc_conv systolic155 <- mkmac_array();
      Ifc_conv systolic156 <- mkmac_array();
      Ifc_conv systolic157 <- mkmac_array();
      Ifc_conv systolic158 <- mkmac_array();
      Ifc_conv systolic159 <- mkmac_array();
      Ifc_conv systolic160 <- mkmac_array();
      Ifc_conv systolic161 <- mkmac_array();
      Ifc_conv systolic162 <- mkmac_array();
      Ifc_conv systolic163 <- mkmac_array();
      Ifc_conv systolic164 <- mkmac_array();
      Ifc_conv systolic165 <- mkmac_array();
      Ifc_conv systolic166 <- mkmac_array();
      Ifc_conv systolic167 <- mkmac_array();
      Ifc_conv systolic168 <- mkmac_array();
      Ifc_conv systolic169 <- mkmac_array();
      Ifc_conv systolic170 <- mkmac_array();
      Ifc_conv systolic171 <- mkmac_array();
      Ifc_conv systolic172 <- mkmac_array();
      Ifc_conv systolic173 <- mkmac_array();
      Ifc_conv systolic174 <- mkmac_array();
      Ifc_conv systolic175 <- mkmac_array();
      Ifc_conv systolic176 <- mkmac_array();
      Ifc_conv systolic177 <- mkmac_array();
      Ifc_conv systolic178 <- mkmac_array();
      Ifc_conv systolic179 <- mkmac_array();
      Ifc_conv systolic180 <- mkmac_array();
      Ifc_conv systolic181 <- mkmac_array();
      Ifc_conv systolic182 <- mkmac_array();
      Ifc_conv systolic183 <- mkmac_array();
      Ifc_conv systolic184 <- mkmac_array();
      Ifc_conv systolic185 <- mkmac_array();
      Ifc_conv systolic186 <- mkmac_array();
      Ifc_conv systolic187 <- mkmac_array();
      Ifc_conv systolic188 <- mkmac_array();
      Ifc_conv systolic189 <- mkmac_array();
      Ifc_conv systolic190 <- mkmac_array();
      Ifc_conv systolic191 <- mkmac_array();
      Ifc_conv systolic192 <- mkmac_array();
      Ifc_conv systolic193 <- mkmac_array();
      Ifc_conv systolic194 <- mkmac_array();
      Ifc_conv systolic195 <- mkmac_array();
      Ifc_conv systolic196 <- mkmac_array();
      Ifc_conv systolic197 <- mkmac_array();
      Ifc_conv systolic198 <- mkmac_array();
      Ifc_conv systolic199 <- mkmac_array();
      Ifc_conv systolic200 <- mkmac_array();
      Ifc_conv systolic201 <- mkmac_array();
      Ifc_conv systolic202 <- mkmac_array();
      Ifc_conv systolic203 <- mkmac_array();
      Ifc_conv systolic204 <- mkmac_array();
      Ifc_conv systolic205 <- mkmac_array();
      Ifc_conv systolic206 <- mkmac_array();
      Ifc_conv systolic207 <- mkmac_array();
      Ifc_conv systolic208 <- mkmac_array();
      Ifc_conv systolic209 <- mkmac_array();
      Ifc_conv systolic210 <- mkmac_array();
      Ifc_conv systolic211 <- mkmac_array();
      Ifc_conv systolic212 <- mkmac_array();
      Ifc_conv systolic213 <- mkmac_array();
      Ifc_conv systolic214 <- mkmac_array();
      Ifc_conv systolic215 <- mkmac_array();
      Ifc_conv systolic216 <- mkmac_array();
      Ifc_conv systolic217 <- mkmac_array();
      Ifc_conv systolic218 <- mkmac_array();
      Ifc_conv systolic219 <- mkmac_array();
      Ifc_conv systolic220 <- mkmac_array();
      Ifc_conv systolic221 <- mkmac_array();
      Ifc_conv systolic222 <- mkmac_array();
      Ifc_conv systolic223 <- mkmac_array();
      Ifc_conv systolic224 <- mkmac_array();
      Ifc_conv systolic225 <- mkmac_array();
      Ifc_conv systolic226 <- mkmac_array();
      Ifc_conv systolic227 <- mkmac_array();
      Ifc_conv systolic228 <- mkmac_array();
      Ifc_conv systolic229 <- mkmac_array();
      Ifc_conv systolic230 <- mkmac_array();
      Ifc_conv systolic231 <- mkmac_array();
      Ifc_conv systolic232 <- mkmac_array();
      Ifc_conv systolic233 <- mkmac_array();
      Ifc_conv systolic234 <- mkmac_array();
      Ifc_conv systolic235 <- mkmac_array();
      Ifc_conv systolic236 <- mkmac_array();
      Ifc_conv systolic237 <- mkmac_array();
      Ifc_conv systolic238 <- mkmac_array();
      Ifc_conv systolic239 <- mkmac_array();
      Ifc_conv systolic240 <- mkmac_array();
      Ifc_conv systolic241 <- mkmac_array();
      Ifc_conv systolic242 <- mkmac_array();
      Ifc_conv systolic243 <- mkmac_array();
      Ifc_conv systolic244 <- mkmac_array();
      Ifc_conv systolic245 <- mkmac_array();
      Ifc_conv systolic246 <- mkmac_array();
      Ifc_conv systolic247 <- mkmac_array();
      Ifc_conv systolic248 <- mkmac_array();
      Ifc_conv systolic249 <- mkmac_array();
      Ifc_conv systolic250 <- mkmac_array();
      Ifc_conv systolic251 <- mkmac_array();
      Ifc_conv systolic252 <- mkmac_array();
      Ifc_conv systolic253 <- mkmac_array();
      Ifc_conv systolic254 <- mkmac_array();
      Ifc_conv systolic255 <- mkmac_array();
      Ifc_conv systolic256 <- mkmac_array();*/

    Vector#(32, Reg#(Bit#(32))) ff <-replicateM( mkReg( 0 ) );
     RegFile#(Bit#(12) , FloatingPoint#(11,52)) stimulus <- mkRegFileLoad("weights.txt", 0, 1024);
     Reg#(Bit#(12)) read_index<-mkReg(0);

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
    BRAM_Configure cfg = defaultValue;
    cfg.memorySize=3073;
    cfg.allowWriteResponseBypass = False;
    cfg.loadFormat = tagged Hex "image0.txt";
    BRAM2Port#(Bit#(12), Bit#(64)) dut1 <- mkBRAM2Server(cfg);
    Reg#(int) i  <-  mkReg(0);
    Reg#(int) j  <-  mkReg(0);
    Reg#(int) k  <-  mkReg(0);
    Reg#(int) l_by_4 <- mkReg(0);
    FIFOF#(FloatingPoint#(11,52))  inputdataFifo <-  mkSizedFIFOF(36);
    FIFOF#(int) indexFifo <-mkSizedFIFOF(36);
    Reg#(Bool) load_done <- mkReg(False);
    Reg#(Bool) fifo_valid <- mkReg(False);
    FloatingPoint#(11,52) input_data[32][32];
    FloatingPoint#(11,52) weight_kernel[3][3];
    Reg#(int) input_count <-mkReg(0);
    int input2[34][34];
Inps i1;
input2=zero_padding(input_data);
i1=fn_im2col(input2, weight_kernel);
int temp[256][36];
for (int e=0;e<256;e=e+1) begin
for (int b=0;b<9;b=b+1) begin
for (int m=0;m<4;m=m+1) begin
  temp[e][b*4+m]=i1.windows[b][m+(4*e)];
  end
  end
  end
Reg#(int) _index <-mkReg(0);
rule index_fifo (_index<256);
   indexFifo.enq(temp[_index][i]);
   if (i==35) begin
      _index<=_index+1;
      i<=0;
    end
    else
      i<=i+1;
 // $display("INDEX %d %d %d", _index, i, temp[_index][i]);
  endrule
let fconv <- mk_fpu_int_to_dp();
Reg#(int) g<-mkReg(0);
//Int#(32) bb[4];
//bb=max_pooling(input_data1);
//rule pool;
  //$display("POOLING %0d %0d %0d %0d", bb[0], bb[1], bb[2], bb[3]);
  //endrule
rule datain (!indexFifo.notFull && g <36);
          Bit#(12) data_addr = truncate(pack(indexFifo.first));
         // $display("data addr %d", data_addr);
          dut1.portA.request.put(makeRequest(False, data_addr, 0));
          if (g==35)
             g<=0;
          else
             g<=g+1;
          indexFifo.deq();
       endrule
rule weights (read_index<9);
    let _e = stimulus.sub(read_index);
   // $display("weights %0h", _e);
    case(read_index) matches
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
    read_index<=read_index+1;
    endrule
rule dataout  ;
          let data1<- dut1.portA.response.get;
          
        //  $display("%0d dut1read[0] = %h : loading ... %0d",j, data1, load_done);
          fconv.start (data1, 1'b1, 1'b1, 3'b000);
          j<=j+1;
          if (j>=35) load_done<=True;
       endrule
Reg#(int) conv_cycle <-mkReg(0);
Reg#(Bool) output_status <- mkReg(True);
rule jhjg(j>0 && conv_cycle<2);
  let x =  fconv.receive();  
      let valid = x.valid;
      let out = x.value;
      let flags = x.ex;
  Bit#(64) _out = {pack(out.sign), out.exp, out.sfd}; 
  let data2=FloatingPoint{sign : unpack(_out[`a4]), exp: _out[`a3:`a2], sfd: _out[`a1:0]};  
  
  if (conv_cycle==1) begin
      inputdataFifo.enq(data2);
     // $display("%d tell me this works %0h", j, data2);
      conv_cycle<=0;
      end
  else 
    conv_cycle<=conv_cycle+1;
  endrule
  
  Reg#(int) new_input <- mkReg(0);
rule clearing_fifo ;
  // $display("%d %d data from bram %0h",output_status, input_count,inputdataFifo.first);
   if (input_count<4)
       _input[input_count]<=inputdataFifo.first;
   else if (input_count<8)
       _input2[input_count-4]<=inputdataFifo.first;
   else if (input_count<12)
       _input3[input_count-8]<=inputdataFifo.first;
   else if (input_count<16)
       _input4[input_count-12]<=inputdataFifo.first;
   else if (input_count<20)
       _input5[input_count-16]<=inputdataFifo.first;
   else if (input_count<24)
       _input6[input_count-20]<=inputdataFifo.first;
   else if (input_count<28)
       _input7[input_count-24]<=inputdataFifo.first;
   else if (input_count<32)
       _input8[input_count-28]<=inputdataFifo.first;
   else if (input_count<36)
       _input9[input_count-32]<=inputdataFifo.first;
   
       
    if (input_count==36 ) begin
           //inputdataFifo.deq();
           input_count<=0;
           new_input<=new_input+1;
      fifo_valid<=True;
      end
    else begin
      inputdataFifo.deq();
       input_count<=input_count+1;
       fifo_valid<=False;
       end
   endrule

//(*fire_when_enabled*)
rule computation_engine (load_done==True && fifo_valid );
       k<=k+1;
  Bit#(64) _final[1024];
  //$display("SNEHA WEIGHT %0h %0h %0h %0h %0h %0h %0h %0h %0h", w1[0],w2[0],w3[0],w4[0],w5[0],w6[0],w7[0],w8[0],w9[0]);
 // $display("%d SNEHA %0h %0h %0h %0h %0h %0h %0h %0h %0h",new_input, _input[0],_input2[0],_input3[0],_input4[0],_input5[0],_input6[0],_input7[0],_input8[0],_input9[0]);
  systolic1.top_cnn_input(new_input, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
  if(k>512) $finish(0);
endrule

Reg#(int) x<-mkReg(1);
rule c2 (fifo_valid && new_input==2);
systolic2.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c3 (fifo_valid && new_input==3);
systolic3.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c4 (fifo_valid && new_input==4);
systolic4.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c5 (fifo_valid && new_input==5);
systolic5.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c6 (fifo_valid && new_input==6);
systolic6.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
/*rule c7 (fifo_valid && new_input==7);
systolic7.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c8 (fifo_valid && new_input==8);
systolic8.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c9 (fifo_valid && new_input==9);
systolic9.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c10 (fifo_valid && new_input==10);
systolic10.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule*/
/*rule c11 (fifo_valid && new_input==11);
systolic11.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c12 (fifo_valid && new_input==12);
systolic12.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c13 (fifo_valid && new_input==13);
systolic13.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c14 (fifo_valid && new_input==14);
systolic14.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c15 (fifo_valid && new_input==15);
systolic15.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c16 (fifo_valid && new_input==16);
systolic16.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c17 (fifo_valid && new_input==17);
systolic17.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c18 (fifo_valid && new_input==18);
systolic18.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c19 (fifo_valid && new_input==19);
systolic19.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c20 (fifo_valid && new_input==20);
systolic20.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c21 (fifo_valid && new_input==21);
systolic21.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c22 (fifo_valid && new_input==22);
systolic22.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c23 (fifo_valid && new_input==23);
systolic23.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c24 (fifo_valid && new_input==24);
systolic24.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c25 (fifo_valid && new_input==25);
systolic25.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c26 (fifo_valid && new_input==26);
systolic26.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c27 (fifo_valid && new_input==27);
systolic27.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c28 (fifo_valid && new_input==28);
systolic28.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c29 (fifo_valid && new_input==29);
systolic29.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c30 (fifo_valid && new_input==30);
systolic30.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c31 (fifo_valid && new_input==31);
systolic31.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c32 (fifo_valid && new_input==32);
systolic32.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c33 (fifo_valid && new_input==33);
systolic33.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c34 (fifo_valid && new_input==34);
systolic34.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c35 (fifo_valid && new_input==35);
systolic35.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c36 (fifo_valid && new_input==36);
systolic36.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c37 (fifo_valid && new_input==37);
systolic37.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c38 (fifo_valid && new_input==38);
systolic38.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c39 (fifo_valid && new_input==39);
systolic39.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c40 (fifo_valid && new_input==40);
systolic40.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c41 (fifo_valid && new_input==41);
systolic41.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c42 (fifo_valid && new_input==42);
systolic42.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c43 (fifo_valid && new_input==43);
systolic43.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c44 (fifo_valid && new_input==44);
systolic44.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c45 (fifo_valid && new_input==45);
systolic45.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c46 (fifo_valid && new_input==46);
systolic46.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c47 (fifo_valid && new_input==47);
systolic47.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c48 (fifo_valid && new_input==48);
systolic48.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c49 (fifo_valid && new_input==49);
systolic49.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c50 (fifo_valid && new_input==50);
systolic50.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c51 (fifo_valid && new_input==51);
systolic51.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c52 (fifo_valid && new_input==52);
systolic52.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c53 (fifo_valid && new_input==53);
systolic53.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c54 (fifo_valid && new_input==54);
systolic54.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c55 (fifo_valid && new_input==55);
systolic55.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c56 (fifo_valid && new_input==56);
systolic56.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c57 (fifo_valid && new_input==57);
systolic57.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c58 (fifo_valid && new_input==58);
systolic58.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c59 (fifo_valid && new_input==59);
systolic59.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c60 (fifo_valid && new_input==60);
systolic60.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c61 (fifo_valid && new_input==61);
systolic61.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c62 (fifo_valid && new_input==62);
systolic62.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c63 (fifo_valid && new_input==63);
systolic63.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c64 (fifo_valid && new_input==64);
systolic64.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule*/
/*rule c65 (fifo_valid && new_input==65);
systolic65.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c66 (fifo_valid && new_input==66);
systolic66.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c67 (fifo_valid && new_input==67);
systolic67.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c68 (fifo_valid && new_input==68);
systolic68.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c69 (fifo_valid && new_input==69);
systolic69.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c70 (fifo_valid && new_input==70);
systolic70.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c71 (fifo_valid && new_input==71);
systolic71.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c72 (fifo_valid && new_input==72);
systolic72.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c73 (fifo_valid && new_input==73);
systolic73.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c74 (fifo_valid && new_input==74);
systolic74.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c75 (fifo_valid && new_input==75);
systolic75.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c76 (fifo_valid && new_input==76);
systolic76.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c77 (fifo_valid && new_input==77);
systolic77.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c78 (fifo_valid && new_input==78);
systolic78.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c79 (fifo_valid && new_input==79);
systolic79.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c80 (fifo_valid && new_input==80);
systolic80.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c81 (fifo_valid && new_input==81);
systolic81.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c82 (fifo_valid && new_input==82);
systolic82.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c83 (fifo_valid && new_input==83);
systolic83.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c84 (fifo_valid && new_input==84);
systolic84.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c85 (fifo_valid && new_input==85);
systolic85.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c86 (fifo_valid && new_input==86);
systolic86.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c87 (fifo_valid && new_input==87);
systolic87.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c88 (fifo_valid && new_input==88);
systolic88.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c89 (fifo_valid && new_input==89);
systolic89.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c90 (fifo_valid && new_input==90);
systolic90.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c91 (fifo_valid && new_input==91);
systolic91.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c92 (fifo_valid && new_input==92);
systolic92.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c93 (fifo_valid && new_input==93);
systolic93.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c94 (fifo_valid && new_input==94);
systolic94.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c95 (fifo_valid && new_input==95);
systolic95.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c96 (fifo_valid && new_input==96);
systolic96.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c97 (fifo_valid && new_input==97);
systolic97.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c98 (fifo_valid && new_input==98);
systolic98.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c99 (fifo_valid && new_input==99);
systolic99.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c100 (fifo_valid && new_input==100);
systolic100.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c101 (fifo_valid && new_input==101);
systolic101.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c102 (fifo_valid && new_input==102);
systolic102.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c103 (fifo_valid && new_input==103);
systolic103.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c104 (fifo_valid && new_input==104);
systolic104.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c105 (fifo_valid && new_input==105);
systolic105.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c106 (fifo_valid && new_input==106);
systolic106.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c107 (fifo_valid && new_input==107);
systolic107.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c108 (fifo_valid && new_input==108);
systolic108.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c109 (fifo_valid && new_input==109);
systolic109.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c110 (fifo_valid && new_input==110);
systolic110.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c111 (fifo_valid && new_input==111);
systolic111.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c112 (fifo_valid && new_input==112);
systolic112.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c113 (fifo_valid && new_input==113);
systolic113.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c114 (fifo_valid && new_input==114);
systolic114.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c115 (fifo_valid && new_input==115);
systolic115.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c116 (fifo_valid && new_input==116);
systolic116.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c117 (fifo_valid && new_input==117);
systolic117.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c118 (fifo_valid && new_input==118);
systolic118.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c119 (fifo_valid && new_input==119);
systolic119.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c120 (fifo_valid && new_input==120);
systolic120.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c121 (fifo_valid && new_input==121);
systolic121.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c122 (fifo_valid && new_input==122);
systolic122.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c123 (fifo_valid && new_input==123);
systolic123.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c124 (fifo_valid && new_input==124);
systolic124.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c125 (fifo_valid && new_input==125);
systolic125.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c126 (fifo_valid && new_input==126);
systolic126.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c127 (fifo_valid && new_input==127);
systolic127.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c128 (fifo_valid && new_input==128);
systolic128.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c129 (fifo_valid && new_input==129);
systolic129.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c130 (fifo_valid && new_input==130);
systolic130.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c131 (fifo_valid && new_input==131);
systolic131.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c132 (fifo_valid && new_input==132);
systolic132.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c133 (fifo_valid && new_input==133);
systolic133.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c134 (fifo_valid && new_input==134);
systolic134.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c135 (fifo_valid && new_input==135);
systolic135.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c136 (fifo_valid && new_input==136);
systolic136.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c137 (fifo_valid && new_input==137);
systolic137.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c138 (fifo_valid && new_input==138);
systolic138.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c139 (fifo_valid && new_input==139);
systolic139.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c140 (fifo_valid && new_input==140);
systolic140.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c141 (fifo_valid && new_input==141);
systolic141.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c142 (fifo_valid && new_input==142);
systolic142.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c143 (fifo_valid && new_input==143);
systolic143.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c144 (fifo_valid && new_input==144);
systolic144.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c145 (fifo_valid && new_input==145);
systolic145.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c146 (fifo_valid && new_input==146);
systolic146.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c147 (fifo_valid && new_input==147);
systolic147.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c148 (fifo_valid && new_input==148);
systolic148.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c149 (fifo_valid && new_input==149);
systolic149.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c150 (fifo_valid && new_input==150);
systolic150.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c151 (fifo_valid && new_input==151);
systolic151.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c152 (fifo_valid && new_input==152);
systolic152.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c153 (fifo_valid && new_input==153);
systolic153.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c154 (fifo_valid && new_input==154);
systolic154.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c155 (fifo_valid && new_input==155);
systolic155.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c156 (fifo_valid && new_input==156);
systolic156.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c157 (fifo_valid && new_input==157);
systolic157.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c158 (fifo_valid && new_input==158);
systolic158.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c159 (fifo_valid && new_input==159);
systolic159.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c160 (fifo_valid && new_input==160);
systolic160.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c161 (fifo_valid && new_input==161);
systolic161.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c162 (fifo_valid && new_input==162);
systolic162.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c163 (fifo_valid && new_input==163);
systolic163.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c164 (fifo_valid && new_input==164);
systolic164.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c165 (fifo_valid && new_input==165);
systolic165.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c166 (fifo_valid && new_input==166);
systolic166.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c167 (fifo_valid && new_input==167);
systolic167.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c168 (fifo_valid && new_input==168);
systolic168.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c169 (fifo_valid && new_input==169);
systolic169.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c170 (fifo_valid && new_input==170);
systolic170.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c171 (fifo_valid && new_input==171);
systolic171.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c172 (fifo_valid && new_input==172);
systolic172.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c173 (fifo_valid && new_input==173);
systolic173.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c174 (fifo_valid && new_input==174);
systolic174.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c175 (fifo_valid && new_input==175);
systolic175.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c176 (fifo_valid && new_input==176);
systolic176.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c177 (fifo_valid && new_input==177);
systolic177.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c178 (fifo_valid && new_input==178);
systolic178.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c179 (fifo_valid && new_input==179);
systolic179.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c180 (fifo_valid && new_input==180);
systolic180.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c181 (fifo_valid && new_input==181);
systolic181.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c182 (fifo_valid && new_input==182);
systolic182.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c183 (fifo_valid && new_input==183);
systolic183.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c184 (fifo_valid && new_input==184);
systolic184.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c185 (fifo_valid && new_input==185);
systolic185.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c186 (fifo_valid && new_input==186);
systolic186.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c187 (fifo_valid && new_input==187);
systolic187.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c188 (fifo_valid && new_input==188);
systolic188.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c189 (fifo_valid && new_input==189);
systolic189.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c190 (fifo_valid && new_input==190);
systolic190.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c191 (fifo_valid && new_input==191);
systolic191.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c192 (fifo_valid && new_input==192);
systolic192.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c193 (fifo_valid && new_input==193);
systolic193.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c194 (fifo_valid && new_input==194);
systolic194.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c195 (fifo_valid && new_input==195);
systolic195.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c196 (fifo_valid && new_input==196);
systolic196.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c197 (fifo_valid && new_input==197);
systolic197.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c198 (fifo_valid && new_input==198);
systolic198.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c199 (fifo_valid && new_input==199);
systolic199.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c200 (fifo_valid && new_input==200);
systolic200.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c201 (fifo_valid && new_input==201);
systolic201.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c202 (fifo_valid && new_input==202);
systolic202.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c203 (fifo_valid && new_input==203);
systolic203.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c204 (fifo_valid && new_input==204);
systolic204.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c205 (fifo_valid && new_input==205);
systolic205.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c206 (fifo_valid && new_input==206);
systolic206.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c207 (fifo_valid && new_input==207);
systolic207.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c208 (fifo_valid && new_input==208);
systolic208.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c209 (fifo_valid && new_input==209);
systolic209.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c210 (fifo_valid && new_input==210);
systolic210.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c211 (fifo_valid && new_input==211);
systolic211.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c212 (fifo_valid && new_input==212);
systolic212.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c213 (fifo_valid && new_input==213);
systolic213.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c214 (fifo_valid && new_input==214);
systolic214.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c215 (fifo_valid && new_input==215);
systolic215.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c216 (fifo_valid && new_input==216);
systolic216.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c217 (fifo_valid && new_input==217);
systolic217.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c218 (fifo_valid && new_input==218);
systolic218.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c219 (fifo_valid && new_input==219);
systolic219.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c220 (fifo_valid && new_input==220);
systolic220.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c221 (fifo_valid && new_input==221);
systolic221.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c222 (fifo_valid && new_input==222);
systolic222.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c223 (fifo_valid && new_input==223);
systolic223.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c224 (fifo_valid && new_input==224);
systolic224.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c225 (fifo_valid && new_input==225);
systolic225.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c226 (fifo_valid && new_input==226);
systolic226.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c227 (fifo_valid && new_input==227);
systolic227.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c228 (fifo_valid && new_input==228);
systolic228.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c229 (fifo_valid && new_input==229);
systolic229.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c230 (fifo_valid && new_input==230);
systolic230.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c231 (fifo_valid && new_input==231);
systolic231.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c232 (fifo_valid && new_input==232);
systolic232.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c233 (fifo_valid && new_input==233);
systolic233.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c234 (fifo_valid && new_input==234);
systolic234.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c235 (fifo_valid && new_input==235);
systolic235.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c236 (fifo_valid && new_input==236);
systolic236.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c237 (fifo_valid && new_input==237);
systolic237.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c238 (fifo_valid && new_input==238);
systolic238.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c239 (fifo_valid && new_input==239);
systolic239.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c240 (fifo_valid && new_input==240);
systolic240.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c241 (fifo_valid && new_input==241);
systolic241.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c242 (fifo_valid && new_input==242);
systolic242.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c243 (fifo_valid && new_input==243);
systolic243.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c244 (fifo_valid && new_input==244);
systolic244.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c245 (fifo_valid && new_input==245);
systolic245.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c246 (fifo_valid && new_input==246);
systolic246.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c247 (fifo_valid && new_input==247);
systolic247.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c248 (fifo_valid && new_input==248);
systolic248.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c249 (fifo_valid && new_input==249);
systolic249.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c250 (fifo_valid && new_input==250);
systolic250.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c251 (fifo_valid && new_input==251);
systolic251.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c252 (fifo_valid && new_input==252);
systolic252.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c253 (fifo_valid && new_input==253);
systolic253.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c254 (fifo_valid && new_input==254);
systolic254.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c255 (fifo_valid && new_input==255);
systolic255.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule
rule c256 (fifo_valid && new_input==256);
systolic256.top_cnn_input(x, True, w1,w2,w3,w4,w5,w6,w7,w8,w9,_input,_input2,_input3,_input4,_input5,_input6,_input7,_input8,_input9);
endrule*/

endmodule