package mac;
import Real::*;
import FloatingPoint::*;
import fpu_fma_pipelined ::*;
import fpu_common::*;

interface Ifc_mac;
method ActionValue#(Bit#(64)) mav_psumout(FloatingPoint#(11,52) weight, FloatingPoint#(11,52) _input, FloatingPoint#(11,52) psum);
endinterface
(*synthesize*)

module mkmac(Ifc_mac);
Reg#(FloatingPoint#(11,52)) input_data <-mkReg(0);
Reg#(FloatingPoint#(11,52)) weight_data <-mkReg(0);
Reg#(FloatingPoint#(11,52)) psum_data <-mkReg(0);
Reg#(Bit#(64)) _out <-mkReg(0);
Reg#(int) rg_cycle <- mkReg(0);
Reg#(Bool) _ready <- mkReg(False);

let fadd<- mk_fpu_fma_dp;
        
rule rl_cycle;
          if(rg_cycle==10) begin
          rg_cycle<=0;
          _ready<=False;
          end
          else begin
          if(rg_cycle==9) begin
              _ready<=True;
              end
          rg_cycle<=rg_cycle+1;
          end
endrule

rule rl_sending_float_data;
        RoundMode rounding_mode=Rnd_Nearest_Away_Zero;
        fadd.send(tuple4(input_data, weight_data, tagged Valid psum_data, rounding_mode));

endrule


rule rl_send_result_float ;
        let x=fadd.receive();
        let valid = x.valid;
              let out = x.value;
              let flags = x.ex;
         _out <= {pack(out.sign), out.exp, out.sfd}; 
endrule

method ActionValue#(Bit#(64)) mav_psumout(FloatingPoint#(11,52) weight, FloatingPoint#(11,52) _input, FloatingPoint#(11,52) psum) /*if (_ready==True)*/;
        Bit#(64) result = 0;
        input_data<=_input;
        weight_data<=weight;
        psum_data<=psum;
        result=zeroExtend(_out);
        return result;
endmethod

endmodule: mkmac
endpackage: mac
