////////////////////////////////////////////////////////////////////////////////
//see LICENSE.iitm
////////////////////////////////////////////////////////////////////////////////
/*
---------------------------------------------------------------------------------------------------

Author: Harrish A, Joyanta Mondal
Email id: harrish1499@gmail.com, ee19m058@smail.iitm.ac.in
Details: Single and Double Precision Fused Multiply-Add Pipelined Modules
         This implementation is derived from modules in fpu_hierarchical/fpu_fma_hierarchical, 
         fpu_pipelined/fpu_booth_multiplier and fpu_pipelined/fpu_adder.
         The two modules are manually pipelined and optimized (optional: register retiming).
         For credits for the original fma module, check fpu_hierarhical/fpu_fma_hierarchical.bsv
--------------------------------------------------------------------------------------------------
*/


//`include "Logger.bsv"
package fpu_fma_pipelined;
import normalize_fma_hierarchical :: * ;
import fpu_common    ::*;
import Vector            ::*;
import Real              ::*;
import BUtils            ::*;
import DefaultValue      ::*;
import FShow             ::*;
import GetPut            ::*;
import ClientServer      ::*;
import FIFO              ::*;
import FixedPoint        ::*;
import DReg  :: *;
import fpu_multiplier_common ::*;
`include "fpu_parameters.bsv"

//`ifdef fpu_hierarchical


// SINGLE PRECISION NORMALIZE and CLA FUNCTIONS
////////////////////////////////////////////////////////////////////////////////

// SP - NORMALIZE_1 PART 1 FUNC
function UInt#(6) normalize_sp_part_1(Bit#(48) sfdin);
   //counting MSB 0s of sfdin
   Vector#(16, Bit#(1)) v1 = unpack(sfdin[47:32]);
   Vector#(16, Bit#(1)) v2 = unpack(sfdin[31:16]); 
   Vector#(16, Bit#(1)) v3 = unpack(sfdin[15:0]);

   UInt#(6) result1=0; 
   UInt#(6) result2=0;  
   UInt#(6) result3=0; 

   Bool done1 = False;
   Bool done2 = False;
   Bool done3 = False;

   for ( Integer p1 = 15; p1 >=0; p1 = p1 - 1)  begin
          
      if ((v1[p1] == 0)&&(!done1))  
         result1 = result1+1 ;
      else
         done1 = True;

      if ((v2[p1] == 0)&&(!done2))  
         result2 = result2+1 ;
      else
         done2 = True;

      if ((v3[p1] == 0)&&(!done3))  
         result3 = result3+1 ;
      else
         done3 = True;
   end

   //MSB 0s of sfdin computed
   UInt#(6) zeros= (|sfdin[47:32]==1)?result1:((|sfdin[31:16]==1)?(result2+16):((|sfdin[15:0]==1)?(result3+32):48));

   return zeros;
endfunction  
/************************************************************************************************************/

// SP - NORMALIZE_2 PART 1 FUNC
function UInt#(6) normalize_sp_2_part_1(Bit#(50) sfdin);

   //counting MSB 0s of sfdin
   Vector#(16, Bit#(1)) v1 = unpack(sfdin[49:34]);  
   Vector#(16, Bit#(1)) v2 = unpack(sfdin[33:18]);   
   Vector#(16, Bit#(1)) v3 = unpack(sfdin[17:2]);
   Vector#( 2, Bit#(1)) v4 = unpack(sfdin[1:0]);

   UInt#(6)  result1=0; 
   UInt#(6)  result2=0;
   UInt#(6)  result3=0; 
   UInt#(6)  result4=0;

   Bool done1 = False;
   Bool done2 = False;
   Bool done3 = False;
   Bool done4 = False;

   for ( Integer p1 = 15; p1 >=0; p1 = p1 - 1) begin

      if ((v1[p1] == 0)&&(!done1))  
         result1 = result1+1 ;
      else
         done1 = True;

      if ((v2[p1] == 0)&&(!done2))  
         result2 = result2+1 ;
      else
         done2 = True;

      if ((v3[p1] == 0)&&(!done3))  
         result3 = result3+1 ;
      else
         done3 = True;

      if ((p1 <=1)&&(v4[p1] == 0)&&(!done4))
         result4 = result4+1 ;
      else
         done4 = True;
   end

   //MSB 0s of sfdin computed
   UInt#(6) zeros= (|sfdin[49:34]==1)?result1:((|sfdin[33:18]==1)?(result2+16):((|sfdin[17:2]==1)?(result3+32):((|sfdin[1:0]==1)?(result4+48):50)));

   return zeros;
endfunction  
/************************************************************************************************************/

// SP - NORMALIZE_1 PART 2 FUNC
function Tuple3#(FloatingPoint#(8,23),Exception,Bit#(50)) normalize_sp_part_2(FloatingPoint#(8,23) out, Bit#(48) sfdin, UInt#(6) zeros);

   Int#(9) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));
   Bit#(50) sfdiN =0;
   Exception exc = defaultValue;

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      sfdiN = {sfdin,2'b0};
   end

   else begin
      if (zeros == 0) begin
         out.exp = (out.exp == 0) ? 2 : out.exp+1;
         sfdin = sfdin << 1;
      end

      else if (zeros == 1) begin
         out.exp = (out.exp == 0) ? 1 : out.exp;
         sfdin = sfdin << 2;
      end

      else if (zeros == fromInteger(valueOf(48))) begin
         out.exp = 0;
      end

      else begin
         Int#(9) shift = zeroExtend(unpack(pack(zeros - 1)));
         Int#(9) maxshift = exp - fromInteger(minexp(out));

         // try to normalize
         `ifdef denormal_support

         if (shift > maxshift) begin
          sfdin = sfdin << maxshift;
          out.exp = 0;
         end

         else begin
            sfdin = sfdin << shift;
            out.exp = out.exp - truncate(pack(shift));
         end
         sfdin = sfdin << 2;
      end

      `else
      if (shift <= maxshift) begin
         sfdin = sfdin << shift;
         out.exp = out.exp - truncate(pack(shift));
      end
      
      sfdin = sfdin << 2;
   end
   `endif

   out.sfd = unpack(truncateLSB(sfdin));
   sfdiN = {1'b0,getHiddenBit(out),sfdin};
   end

   return tuple3(out,exc,sfdiN);
endfunction
/************************************************************************************************************/

// SP - NORMALIZE_2 PART 2 FUNC
function Tuple3#(FloatingPoint#(8,23),Bit#(3),Exception) normalize_sp_2_part_2(UInt#(6) zeros,Int#(10) exp_out,FloatingPoint#(8,23) out, Bit#(50) sfdin,RoundMode rmode);

   Bit#(3) guard = 0;
   Int#(9) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out))); 
   Exception exc = defaultValue;

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      guard = '1;

      if (rmode == Rnd_Minus_Inf || rmode == Rnd_Zero || rmode == Rnd_Plus_Inf) begin
         exc.overflow = True;
         exc.inexact = True;
      end
   end

   if (!((zeros == 0) && (exp == fromInteger(maxexp(out))))) begin
   
      if (zeros == 0) begin
         out.exp = (exp_out + 'd127 == 0) ? 2 : out.exp + 1;
         exp_out = (exp_out + 'd127 == 0) ? 2 : exp_out + 1;

         if (exp_out > 127) begin
            exc.overflow = True;
            exc.inexact = True;
         end
         // carry bit
         sfdin = sfdin << 1;
      end

      // already normalized
      else if (zeros == 1) begin
         if (exp_out + 'd127 == 0) begin
            out.exp = 1;
         end   
         // carry, hidden bits
         sfdin = sfdin << 2;
         if(exp_out > 127)  begin
            exc.overflow = True;
            exc.inexact = True;
         end
      end 

      else if (zeros == fromInteger(50)) begin
         // exactly zero
         out.exp = 0;
      end

      else  begin
         // try to normalize
         Int#(10) shift = zeroExtend(unpack(pack(zeros - 1)));
         Int#(10) maxshift = exp_out + 'd126;

         `ifdef denormal_support
         if (shift > maxshift) begin
            // result will be subnormal
            sfdin = sfdin << maxshift;
            out.exp = 0;
         end
         else begin
            // result will be normal
            sfdin = sfdin << shift;
            out.exp = out.exp - truncate(pack(shift));
            exp_out = exp_out - shift;
            if(exp_out > 127) begin
               exc.overflow = True;
               exc.inexact = True;
            end
         end

         // carry, hidden bits
         sfdin = sfdin << 2;
      end

      `else
      if (shift <= maxshift) begin
         // result will be normal
         sfdin = sfdin << shift;
         out.exp = out.exp - truncate(pack(shift));
      end
      // carry, hidden bits
      sfdin = sfdin << 2;
      end
      `endif
      
      out.sfd = unpack(truncateLSB(sfdin));
      sfdin = sfdin << fromInteger(23);
      guard[2] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;
      guard[1] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;
      guard[0] = |sfdin;
   end

   return tuple3(out,guard,exc);
endfunction
/************************************************************************************************************/

// 50 - BIT CARRY LOOK ADDER FUNC
function Bit#(50) fn_CLA_sp_new(Bit#(1) sub,Bit#(50) sfdX,Bit#(50) sfdY1);

   Bit#(1) cin = (sub == 0)?0:1;                //subtraction part
   Bit#(50) sfdY = (sub==0)?sfdY1:~(sfdY1);
   
   Bit#(50) gen_carry = sfdX&sfdY;              //carry generation
   Bit#(50) prop_carry = sfdX|sfdY;             //carry propagation

   Vector#(50, Bit#(1)) gen_c = unpack(gen_carry);
   Vector#(50, Bit#(1)) prop_c = unpack(prop_carry);

   Vector#(8, Bit#(1)) act_c1 ;
   Vector#(8, Bit#(1)) act_c2_1 ;
   Vector#(8, Bit#(1)) act_c3_1 ;
   Vector#(8, Bit#(1)) act_c4_1 ;
   Vector#(8, Bit#(1)) act_c5_1 ;
   Vector#(8, Bit#(1)) act_c6_1 ;
   Vector#(8, Bit#(1)) act_c7_1 ;
   Vector#(1, Bit#(1)) act_c8_1 ;

   Vector#(8, Bit#(1)) act_c2_0 ;
   Vector#(8, Bit#(1)) act_c3_0 ;
   Vector#(8, Bit#(1)) act_c4_0 ;
   Vector#(8, Bit#(1)) act_c5_0 ;
   Vector#(8, Bit#(1)) act_c6_0 ;
   Vector#(8, Bit#(1)) act_c7_0 ;
   Vector#(1, Bit#(1)) act_c8_0 ;   

   act_c1[0] = cin;
   act_c2_0[0] = 1'b0;
   act_c2_1[0] = 1'b1; 
   act_c3_0[0] = 1'b0;
   act_c3_1[0] = 1'b1;
   act_c4_1[0] = 1'b1;
   act_c4_0[0] = 1'b0;
   act_c5_1[0] = 1'b1;
   act_c5_0[0] = 1'b0;
   act_c6_1[0] = 1'b1;
   act_c6_0[0] = 1'b0;
   act_c7_1[0] = 1'b1;
   act_c7_0[0] = 1'b0;
   act_c8_1[0] = 1'b1;
   act_c8_0[0] = 1'b0;


   for ( Integer k=1;k<8;k=k+1) begin
      act_c1[k]   = gen_c[k-1]  | (prop_c[k-1]&act_c1[k-1]);      // carry generation,1st segment
      act_c2_0[k] = gen_c[k+6]  | (prop_c[k+6]&act_c2_0[k-1]);    // carry generation for c_in=0,2nd segment
      act_c2_1[k] = gen_c[k+6]  | (prop_c[k+6]&act_c2_1[k-1]);    // carry generation for c_in=1,2nd segment  
      act_c3_0[k] = gen_c[k+13] | (prop_c[k+13]&act_c3_0[k-1]);   // carry generation for c_in=0,3rd segment
      act_c3_1[k] = gen_c[k+13] | (prop_c[k+13]&act_c3_1[k-1]);   // carry generation for c_in=1,3rd segment
      act_c4_0[k] = gen_c[k+20] | (prop_c[k+20]&act_c4_0[k-1]);   // carry generation for c_in=0,4th segment
      act_c4_1[k] = gen_c[k+20] | (prop_c[k+20]&act_c4_1[k-1]);   // carry generation for c_in=1,4th segment
      act_c5_0[k] = gen_c[k+27] | (prop_c[k+27]&act_c5_0[k-1]);   // carry generation for c_in=0,5th segment
      act_c5_1[k] = gen_c[k+27] | (prop_c[k+27]&act_c5_1[k-1]);   // carry generation for c_in=1,5th segment
      act_c6_0[k] = gen_c[k+34] | (prop_c[k+34]&act_c6_0[k-1]);   // carry generation for c_in=0,6th segment  
      act_c6_1[k] = gen_c[k+34] | (prop_c[k+34]&act_c6_1[k-1]);   // carry generation for c_in=1,6th segment  
      act_c7_0[k] = gen_c[k+41] | (prop_c[k+41]&act_c7_0[k-1]);   // carry generation for c_in=0,7th segment  
      act_c7_1[k] = gen_c[k+41] | (prop_c[k+41]&act_c7_1[k-1]);   // carry generation for c_in=1,7th segment
   end

   Bit#(8) carry1 = pack(act_c1);
   Bit#(8) carry2_1 = pack(act_c2_1);
   Bit#(8) carry2_0 = pack(act_c2_0);
   Bit#(8) carry3_0 = pack(act_c3_0);
   Bit#(8) carry3_1 = pack(act_c3_1);
   Bit#(8) carry4_1 = pack(act_c4_1);
   Bit#(8) carry4_0 = pack(act_c4_0);
   Bit#(8) carry5_1 = pack(act_c5_1);
   Bit#(8) carry5_0 = pack(act_c5_0);
   Bit#(8) carry6_1 = pack(act_c6_1);
   Bit#(8) carry6_0 = pack(act_c6_0);
   Bit#(8) carry7_1 = pack(act_c7_1);
   Bit#(8) carry7_0 = pack(act_c7_0);
   Bit#(1) carry8_1 = pack(act_c8_1);
   Bit#(1) carry8_0 = pack(act_c8_0);

   //choosing the carry segments according to actual c_in
   Bit#(8) carry2 = (carry1[7]==1'b1)?carry2_1:carry2_0;
   Bit#(8) carry3 = (carry2[7]==1'b1)?carry3_1:carry3_0;
   Bit#(8) carry4 = (carry3[7]==1'b1)?carry4_1:carry4_0;
   Bit#(8) carry5 = (carry4[7]==1'b1)?carry5_1:carry5_0;
   Bit#(8) carry6 = (carry5[7]==1'b1)?carry6_1:carry6_0;
   Bit#(8) carry7 = (carry6[7]==1'b1)?carry7_1:carry7_0;
   Bit#(1) carry8 = (carry7[7]==1'b1)?carry8_1:carry8_0;

   //generating actual carry vector
   Bit#(50) carry = {carry8,carry7[6:0],carry6[6:0],carry5[6:0],carry4[6:0],carry3[6:0],carry2[6:0],carry1[6:0]}; 

   //Actual sum generation
   Bit#(50) sum = sfdX^sfdY^carry;

   return sum;
endfunction
/************************************************************************************************************/

// DOUBLE PRECISION NORMALIZE and CLA FUNCTIONS
////////////////////////////////////////////////////////////////////////////////

// DP - NORMALIZE_1 PART 1 FUNC
function UInt#(7) normalize_dp_1_part_1 (Bit#(106) sfdin);   
   
   Bit#(106) inp = sfdin;
   Vector#(16, Bit#(1)) v1 = unpack(sfdin[105:90]);  
   Vector#(16, Bit#(1)) v2 = unpack(sfdin[89:74]); 
   Vector#(16, Bit#(1)) v3 = unpack(sfdin[73:58]); 
   Vector#(16, Bit#(1)) v4 = unpack(sfdin[57:42]); 
   Vector#(16, Bit#(1)) v5 = unpack(sfdin[41:26]);
   Vector#(16, Bit#(1)) v6 = unpack(sfdin[25:10]);  
   Vector#(10, Bit#(1)) v7 = unpack(sfdin[9:0]); 

   UInt#(7) result1=0;
   UInt#(7) result2=16;
   UInt#(7) result3=32;
   UInt#(7) result4=48;
   UInt#(7) result5=64;
   UInt#(7) result6=80;
   UInt#(7) result7=96;

   Bool done1 = False;
   Bool done2 = False;
   Bool done3 = False;
   Bool done4 = False;
   Bool done5 = False;
   Bool done6 = False;
   Bool done7 = False;

   for( Integer p1 = 15; p1 >=0; p1 = p1 - 1) begin

      if ((v1[p1] == 0)&&(!done1))
         result1 = result1+1 ;
      else
         done1 = True;
   
      if ((v2[p1] == 0)&&(!done2)) 
         result2 = result2+1 ;
      else
         done2 = True;

      if ((v3[p1] == 0)&&(!done3))  
         result3 = result3+1 ;
      else
         done3 = True;

      if ((v4[p1] == 0)&&(!done4))  
         result4 = result4+1 ;
      else
         done4 = True;

      if ((v5[p1] == 0)&&(!done5))  
         result5 = result5+1 ;
      else
         done5 = True;

      if ((v6[p1] == 0)&&(!done6))  
         result6 = result6+1 ;
      else
         done6 = True;

      if ((p1 <=9)&&(v7[p1] == 0)&&(!done7))  
         result7 = result7+1 ;
      else
         done7 = True;
   end
  
   //MSB 0s of sfdin computed

   Bit#(1) flag_1 = |sfdin[105:90];
   Bit#(1) flag_2 = |sfdin[89:74];
   Bit#(1) flag_3 = |sfdin[73:58];
   Bit#(1) flag_4 = |sfdin[57:42];
   Bit#(1) flag_5 = |sfdin[41:26];
   Bit#(1) flag_6 = |sfdin[25:10];
   Bit#(1) flag_7 = |sfdin[9:0];

   UInt#(7) zeros= (flag_1==1)?result1:((flag_2==1)?result2:((flag_3==1)?result3:((flag_4==1)?result4:((flag_5==1)?result5:((flag_6==1)?result6:((flag_7==1)?result7:106))))));

   return zeros;
endfunction
/************************************************************************************************************/

// DP - NORMALIZE_1 PART 2 FUNC
function Tuple3#(FloatingPoint#(11,52),Exception,Bit#(108)) normalize_dp_1_part_2(FloatingPoint#(11,52) out, Bit#(106) sfdin, UInt#(7) zeros);

   Bit#(2) guard = 0;
   Int#(12) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));
   Bit#(108) sfdiN =0;
   Exception exc = defaultValue;

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      sfdiN = {sfdin,2'b0};
      guard = '1;
   end

   else begin
      if (zeros == 0) begin
         out.exp = (out.exp == 0) ? 2 : out.exp+1;
         sfdin = sfdin << 1;
      end

      else if (zeros == 1) begin
         out.exp = (out.exp == 0) ? 1 : out.exp;
         sfdin = sfdin <<2;
      end

      else if (zeros == fromInteger(valueOf(106))) begin
         out.exp = 0;
      end
    
      else begin
         Int#(12) shift = zeroExtend(unpack(pack(zeros - 1)));
         Int#(12) maxshift = exp - fromInteger(minexp(out));

         `ifdef denormal_support
         if (shift > maxshift) begin
            // result will be subnormal
            sfdin = sfdin << maxshift;
            out.exp = 0;
         end
          
         else begin
            // result will be normal
            sfdin = sfdin << shift;
            out.exp = out.exp - truncate(pack(shift));
         end
         sfdin = sfdin << 2;
      end

      `else
      if (shift <= maxshift) begin
         // result will be normal
         sfdin = sfdin << shift;
         out.exp = out.exp - truncate(pack(shift));
      end
      sfdin = sfdin << 2;
   end
   `endif

   out.sfd = unpack(truncateLSB(sfdin));
   sfdiN = {1'b0,getHiddenBit(out),sfdin};
   end

   return tuple3(out,exc,sfdiN);
endfunction
/************************************************************************************************************/

// DP - NORMALIZE_2 PART 1 FUNC
function UInt#(7) normalize_dp_2_part_1(Bit#(108) sfdin);

   Bit#(108) inp = sfdin;
   Vector#(16, Bit#(1)) v1 = unpack(inp[107:92]);  
   Vector#(16, Bit#(1)) v2 = unpack(inp[91:76]);   
   Vector#(16, Bit#(1)) v3 = unpack(inp[75:60]);  
   Vector#(16, Bit#(1)) v4 = unpack(inp[59:44]); 
   Vector#(16, Bit#(1)) v5 = unpack(inp[43:28]); 
   Vector#(16, Bit#(1)) v6 = unpack(inp[27:12]);
   Vector#(12, Bit#(1)) v7 = unpack(inp[11:0]); 

   UInt#(7) result1=0; 
   UInt#(7) result2=16; 
   UInt#(7) result3=32; 
   UInt#(7) result4=48;  
   UInt#(7) result5=64; 
   UInt#(7) result6=80; 
   UInt#(7) result7=96;

   Bool done1 = False;
   Bool done2 = False;
   Bool done3 = False;
   Bool done4 = False;
   Bool done5 = False;
   Bool done6 = False;
   Bool done7 = False;

   for( Integer p1 = 15; p1 >=0; p1 = p1 - 1) begin

      if ((v1[p1] == 0)&&(!done1))  
         result1 = result1+1 ;
      else
         done1 = True;

      if ((v2[p1] == 0)&&(!done2))  
         result2 = result2+1 ;
      else
         done2 = True;

      if ((v3[p1] == 0)&&(!done3))  
         result3 = result3+1 ;
      else
         done3 = True;

      if ((v4[p1] == 0)&&(!done4))  
         result4 = result4+1 ;
      else
         done4 = True;

      if ((v5[p1] == 0)&&(!done5))  
         result5 = result5+1 ;
      else
         done5 = True;

      if ((v6[p1] == 0)&&(!done6))  
         result6 = result6+1 ;
      else
         done6 = True;

      if (p1 <= 11) begin
         if ((v7[p1] == 0)&&(!done7))  
            result7 = result7+1 ;
         else
            done7 = True;
      end
   end
   
   Bit#(1) flag_1 = |inp[107:92];
   Bit#(1) flag_2 = |inp[91:76];
   Bit#(1) flag_3 = |inp[75:60];
   Bit#(1) flag_4 = |inp[59:44];
   Bit#(1) flag_5 = |inp[43:28];
   Bit#(1) flag_6 = |inp[27:12];
   Bit#(1) flag_7 = |inp[11:0];
   
   //MSB 0s of sfdin computed
   UInt#(7) zeros= (flag_1==1)?result1:((flag_2==1)?result2:((flag_3==1)?result3:((flag_4==1)?result4:((flag_5==1)?result5:((flag_6==1)?result6:((flag_7==1)?result7:108))))));
   return zeros;
endfunction  
/************************************************************************************************************/

// DP - NORMALIZE_2 PART 2 FUNC
function Tuple3#(FloatingPoint#(11,52),Bit#(3),Exception) normalize_dp_2_part_2(UInt#(7) zeros,Int#(13) exp_out,FloatingPoint#(11,52) out, Bit#(108) sfdin,RoundMode rmode);

   Bit#(3) guard = 0;
   Int#(12) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out))); 
   Exception exc = defaultValue;

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      guard = '1;

      if(rmode == Rnd_Minus_Inf || rmode == Rnd_Zero || rmode == Rnd_Plus_Inf) begin
         exc.overflow = True;
         exc.inexact = True;
      end
   end

   if (!((zeros == 0) && (exp == fromInteger(maxexp(out))))) begin
      if (zeros == 0) begin
         out.exp = (exp_out + 'd1023 == 0) ? 2 : out.exp + 1;
         exp_out = (exp_out + 'd1023 == 0) ? 2 : exp_out + 1;

         if (exp_out > 1023) begin
            exc.overflow = True;
            exc.inexact = True;
         end
         // carry bit
         sfdin = sfdin << 1;
      end

      else if (zeros == 1) begin
         if (exp_out + 'd1023 == 0) begin
            out.exp = 1;
         end
         // carry, hidden bits
         sfdin = sfdin << 2;
         if(exp_out > 1023) begin
            exc.overflow = True;
            exc.inexact = True;
         end
      end

      else if (zeros == fromInteger(108)) begin
         // exactly zero
         out.exp = 0;
      end

      else  begin
         // try to normalize
         Int#(13) shift = zeroExtend(unpack(pack(zeros - 1)));
         Int#(13) maxshift = exp_out + 'd1022;
         `ifdef denormal_support
         if (shift > maxshift) begin
            // result will be subnormal
            sfdin = sfdin << maxshift;
            out.exp = 0;
         end

         else begin
            // result will be normal
            sfdin = sfdin << shift;
            out.exp = out.exp - truncate(pack(shift));
            exp_out = exp_out - shift;
            if(exp_out > 1023) begin
               exc.overflow = True;
               exc.inexact = True;
            end
         end

         // carry, hidden bits
         sfdin = sfdin << 2;
      end

      `else
      if (shift <= maxshift) begin
         // result will be normal
         sfdin = sfdin << shift;
         out.exp = out.exp - truncate(pack(shift));
      end
      // carry, hidden bits
      sfdin = sfdin << 2;

      end
      `endif
      out.sfd = unpack(truncateLSB(sfdin));
      sfdin = sfdin << fromInteger(52);
      guard[2] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;
      guard[1] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;
      guard[0] = |sfdin;
   end

   return tuple3(out,guard,exc);
endfunction
/************************************************************************************************************/

// 108 - BIT CARRY LOOK ADDER FUNC
function Bit#(108) fn_CLA_dp_new(Bit#(1) sub,Bit#(108) sfdX,Bit#(108) sfdY1);

   Bit#(1) cin = (sub == 0)?0:1;                //subtraction part
   Bit#(108) sfdY = (sub==0)?sfdY1:~(sfdY1);

   Bit#(108) gen_carry = sfdX&sfdY;              //carry generation
   Bit#(108) prop_carry = sfdX|sfdY;             //carry propagation
   
   Vector#(108, Bit#(1)) gen_c = unpack(gen_carry);
   Vector#(108, Bit#(1)) prop_c = unpack(prop_carry);
   
   Vector#(8, Bit#(1)) act_c1 ;
   Vector#(8, Bit#(1)) act_c2_1 ;
   Vector#(8, Bit#(1)) act_c3_1 ;
   Vector#(8, Bit#(1)) act_c4_1 ;
   Vector#(8, Bit#(1)) act_c5_1 ;
   Vector#(8, Bit#(1)) act_c6_1 ;
   Vector#(8, Bit#(1)) act_c7_1 ;
   Vector#(8, Bit#(1)) act_c8_1 ;
   Vector#(8, Bit#(1)) act_c9_1 ;
   Vector#(8, Bit#(1)) act_c10_1 ;
   Vector#(8, Bit#(1)) act_c11_1 ;
   Vector#(8, Bit#(1)) act_c12_1 ;
   Vector#(8, Bit#(1)) act_c13_1 ;
   Vector#(8, Bit#(1)) act_c14_1 ;
   Vector#(8, Bit#(1)) act_c15_1 ;
   Vector#(4, Bit#(1)) act_c16_1 ;

   Vector#(8, Bit#(1)) act_c2_0 ;
   Vector#(8, Bit#(1)) act_c3_0 ;
   Vector#(8, Bit#(1)) act_c4_0 ;
   Vector#(8, Bit#(1)) act_c5_0 ;
   Vector#(8, Bit#(1)) act_c6_0 ;
   Vector#(8, Bit#(1)) act_c7_0 ;
   Vector#(8, Bit#(1)) act_c8_0 ;
   Vector#(8, Bit#(1)) act_c9_0 ;
   Vector#(8, Bit#(1)) act_c10_0 ;
   Vector#(8, Bit#(1)) act_c11_0 ;
   Vector#(8, Bit#(1)) act_c12_0 ;
   Vector#(8, Bit#(1)) act_c13_0 ;
   Vector#(8, Bit#(1)) act_c14_0 ;
   Vector#(8, Bit#(1)) act_c15_0 ;
   Vector#(4, Bit#(1)) act_c16_0 ;  
   
   act_c1[0] = cin;
   act_c2_0[0] = 1'b0;
   act_c3_0[0] = 1'b0;
   act_c4_0[0] = 1'b0;
   act_c5_0[0] = 1'b0;
   act_c6_0[0] = 1'b0;
   act_c7_0[0] = 1'b0;
   act_c8_0[0] = 1'b0;
   act_c9_0[0] = 1'b0;
   act_c10_0[0] = 1'b0;
   act_c11_0[0] = 1'b0;
   act_c12_0[0] = 1'b0;
   act_c13_0[0] = 1'b0;
   act_c14_0[0] = 1'b0;
   act_c15_0[0] = 1'b0;
   act_c16_0[0] = 1'b0;

   act_c2_1[0] = 1'b1; 
   act_c3_1[0] = 1'b1;
   act_c4_1[0] = 1'b1;
   act_c5_1[0] = 1'b1;
   act_c6_1[0] = 1'b1;
   act_c7_1[0] = 1'b1;
   act_c8_1[0] = 1'b1;
   act_c9_1[0] = 1'b1;
   act_c10_1[0] = 1'b1;
   act_c11_1[0] = 1'b1;
   act_c12_1[0] = 1'b1;
   act_c13_1[0] = 1'b1;
   act_c14_1[0] = 1'b1;
   act_c15_1[0] = 1'b1;
   act_c16_1[0] = 1'b1;
   
   for ( Integer k=1;k<8;k=k+1) begin
      act_c1[k]    = gen_c[k-1]  | (prop_c[k-1]&act_c1[k-1]);      // carry generation,1st segment
      act_c2_0[k]  = gen_c[k+6]  | (prop_c[k+6]&act_c2_0[k-1]);    // carry generation for c_in=0,2nd segment
      act_c2_1[k]  = gen_c[k+6]  | (prop_c[k+6]&act_c2_1[k-1]);    // carry generation for c_in=1,2nd segment  
      act_c3_0[k]  = gen_c[k+13] | (prop_c[k+13]&act_c3_0[k-1]);   // carry generation for c_in=0,3rd segment
      act_c3_1[k]  = gen_c[k+13] | (prop_c[k+13]&act_c3_1[k-1]);   // carry generation for c_in=1,3rd segment
      act_c4_0[k]  = gen_c[k+20] | (prop_c[k+20]&act_c4_0[k-1]);   // carry generation for c_in=0,4th segment
      act_c4_1[k]  = gen_c[k+20] | (prop_c[k+20]&act_c4_1[k-1]);   // carry generation for c_in=1,4th segment
      act_c5_0[k]  = gen_c[k+27] | (prop_c[k+27]&act_c5_0[k-1]);   // carry generation for c_in=0,5th segment
      act_c5_1[k]  = gen_c[k+27] | (prop_c[k+27]&act_c5_1[k-1]);   // carry generation for c_in=1,5th segment
      act_c6_0[k]  = gen_c[k+34] | (prop_c[k+34]&act_c6_0[k-1]);   // carry generation for c_in=0,6th segment  
      act_c6_1[k]  = gen_c[k+34] | (prop_c[k+34]&act_c6_1[k-1]);   // carry generation for c_in=1,6th segment  
      act_c7_0[k]  = gen_c[k+41] | (prop_c[k+41]&act_c7_0[k-1]);   // carry generation for c_in=0,7th segment  
      act_c7_1[k]  = gen_c[k+41] | (prop_c[k+41]&act_c7_1[k-1]);   // carry generation for c_in=1,7th segment
      act_c8_0[k]  = gen_c[k+48] | (prop_c[k+48]&act_c8_0[k-1]);   // carry generation for c_in=0,8th segment  
      act_c8_1[k]  = gen_c[k+48] | (prop_c[k+48]&act_c8_1[k-1]);   // carry generation for c_in=1,8th segment
      act_c9_0[k]  = gen_c[k+55] | (prop_c[k+55]&act_c9_0[k-1]);   // carry generation for c_in=0,9th segment  
      act_c9_1[k]  = gen_c[k+55] | (prop_c[k+55]&act_c9_1[k-1]);   // carry generation for c_in=1,9th segment
      act_c10_0[k] = gen_c[k+62] | (prop_c[k+62]&act_c10_0[k-1]);   // carry generation for c_in=0,10th segment  
      act_c10_1[k] = gen_c[k+62] | (prop_c[k+62]&act_c10_1[k-1]);   // carry generation for c_in=1,10th segment
      act_c11_0[k] = gen_c[k+69] | (prop_c[k+69]&act_c11_0[k-1]);   // carry generation for c_in=0,11th segment  
      act_c11_1[k] = gen_c[k+69] | (prop_c[k+69]&act_c11_1[k-1]);   // carry generation for c_in=1,11th segment
      act_c12_0[k] = gen_c[k+76] | (prop_c[k+76]&act_c12_0[k-1]);   // carry generation for c_in=0,12th segment  
      act_c12_1[k] = gen_c[k+76] | (prop_c[k+76]&act_c12_1[k-1]);   // carry generation for c_in=1,12th segment
      act_c13_0[k] = gen_c[k+83] | (prop_c[k+83]&act_c13_0[k-1]);   // carry generation for c_in=0,13th segment  
      act_c13_1[k] = gen_c[k+83] | (prop_c[k+83]&act_c13_1[k-1]);   // carry generation for c_in=1,13th segment
      act_c14_0[k] = gen_c[k+90] | (prop_c[k+90]&act_c14_0[k-1]);   // carry generation for c_in=0,14th segment  
      act_c14_1[k] = gen_c[k+90] | (prop_c[k+90]&act_c14_1[k-1]);   // carry generation for c_in=1,14th segment
      act_c15_0[k] = gen_c[k+97] | (prop_c[k+97]&act_c15_0[k-1]);   // carry generation for c_in=0,15th segment  
      act_c15_1[k] = gen_c[k+97] | (prop_c[k+97]&act_c15_1[k-1]);   // carry generation for c_in=1,15th segment
   end
   
   for ( Integer k=1;k<4;k=k+1) begin
      act_c16_0[k] = gen_c[k+104] | (prop_c[k+104]&act_c16_0[k-1]);   // carry generation for c_in=0,16th segment  
      act_c16_1[k] = gen_c[k+104] | (prop_c[k+104]&act_c16_1[k-1]);   // carry generation for c_in=1,16th segment
   end  
   
   Bit#(8) carry1 = pack(act_c1);
   Bit#(8) carry2_1 = pack(act_c2_1);
   Bit#(8) carry2_0 = pack(act_c2_0);
   Bit#(8) carry3_0 = pack(act_c3_0);
   Bit#(8) carry3_1 = pack(act_c3_1);
   Bit#(8) carry4_1 = pack(act_c4_1);
   Bit#(8) carry4_0 = pack(act_c4_0);
   Bit#(8) carry5_1 = pack(act_c5_1);
   Bit#(8) carry5_0 = pack(act_c5_0);
   Bit#(8) carry6_1 = pack(act_c6_1);
   Bit#(8) carry6_0 = pack(act_c6_0);
   Bit#(8) carry7_1 = pack(act_c7_1);
   Bit#(8) carry7_0 = pack(act_c7_0);  
   Bit#(8) carry8_1 = pack(act_c8_1);
   Bit#(8) carry8_0 = pack(act_c8_0); 
   Bit#(8) carry9_1 = pack(act_c9_1);
   Bit#(8) carry9_0 = pack(act_c9_0); 
   Bit#(8) carry10_1 = pack(act_c10_1);
   Bit#(8) carry10_0 = pack(act_c10_0); 
   Bit#(8) carry11_1 = pack(act_c11_1);
   Bit#(8) carry11_0 = pack(act_c11_0); 
   Bit#(8) carry12_1 = pack(act_c12_1);
   Bit#(8) carry12_0 = pack(act_c12_0); 
   Bit#(8) carry13_1 = pack(act_c13_1);
   Bit#(8) carry13_0 = pack(act_c13_0); 
   Bit#(8) carry14_1 = pack(act_c14_1);
   Bit#(8) carry14_0 = pack(act_c14_0);
   Bit#(8) carry15_1 = pack(act_c15_1);
   Bit#(8) carry15_0 = pack(act_c15_0);
   Bit#(4) carry16_1 = pack(act_c16_1);
   Bit#(4) carry16_0 = pack(act_c16_0);

   //choosing the carry segments according to actual c_in
   Bit#(8) carry2  = (carry1 [7]==1'b1)?carry2_1:carry2_0;
   Bit#(8) carry3  = (carry2 [7]==1'b1)?carry3_1:carry3_0;
   Bit#(8) carry4  = (carry3 [7]==1'b1)?carry4_1:carry4_0;
   Bit#(8) carry5  = (carry4 [7]==1'b1)?carry5_1:carry5_0;
   Bit#(8) carry6  = (carry5 [7]==1'b1)?carry6_1:carry6_0;
   Bit#(8) carry7  = (carry6 [7]==1'b1)?carry7_1:carry7_0;
   Bit#(8) carry8  = (carry7 [7]==1'b1)?carry8_1:carry8_0;
   Bit#(8) carry9  = (carry8 [7]==1'b1)?carry9_1:carry9_0;
   Bit#(8) carry10 = (carry9 [7]==1'b1)?carry10_1:carry10_0;
   Bit#(8) carry11 = (carry10[7]==1'b1)?carry11_1:carry11_0;
   Bit#(8) carry12 = (carry11[7]==1'b1)?carry12_1:carry12_0;
   Bit#(8) carry13 = (carry12[7]==1'b1)?carry13_1:carry13_0;
   Bit#(8) carry14 = (carry13[7]==1'b1)?carry14_1:carry14_0;
   Bit#(8) carry15 = (carry14[7]==1'b1)?carry15_1:carry15_0;
   Bit#(4) carry16 = (carry15[7]==1'b1)?carry16_1:carry16_0;

   //generating actual carry vector
   Bit#(108) carry = {carry16[2:0],carry15[6:0],carry14[6:0],carry13[6:0],carry12[6:0],carry11[6:0],carry10[6:0],carry9[6:0],carry8[6:0],carry7[6:0],carry6[6:0],carry5[6:0],carry4[6:0],carry3[6:0],carry2[6:0],carry1[6:0]}; 
   
   //Actual sum generation
   Bit#(108) sum = sfdX^sfdY^carry;

   return sum;
endfunction
/************************************************************************************************************/

////////////////////////////////////////////////////////////////////////////////
/// SINGLE Floating point fused multiple accumulate
////////////////////////////////////////////////////////////////////////////////

interface Ifc_fpu_fma_sp;
     //doc:method:send method for receiving inputs in sp.
     method Action send(Tuple4#(FloatingPoint#(8,23),FloatingPoint#(8,23),Maybe#(FloatingPoint#(8,23)),RoundMode) operands);
     //doc:method:receive method for returning result in sp.
     method ReturnType#(8,23) receive();
endinterface
/************************************************************************************************************/

(*synthesize*)
//doc:module:this module implements send and receive method in sp
module mk_fpu_fma_sp(Ifc_fpu_fma_sp);

   Vector#(`STAGES_FMA_SP,Reg#(Tuple2#(FloatingPoint#(8,23),Exception))) rg_stage_out <- replicateM(mkReg(tuple2(unpack(0),unpack(0))));
   Vector#(`STAGES_FMA_SP,Reg#(Bit#(1))) rg_stage_valid <- replicateM(mkDReg(0));
   Wire#(Tuple4#(FloatingPoint#(8,23),FloatingPoint#(8,23),Maybe#(FloatingPoint#(8,23)),RoundMode)) wr_operands <- mkDWire(tuple4(unpack(0), unpack(0), unpack(0), unpack(0)));
   Wire#(Tuple2#(FloatingPoint#(8,23),Exception)) wr_returnvalues <- mkDWire(tuple2(unpack(0), unpack(0)));


   //REG BTW STAGE1 -> STAGE2
   Reg#(Bit#(24)) rg_sfdA_1 <- mkReg(0);
   Reg#(Bit#(24)) rg_sfdB_1 <- mkReg(0);
   Reg#(Int#(10)) rg_expAB_1 <- mkReg(0);
   Reg#(Bool) rg_sgnAB_1 <- mkReg(False);
   Reg#(Bool) rg_zeroC_1 <- mkReg(False);
   Reg#(Bool)   rg_acc_1 <- mkReg(False);
   Reg#(FloatingPoint#(8, 23)) rg_opA_1 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opC_1 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opB_1 <- mkReg(0);
   Reg#(CommonState#(8,23)) rg_s_1 <- mkReg(unpack(0));


   //REG BTW STAGE2 -> STAGE3
   Reg#(Bit#(24)) rg_sfdA_2  <- mkReg(0);
   Reg#(Bit#(24)) rg_sfdB_2  <- mkReg(0);
   Reg#(Int#(10)) rg_expAB_2 <- mkReg(0);
   Reg#(Bool)   rg_acc_2 <- mkReg(False);
   Reg#(Bool) rg_sgnAB_2 <- mkReg(False);
   Reg#(Bool) rg_zeroC_2 <- mkReg(False);
   Reg#(FloatingPoint#(8, 23)) rg_opA_2 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opB_2 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opC_2 <- mkReg(0);
   Reg#(Bit#(48)) rg_partial_product0_2 <- mkReg(0);
   Reg#(Bit#(48)) rg_partial_product1_2 <- mkReg(0);
   Reg#(Bit#(48)) rg_partial_product2_2 <- mkReg(0);
   Reg#(Bit#(48)) rg_partial_product3_2 <- mkReg(0);
   Reg#(CommonState#(8,23)) rg_s_2 <- mkReg(unpack(0));
   Reg#(Tuple4#(Bit#(8),Int#(32), Bit#(5),Bit#(2))) rg_e0<-mkReg(tuple4(0,0,0,0));


   //REG BTW STAGE3 -> STAGE4
   Reg#(Bit#(24)) rg_sfdA_3 <- mkReg(0);
   Reg#(Bit#(24)) rg_sfdB_3 <- mkReg(0);
   Reg#(Int#(10)) rg_expAB_3 <- mkReg(0);
   Reg#(Bit#(48)) rg_sfdAB_3 <- mkReg(0);
   Reg#(Bool)   rg_acc_3 <- mkReg(False);
   Reg#(Bool) rg_zeroC_3 <- mkReg(False);
   Reg#(Bool) rg_sgnAB_3 <- mkReg(False);
   Reg#(FloatingPoint#(8, 23)) rg_opA_3 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opB_3 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opC_3 <- mkReg(0);
   Reg#(CommonState#(8,23)) rg_s_3 <- mkReg(unpack(0));


   //REG BTW STAGE4 -> STAGE5
   Reg#(Bit#(50)) rg_sfdAb_4 <- mkReg(0);
   Reg#(Int#(10)) rg_expAB_4 <- mkReg(0);
   Reg#(UInt#(6)) rg_zeros_4 <- mkReg(0);
   Reg#(Bit#(48)) rg_sfdAB_4 <- mkReg(0);
   Reg#(Bool)    rg_acc_4 <- mkReg(False);
   Reg#(Bool)  rg_zeroC_4 <- mkReg(False);
   Reg#(Bool) rg_bigEXP_4 <- mkReg(False);
   Reg#(Bool) rg_sgnAB_4  <- mkReg(False);
   Reg#(Bool) rg_flag_2_4 <- mkReg(False);
   Reg#(Bool) rg_flag_3_4 <- mkReg(False);
   Reg#(FloatingPoint#(8, 23))  rg_ab_4 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opC_4 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opA_4 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opB_4 <- mkReg(0);
   Reg#(CommonState#(8,23)) rg_s_4 <- mkReg(unpack(0));


   //REG BTW STAGE5 -> STAGE6
   Reg#(Bool) rg_sgnAB_5 <- mkReg(False);
   Reg#(Bit#(50)) rg_sfdAb_5 <- mkReg(0);
   Reg#(Int#(10)) rg_expAB_5 <- mkReg(0);
   Reg#(Bool)    rg_acc_5 <- mkReg(False);
   Reg#(Bool)  rg_zeroC_5 <- mkReg(False);
   Reg#(Bool) rg_bigEXP_5 <- mkReg(False);
   Reg#(FloatingPoint#(8, 23))  rg_ab_5 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opC_5 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opA_5 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opB_5 <- mkReg(0);
   Reg#(CommonState#(8,23)) rg_s_5 <- mkReg(unpack(0));


   //REG BTW STAGE6 -> STAGE7    
   Reg#(Bool) rg_sgn_6 <- mkReg(False);
   Reg#(Bool) rg_sub_6 <- mkReg(False);
   Reg#(Bit#(50))  rg_sfdX_6 <- mkReg(0);
   Reg#(Bit#(50))  rg_sfdY_6 <- mkReg(0);
   Reg#(Bit#(50)) rg_sfdAb_6 <- mkReg(0);
   Reg#(Int#(10)) rg_expAB_6 <- mkReg(0);
   Reg#(Int#(10))   rg_exp_6 <- mkReg(0);
   Reg#(Bool)    rg_acc_6 <- mkReg(False);
   Reg#(Bool)  rg_zeroC_6 <- mkReg(False);
   Reg#(Bool) rg_bigEXP_6 <- mkReg(False);
   Reg#(FloatingPoint#(8, 23))  rg_ab_6 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opC_6 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opA_6 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opB_6 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_out_6 <- mkReg(0);
   Reg#(CommonState#(8,23)) rg_s_6 <- mkReg(unpack(0));


   //REG BTW STAGE7 -> STAGE8
   Reg#(Bool) rg_sgn_7    <- mkReg(False);
   Reg#(Bool) rg_acc_7    <- mkReg(False);
   Reg#(Bool) rg_zeroC_7  <- mkReg(False);
   Reg#(Bool) rg_bigEXP_7 <- mkReg(False);
   Reg#(Bit#(50)) rg_sfdAb_7 <- mkReg(0);
   Reg#(Int#(10)) rg_expAB_7 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opC_7 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opA_7 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opB_7 <- mkReg(0);
   Reg#(CommonState#(8,23)) rg_s_7 <- mkReg(unpack(0));
   Reg#(FloatingPoint#(8,23)) rg_ab_7 <- mkReg(0);
   Reg#(Int#(10)) rg_exp_7 <- mkReg(0);
   Reg#(Bit#(50)) rg_sfdX_7 <- mkReg(0);
   Reg#(Bit#(50)) rg_sfdY_7 <- mkReg(0);
   Reg#(Bool) rg_sub_7 <- mkReg(False);
   Reg#(FloatingPoint#(8, 23)) rg_out_7 <- mkReg(0);   
   Reg#(Bit#(50)) rg_sfd_7 <- mkReg(0);


   //REG BTW STAGE8 -> STAGE9
   Reg#(CommonState#(8,23)) rg_s2_8 <- mkReg(unpack(0));
   Reg#(FloatingPoint#(8, 23)) rg_out_8 <- mkReg(0);
   Reg#(Bit#(3)) rg_guard_8 <- mkReg(0);
   Reg#(Bool) rg_acc2_8 <- mkReg(False);
   Reg#(FloatingPoint#(8,23)) rg_ab1_8 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opA2_8 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opB2_8 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opC2_8 <- mkReg(0);
   Reg#(Bool) rg_sub_8 <- mkReg(False);
   Reg#(Int#(10)) rg_expAB2_8 <- mkReg(0);
   Reg#(UInt#(6)) rg_zeros_8 <- mkReg(0);
   Reg#(Int#(10)) rg_exp_8 <- mkReg(0);
   Reg#(Bit#(50)) rg_sfd_8 <- mkReg(0);
   Reg#(Bool) rg_bigExp_8 <- mkReg(False);


   //REG BTW STAGE9 -> Receive
   Reg#(CommonState#(8,23)) rg_s2_9 <- mkReg(unpack(0));
   Reg#(FloatingPoint#(8, 23)) rg_out_9 <- mkReg(0);
   Reg#(Bit#(3)) rg_guard_9 <- mkReg(0);
   Reg#(Bool) rg_acc2_9 <- mkReg(False);
   Reg#(FloatingPoint#(8,23)) rg_ab1_9 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opA2_9 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opB2_9 <- mkReg(0);
   Reg#(FloatingPoint#(8, 23)) rg_opC2_9 <- mkReg(0);
   Reg#(Bool) rg_sub_9 <- mkReg(False);
   Reg#(Int#(10)) rg_expAB2_9 <- mkReg(0);
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_1; 
      match {.opA, .opB, .mopC, .rmode } =  wr_operands;
      CommonState#(8,23) s = CommonState {res: tagged Invalid, exc: defaultValue, rmode: rmode};
      Bool acc = False;
      FloatingPoint#(8,23) opC = 0;

      if (mopC matches tagged Valid .opC_) begin
         opC = opC_;
         acc = True;
      end
    
      //zeroC indicates whether operand C is zero or not
      Bool zeroC = (opC.exp == 0 && opC.sfd == 0);
      //If number is subnormal, then exponent will become minimum exponent, it is -126 for sp and -1022 for dp
      //If number is not subnormal, then unbias it, bias is 127 for sp and 1023 for dp
      Int#(10) expA = isSubNormal(opA) ? fromInteger(minexp(opA)) : signExtend(unpack(unbias(opA)));
      Int#(10) expB = isSubNormal(opB) ? fromInteger(minexp(opB)) : signExtend(unpack(unbias(opB)));

      //significand is simply taken from sfd part of operand. Hidden bit is either 0 or 1, it is 0 for subnormal number and 1 for normal number subnormal number have exponent as zero.
      Bit#(24) sfdA = { getHiddenBit(opA), opA.sfd };
      Bit#(24) sfdB = { getHiddenBit(opB), opB.sfd };
   
      
      Bool sgnAB = opA.sign != opB.sign;                 //sgnAB maintains sign of product      
      Bool infAB = isInfinity(opA) || isInfinity(opB);   //infAB indicates whether result of product is infinity or not
      Bool zeroAB = isZero(opA) || isZero(opB);          //zeroAB indicates product is zero or not.

      // OpA*OpB - add exponents of both the operands and multiplies their mantissas 
      Int#(10) expAB = expA + expB;

      if(((isZero(opA) && isInfinity(opB)) || (isZero(opB) && isInfinity(opA)) ) && isNaN(opC))
         s.exc.invalid_op = True;

      //if opC is signaling NaN, its quiet NaN is the result
      if (isSNaN(opC)) begin
         s.res = tagged Valid nanQuiet(opC);
         s.exc.invalid_op = True;
      end
      
      //if opA is signaling NaN, it's quiet NaN is the result 
      else if (isSNaN(opA)) begin
         s.res = tagged Valid nanQuiet(opA);
         s.exc.invalid_op = True;
      end

      //if opB is signaling NaN, it's Quiet NaN is the result
      else if (isSNaN(opB)) begin
         s.res = tagged Valid nanQuiet(opB);
         s.exc.invalid_op = True;
      end
      
      //When opA, opB or opC is quiet NaN, the invalid flag will not be set
      else if (isQNaN(opC)) begin
         s.res = tagged Valid opC;
      end

      else if (isQNaN(opA)) begin
         s.res = tagged Valid opA;
      end

      else if (isQNaN(opB)) begin
         s.res = tagged Valid opB;
      end

      else if ((isInfinity(opA) && isZero(opB)) || (isZero(opA) && isInfinity(opB)) || (isInfinity(opC) && infAB && (opC.sign != sgnAB))) begin
         s.res = tagged Valid qnan();
         s.exc.invalid_op = True;
      end
      
      //if opC is infinity, it is final result
      else if (isInfinity(opC)) begin
         s.res = tagged Valid opC;
      end
      
      //if opA or opB is infinity, the result is infinity
      else if (infAB) begin
         s.res = tagged Valid infinity(sgnAB);
      end
      
      //if opC and product is zero, the result is also zero
      else if (isZero(opC) && zeroAB && (opC.sign == sgnAB)) begin
         s.res = tagged Valid opC;
      end
      
      rg_sfdA_1 <= sfdA;
      rg_sfdB_1 <= sfdB;
      rg_s_1 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_expAB_1 <= expAB;
      rg_sgnAB_1 <= sgnAB;
      rg_opC_1 <= opC;
      rg_opA_1 <= opA;
      rg_opB_1 <= opB;
      rg_acc_1 <= acc;
      rg_zeroC_1 <= zeroC;
      rg_stage_valid[0] <= 1;
   endrule // STAGE 1
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_2;

      Bit#(24) sfdA = rg_sfdA_1 ;
      Bit#(24) sfdB = rg_sfdB_1 ;
      CommonState#(8,23) s = rg_s_1; 
      Int#(10) expAB = rg_expAB_1;
      Bool sgnAB = rg_sgnAB_1;
      FloatingPoint#(8, 23) opC = rg_opC_1;
      FloatingPoint#(8, 23) opB = rg_opB_1;
      FloatingPoint#(8, 23) opA = rg_opA_1;
      Bool acc = rg_acc_1;
      Bool zeroC = rg_zeroC_1;

      Bit#(8) expA1 = opA.exp;
      Bit#(8) expB1 = opB.exp;
      Bit#(23) sfdA1 = opA.sfd;
      Bit#(23) sfdB1 = opB.sfd;

      // GENERATING PARTIAL PRODUCTS
      let  pp0 = fn_gen_pp_sp({(|expA1),sfdA1},sfdB1[5:0]);
      let  pp1 = {fn_gen_pp_sp({(|expA1),sfdA1},sfdB1[11:6])[41:0],6'd0};
      let  pp2 = {fn_gen_pp_sp({(|expA1),sfdA1},sfdB1[17:12])[35:0],12'd0};
      let  pp3 = {fn_gen_pp_sp({(|expA1),sfdA1},{(|expB1),sfdB1[22:18]})[29:0],18'd0};  

      //rg_e0 <= fn_find_exp_sp(expA1,expB1,sfdA1,sfdB1); 
      rg_partial_product0_2 <= pp0;
      rg_partial_product1_2 <= pp1;
      rg_partial_product2_2 <= pp2;
      rg_partial_product3_2 <= pp3;

      rg_sfdA_2 <= sfdA;
      rg_sfdB_2 <= sfdB;
      rg_s_2 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_expAB_2 <= expAB;
      rg_sgnAB_2 <= sgnAB;
      rg_opC_2 <= opC;
      rg_opA_2 <= opA;
      rg_opB_2 <= opB;
      rg_acc_2 <= acc;
      rg_zeroC_2 <= zeroC;
      rg_stage_valid[1] <= rg_stage_valid[0];
   endrule // STAGE 2
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_3;

      Bit#(24) sfdA = rg_sfdA_2 ;
      Bit#(24) sfdB = rg_sfdB_2 ;
      CommonState#(8,23) s = rg_s_2; 
      Int#(10) expAB = rg_expAB_2;
      Bool sgnAB = rg_sgnAB_2;
      FloatingPoint#(8, 23) opC = rg_opC_2;
      FloatingPoint#(8, 23) opB = rg_opB_2;
      FloatingPoint#(8, 23) opA = rg_opA_2;
      Bool acc = rg_acc_2;
      Bool zeroC = rg_zeroC_2;

      Bit#(48) partial_product0 = rg_partial_product0_2;
      Bit#(48) partial_product1 = rg_partial_product1_2;
      Bit#(48) partial_product2 = rg_partial_product2_2;
      Bit#(48) partial_product3 = rg_partial_product3_2;

      //sfdAB holds the result of multiplication of significand of opA and significand of opB
      //Adding up all Partial Products
      Bit#(48) sfdAB = partial_product0+partial_product1+partial_product2+partial_product3; 

      rg_sfdA_3 <= sfdA;
      rg_sfdB_3 <= sfdB;
      rg_s_3 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_expAB_3 <= expAB;
      rg_sgnAB_3 <= sgnAB;
      rg_opC_3 <= opC;
      rg_opA_3 <= opA;
      rg_opB_3 <= opB;
      rg_acc_3 <= acc;
      rg_zeroC_3 <= zeroC;
      rg_sfdAB_3 <= sfdAB;
      rg_stage_valid[2] <= rg_stage_valid[1];
   endrule // STAGE 3
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_4;

      Bit#(24) sfdA = rg_sfdA_3 ;
      Bit#(24) sfdB = rg_sfdB_3 ;
      CommonState#(8,23) s = rg_s_3; 
      Int#(10) expAB = rg_expAB_3;
      Bool sgnAB = rg_sgnAB_3;
      FloatingPoint#(8, 23) opC = rg_opC_3;
      FloatingPoint#(8, 23) opB = rg_opB_3;
      FloatingPoint#(8, 23) opA = rg_opA_3;
      Bool acc = rg_acc_3;
      Bool zeroC = rg_zeroC_3;
      Bit#(48) sfdAB = rg_sfdAB_3;
      Bit#(1) ab12 =0;

      Bit#(50) sfdAb = 0;
      Bool bigEXP = False;   
      FloatingPoint#(8,23) ab = defaultValue;
      Bool flag_2 = expAB > fromInteger(maxexp(ab));
      Bool flag_3 = expAB < (fromInteger(minexp_subnormal(ab))-2);
      let shift = fromInteger(minexp(ab)) - expAB;
      Bit#(1) bit_flag = |sfdAB;
      UInt#(6) zeros =0;

      // s.res is invalid -> Not a special case  
      if (s.res matches tagged Invalid) begin

         //if product exponent is greater than max exponent(127 for sp and 1023 for dp), normalization need not be done
         //product exponent and product mantissa remains as it is
         if (flag_2) begin
            ab.sign = sgnAB;
            ab.exp = maxBound - 1;
            ab.sfd = maxBound;
            sfdAb = {sfdAB,2'b00};
         end 

         //checking for minimum possible exponent, checks if the exponent cannot be in a valid range after a shift  
         else begin
            if (flag_3) begin
               ab.sign = sgnAB;
               ab.exp = 0;
               ab.sfd = 0;
               sfdAb = {sfdAB,2'b00};
               bigEXP = (bit_flag == 1)? True: False;
            end
         end
      end

      if (s.res matches tagged Invalid) begin
         if(!flag_2 && !flag_3) begin
            //shift > 0 indicates expAB is smaller than minimum exponent. If true, set expAB as minimum possible exponent.
            if (shift > 0) begin
               //subnormal
               `ifdef denormal_support
                  Bit#(1) sfdlsb = |(sfdAB << (fromInteger(valueOf(48)) - shift));
                  sfdAB = sfdAB >> shift;
                  sfdAB[0] = sfdAB[0] | sfdlsb;
                  expAB = 'd-126;
               `else
                  ab.sfd = 0;
                  sfdAB = 0;
               `endif
               ab.exp = 0;
            end
            //simply add bias in product exponent 
            else begin
               ab.exp = cExtend(expAB + fromInteger(bias(ab)));
            end
            ab.sign = sgnAB;
            expAB = (bit_flag == 0)? 'd-127: expAB;
            //Normalization 1 Part 1
            zeros = normalize_sp_part_1(sfdAB);
         end
      end

      rg_sgnAB_4 <= sgnAB;
      rg_sfdAb_4 <= sfdAb;
      rg_expAB_4 <= expAB;
      rg_opC_4 <= opC;
      rg_opA_4 <= opA;
      rg_opB_4 <= opB;
      rg_s_4 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_ab_4 <= ab;
      rg_acc_4 <= acc;
      rg_zeroC_4 <= zeroC;
      rg_bigEXP_4 <= bigEXP;
      rg_sfdAB_4 <= sfdAB;
      rg_zeros_4 <= zeros;
      rg_flag_2_4 <= flag_2;
      rg_flag_3_4 <= flag_3;
      rg_stage_valid[3] <= rg_stage_valid[2];
   endrule // STAGE 4
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_5;

      Bool sgnAB = rg_sgnAB_4;
      Bit#(50) sfdAb = rg_sfdAb_4;
      Int#(10) expAB = rg_expAB_4;
      FloatingPoint#(8, 23) opC = rg_opC_4 ;
      FloatingPoint#(8, 23) opA = rg_opA_4 ;
      FloatingPoint#(8, 23) opB = rg_opB_4 ;
      CommonState#(8,23) s = rg_s_4; 
      FloatingPoint#(8, 23) ab = rg_ab_4 ;
      Bool acc = rg_acc_4;
      Bool zeroC = rg_zeroC_4;
      Bool bigEXP = rg_bigEXP_4;

      Bit#(48) sfdAB = rg_sfdAB_4;
      UInt#(6) zeros = rg_zeros_4;
      Exception exc = defaultValue;
      Bool flag_2 = rg_flag_2_4;
      Bool flag_3 = rg_flag_3_4;
   
      if (s.res matches tagged Invalid) begin
         if(!flag_2 && !flag_3) begin
            let y1 = normalize_sp_part_2(ab, sfdAB, zeros);
            //Normalization1 sp part 2 function gives exception, normalized result. 
            ab = tpl_1(y1);
            s.exc = s.exc | tpl_2(y1);
            sfdAb = tpl_3(y1);
            expAB = isSubNormal(ab) ? fromInteger(minexp(ab)) : signExtend(unpack(unbias(ab)));
         end
      end

      rg_sgnAB_5 <= sgnAB;
      rg_sfdAb_5 <= sfdAb;
      rg_expAB_5 <= expAB;
      rg_opC_5 <= opC;
      rg_opA_5 <= opA;
      rg_opB_5 <= opB;
      rg_s_5 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_ab_5 <= ab;
      rg_acc_5 <= acc;
      rg_zeroC_5 <= zeroC;
      rg_bigEXP_5 <= bigEXP;
      rg_stage_valid[4] <=  rg_stage_valid[3];
   endrule // STAGE 5
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_6;

      Bool sgnAB = rg_sgnAB_5;
      Bit#(50) sfdAb = rg_sfdAb_5;
      Int#(10) expAB = rg_expAB_5;
      FloatingPoint#(8, 23) opC = rg_opC_5 ;
      FloatingPoint#(8, 23) opA = rg_opA_5 ;
      FloatingPoint#(8, 23) opB = rg_opB_5 ;
      CommonState#(8,23) s = rg_s_5; 
      FloatingPoint#(8, 23) ab = rg_ab_5 ;
      Bool acc = rg_acc_5;
      Bool zeroC = rg_zeroC_5;
      Bool bigEXP = rg_bigEXP_5;


      //compute expC similar to expA,expB
      Int#(10) expC = isSubNormal(opC) ? fromInteger(minexp(opC)) : signExtend(unpack(unbias(opC)));
      `ifdef denormal_support
         opC.sfd = opC.sfd;
      `else
         if (isSubNormal(opC))
            opC.sfd = 0;
      `endif

      //compute the significand of opC
      Bit#(50) sfdC = {1'b0,getHiddenBit(opC), opC.sfd, 25'b0}; 
      Bool sub = opC.sign != ab.sign; //opC should be added or subtracted
      Bit#(1) subflag = sub ? 1:0;
      Int#(10) exp = ?;
      Int#(10) shift = ?;
      Bit#(50) x  = ?;
      Bit#(50) y  = ?;   
      Bool sgn = ?;
      FloatingPoint#(8,23) out = defaultValue;

      //Smaller operand among opAB and opC will be y and larger will be x
      //later x+y and x-y is performed for final result 
      
      if ((!acc) || (expAB > expC) || ((expAB == expC) && (sfdAb > sfdC))) begin
         exp = expAB;
         shift = expAB - expC;
         x = sfdAb;
         y = sfdC;
         sgn = ab.sign;
      end

      else begin
         exp = expC;
         shift = expC - expAB;
         x = sfdC;
         y = sfdAb;
         sgn = opC.sign;
      end      
   
      //y is shifted to make exponent of opAB and opC equal
      if (s.res matches tagged Invalid) begin
         if (shift < fromInteger(50)) begin
            Bit#(50) guard = 0;
            guard = y << ((50) - shift);
            y = y >> shift;
            y[0] = y[0] | (|guard);
         end
         else if (|y == 1) begin
            y = 1;
         end
      end

      rg_sgn_6 <= sgn;
      rg_sfdAb_6 <= sfdAb;
      rg_expAB_6 <= expAB;
      rg_opC_6 <= opC;
      rg_opA_6 <= opA;
      rg_opB_6 <= opB;
      rg_s_6 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_ab_6 <= ab;
      rg_acc_6 <= acc;
      rg_zeroC_6 <= zeroC;
      rg_bigEXP_6 <= bigEXP;
      rg_exp_6 <= exp;
      rg_sfdX_6 <= x;
      rg_sfdY_6 <= y;
      rg_out_6 <= out;
      rg_sub_6 <= sub;
      rg_stage_valid[5] <=  rg_stage_valid[4];
   endrule // STAGE 6
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_7;

      Bool sgn = rg_sgn_6;
      Bit#(50) sfdAb = rg_sfdAb_6;
      Int#(10) expAB = rg_expAB_6;
      FloatingPoint#(8, 23) opC = rg_opC_6 ;
      FloatingPoint#(8, 23) opA = rg_opA_6 ;
      FloatingPoint#(8, 23) opB = rg_opB_6 ;
      CommonState#(8,23) s = rg_s_6; 
      FloatingPoint#(8, 23) ab = rg_ab_6 ;
      Bool acc = rg_acc_6;
      Bool zeroC = rg_zeroC_6;
      Bool bigEXP = rg_bigEXP_6;
      Int#(10) exp = rg_exp_6;
      Bit#(50) x = rg_sfdX_6;
      Bit#(50) y =rg_sfdY_6;
      FloatingPoint#(8,23) out = rg_out_6;
      Bool sub = rg_sub_6;
      Bit#(1) subflag = sub ? 1:0;

      // ADDITION USING 50-BIT CLA FUNC
      rg_sfd_7 <= fn_CLA_sp_new(subflag,x,y);
   
      rg_sgn_7 <= sgn;
      rg_sfdAb_7 <= sfdAb;
      rg_expAB_7 <= expAB;
      rg_opC_7 <= opC;
      rg_opA_7 <= opA;
      rg_opB_7 <= opB;
      rg_s_7 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_ab_7 <= ab;
      rg_acc_7 <= acc;
      rg_zeroC_7 <= zeroC;
      rg_bigEXP_7 <= bigEXP;
      rg_exp_7 <= exp;
      rg_sfdX_7 <= x;
      rg_sfdY_7 <= y;
      rg_out_7 <= out;
      rg_sub_7 <= sub;
      rg_stage_valid[6] <=  rg_stage_valid[5];
   endrule // STAGE 7
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_8;
      Bool sgn = rg_sgn_7;
      Bit#(50) sfdAb = rg_sfdAb_7;
      Int#(10) expAB = rg_expAB_7;
      FloatingPoint#(8, 23) opC = rg_opC_7 ;
      FloatingPoint#(8, 23) opA = rg_opA_7 ;
      FloatingPoint#(8, 23) opB = rg_opB_7 ;
      CommonState#(8,23) s = rg_s_7; 
      FloatingPoint#(8, 23) ab = rg_ab_7 ;
      Bool acc = rg_acc_7;
      Bool zeroC = rg_zeroC_7;
      Bool bigEXP = rg_bigEXP_7;
      Int#(10) exp = rg_exp_7;
      Bit#(50) x = rg_sfdX_7;
      Bit#(50) y =rg_sfdY_7;
      FloatingPoint#(8,23) out = rg_out_7;
      Bool sub = rg_sub_7;
      Bit#(50) sfd = rg_sfd_7;
      UInt#(6) zeros = 0;
      Bit#(3) guard = 0;

      //Checking for overflow while addition - extra carry generated implies overflow, inexact and overflow flag are set
      if((x[49] == 1 || y[49] == 1) && sfd[49] == 0 && !sub) begin
         s.exc.overflow = True;
         s.exc.inexact = True;
      end

      //final significand is either sum or difference, it is decided by value of sub
      if (s.res matches tagged Invalid) begin
         out.sign = sgn;                  
         out.exp = cExtend(exp + fromInteger(bias(out)));   //bias added in exponent
         if(zeroC) begin                                    //if opC is zero, final result is product of opA and op
            out = ab;
         end
         zeros = normalize_sp_2_part_1(sfd);     // normalization after add/sub phase NORMALIZE_2 PART  
      end

      rg_s2_8 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_out_8 <= out;
      rg_guard_8 <= guard;
      rg_ab1_8 <= ab;
      rg_acc2_8 <= acc;
      rg_opA2_8 <= opA;
      rg_opB2_8 <= opB;
      rg_opC2_8 <= opC;
      rg_sub_8 <= sub; 
      rg_expAB2_8 <= expAB;
      rg_zeros_8 <= zeros;
      rg_exp_8 <= exp;
      rg_sfd_8 <= sfd;
      rg_bigExp_8 <= bigEXP;
      rg_stage_valid[7] <=  rg_stage_valid[6];
   endrule //STAGE 8
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_9;

      CommonState#(8,23) s = rg_s2_8; 
      FloatingPoint#(8, 23) out = rg_out_8;
      Bit#(3) guard = rg_guard_8;
      FloatingPoint#(8, 23) ab = rg_ab1_8 ;
      Bool acc = rg_acc2_8;
      FloatingPoint#(8, 23) opA = rg_opA2_8;
      FloatingPoint#(8, 23) opB = rg_opB2_8;
      FloatingPoint#(8, 23) opC = rg_opC2_8;
      Bool sub = rg_sub_8;
      Int#(10) expAB = rg_expAB2_8;
      UInt#(6) zeros = rg_zeros_8;
      Bit#(50) sfd = rg_sfd_8;
      Int#(10) exp =rg_exp_8;
      Bool bigEXP = rg_bigExp_8;

      //this normalzation function gives result, exception flags and guard bit. guard bits are used for rounding. 
      if (s.res matches tagged Invalid) begin
         let norm = normalize_sp_2_part_2(zeros,exp,out, sfd,s.rmode);
         out = tpl_1(norm);
         guard = tpl_2(norm);
         s.exc = s.exc | tpl_3(norm);
         if(bigEXP)        //prevents by setting underflow flag during product phase. 
            s.exc.underflow = False;
      end   

      rg_s2_9 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_out_9 <= out;
      rg_guard_9 <= guard;
      rg_ab1_9 <= ab;
      rg_acc2_9 <= acc;
      rg_opA2_9 <= opA;
      rg_opB2_9 <= opB;
      rg_opC2_9 <= opC;
      rg_sub_9 <= sub; 
      rg_expAB2_9 <= expAB;
      rg_stage_valid[8] <= rg_stage_valid[7];
   endrule // STAGE 9
   /************************************************************************************************************/

   rule rl_receive;

      CommonState#(8,23) s = rg_s2_9; 
      FloatingPoint#(8, 23) out = rg_out_9;
      Bit#(3) guard = rg_guard_9;
      FloatingPoint#(8, 23) ab = rg_ab1_9 ;
      Bool acc = rg_acc2_9;
      FloatingPoint#(8, 23) opA = rg_opA2_9;
      FloatingPoint#(8, 23) opB = rg_opB2_9;
      FloatingPoint#(8, 23) opC = rg_opC2_9;
      Bool sub = rg_sub_9;
      Int#(10) expAB = rg_expAB2_9;

      if (s.res matches tagged Valid .x) begin
         out = x;
      end
      
      else begin
         let y = round2(s.rmode, out, guard);   //rounding done based on rounding mode and guard bit
         out = tpl_1(y);
         s.exc = s.exc | tpl_2(y);

         if(s.exc.overflow == True)     // since we do not send the previously set flags to round2()
            s.exc.underflow = False;
   
         if(out.exp == 0 && s.exc.inexact && !s.exc.overflow)  //set underflow for zero exponent
            s.exc.underflow = True;
      
         if (acc && isZero(out) && !s.exc.inexact && sub)     // adjust sign for exact zero result
            out.sign = (s.rmode == Rnd_Minus_Inf);

         //in round_nearest_even and maxMax rounding mode and overflow, set final result to +inf.
         if(s.exc.overflow == True && (s.rmode == Rnd_Nearest_Even ||s.rmode == Rnd_Nearest_Away_Zero)) begin
            s.exc.underflow = False;
            out.exp = '1;
            out.sfd = '0;
         end   

         //in round_plus_inf and overflow, set the result to infinity in case of positive result and largest negative number in case of negative result
         else if(s.exc.overflow == True && s.rmode == Rnd_Plus_Inf)begin
            if(ab.sign)   begin
               out.exp = 'b11111110;
               out.sfd = '1;   
            end
            else begin
               out.exp = '1;
               out.sfd = '0;   
            end
         end

         //in round_minus_inf and overflow, set the result to minus infinity in case of negative result and largest positive number in case of positive result
         else if(s.exc.overflow == True && s.rmode == Rnd_Minus_Inf) begin
            if(ab.sign) begin
               out.exp = '1;
               out.sfd = '0;   
            end
            else begin
               out.exp = 'b11111110;
               out.sfd = '1;   
            end
         end

         //in round_zero rounding mode and overflow, set result to the maximum normal number
         else if(s.exc.overflow == True && s.rmode == Rnd_Zero) begin
            out.exp = 'b11111110;
            out.sfd = '1;   
         end

         //if opA, opB and opC are zero, set result to zero 
         else if( (isZero(opA)||isZero(opB)) && isZero(opC)) begin
            out.exp = 0;
            out.sfd = 0;
         end

         //If exponent less than -126, we cannot represent it, set it to zero. But round up rounding mode, set it to least number in case of positive result
         else if(isZero(opC) && expAB < -126 && !ab.sign  && s.rmode == Rnd_Plus_Inf) begin
            out.exp = 0;
            out.sfd = 1;
            s.exc.underflow = True;
         end

         else if(isZero(opC) && expAB < -126 && ab.sign && s.rmode == Rnd_Minus_Inf) begin
            out.exp = 0;
            out.sfd = 1;
            s.exc.underflow = True;
            s.exc.inexact = True;
         end

         else if(isZero(opC) && expAB < -126) begin
            out.exp = 0;
            out.sfd = 0;
            s.exc.underflow = True;
            s.exc.inexact = True;
         end   
      end

      wr_returnvalues <= tuple2(canonicalize(out),s.exc);
   endrule   
   /************************************************************************************************************/   

   method Action send(Tuple4#(FloatingPoint#(8,23),FloatingPoint#(8,23),Maybe#(FloatingPoint#(8,23)),RoundMode) operands);
      wr_operands <= operands;
   endmethod
   
   method ReturnType#(8,23) receive();
      return ReturnType{valid:rg_stage_valid[8],value:tpl_1(wr_returnvalues) ,ex:tpl_2(wr_returnvalues)};
   endmethod 
   /************************************************************************************************************/
endmodule


/*------------------------------------------------------------------------------------------------------------*/


////////////////////////////////////////////////////////////////////////////////
/// DOUBLE PRECISION Floating point fused multiple accumulate
////////////////////////////////////////////////////////////////////////////////

interface Ifc_fpu_fma_dp;
   /*doc:method:send method for receiving inputs in dp. */
   method Action send(Tuple4#(FloatingPoint#(11,52),FloatingPoint#(11,52), Maybe#(FloatingPoint#(11,52)),RoundMode) operands);
   /*doc:method:receive method for returning result in dp. */
   method ReturnType#(11,52) receive();
endinterface
/************************************************************************************************************/

(*synthesize*)
/*doc:module:this module implements send and receive method in dp*/
module mk_fpu_fma_dp(Ifc_fpu_fma_dp);

   Vector#(`STAGES_FMA_DP,Reg#(Tuple2#(FloatingPoint#(11,52),Exception))) rg_stage_out <- replicateM(mkReg(tuple2(unpack(0),unpack(0))));
   Vector#(`STAGES_FMA_DP,Reg#(Bit#(1))) rg_stage_valid <- replicateM(mkDReg(0));
   Wire#(Tuple4#(FloatingPoint#(11,52),FloatingPoint#(11,52),Maybe#(FloatingPoint#(11,52)),RoundMode)) wr_operands <- mkDWire(tuple4(unpack(0), unpack(0), unpack(0), unpack(0)));
   Wire#(Tuple2#(FloatingPoint#(11,52),Exception)) wr_returnvalues <- mkDWire(tuple2(unpack(0), unpack(0)));

   //REG BTW STAGE1 -> STAGE2
   Reg#(Bit#(53)) rg_sfdA_1 <- mkReg(0);
   Reg#(Bit#(53)) rg_sfdB_1 <- mkReg(0);
   Reg#(Int#(13)) rg_expAB_1 <- mkReg(0);
   Reg#(Bool) rg_sgnAB_1 <- mkReg(False);
   Reg#(Bool) rg_zeroC_1 <- mkReg(False);
   Reg#(Bool)   rg_acc_1 <- mkReg(False);
   Reg#(FloatingPoint#(11, 52)) rg_opC_1 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opA_1 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB_1 <- mkReg(0);
   Reg#(CommonState#(11, 52)) rg_s_1 <- mkReg(unpack(0));

   //REG BTW STAGE2 -> STAGE3
   Reg#(Bit#(53)) rg_sfdA_2  <- mkReg(0);
   Reg#(Bit#(53)) rg_sfdB_2  <- mkReg(0);
   Reg#(Int#(13)) rg_expAB_2 <- mkReg(0);
   Reg#(Bool)   rg_acc_2 <- mkReg(False);
   Reg#(Bool) rg_sgnAB_2 <- mkReg(False);
   Reg#(Bool) rg_zeroC_2 <- mkReg(False);
   Reg#(FloatingPoint#(11, 52)) rg_opA_2 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB_2 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opC_2 <- mkReg(0);
   Reg#(Bit#(216)) rg_partial_product0_2 <- mkReg(0);
   Reg#(Bit#(216)) rg_partial_product1_2 <- mkReg(0);
   Reg#(Bit#(216)) rg_partial_product2_2 <- mkReg(0);
   Reg#(Bit#(216)) rg_partial_product3_2 <- mkReg(0);
   Reg#(Bit#(216)) rg_partial_product4_2 <- mkReg(0);
   Reg#(Bit#(216)) rg_partial_product5_2 <- mkReg(0);
   Reg#(Bit#(216)) rg_partial_product6_2 <- mkReg(0);
   Reg#(Bit#(216)) rg_partial_product7_2 <- mkReg(0);
   Reg#(Bit#(216)) rg_partial_product8_2 <- mkReg(0);
   Reg#(CommonState#(11,52)) rg_s_2 <- mkReg(unpack(0));

   //REG BTW STAGE3 -> STAGE4
   Reg#(Bit#(53)) rg_sfdA_3  <- mkReg(0);
   Reg#(Bit#(53)) rg_sfdB_3  <- mkReg(0);
   Reg#(Int#(13)) rg_expAB_3 <- mkReg(0);
   Reg#(Bool)   rg_acc_3 <- mkReg(False);
   Reg#(Bool) rg_sgnAB_3 <- mkReg(False);
   Reg#(Bool) rg_zeroC_3 <- mkReg(False);
   Reg#(FloatingPoint#(11, 52)) rg_opA_3 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB_3 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opC_3 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product0_3 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product1_3 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product2_3 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product3_3 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product4_3 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product5_3 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product6_3 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product7_3 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product8_3 <- mkReg(0);
   Reg#(CommonState#(11, 52)) rg_s_3 <- mkReg(unpack(0));

   //REG BTW STAGE4 -> STAGE5
   Reg#(Bit#(53)) rg_sfdA_4 <- mkReg(0);
   Reg#(Bit#(53)) rg_sfdB_4 <- mkReg(0);
   Reg#(Int#(13)) rg_expAB_4 <- mkReg(0);
   Reg#(Bit#(108)) rg_sfdAB_4 <- mkReg(0);
   Reg#(Bool)   rg_acc_4 <- mkReg(False);
   Reg#(Bool) rg_zeroC_4 <- mkReg(False);
   Reg#(Bool) rg_sgnAB_4 <- mkReg(False);
   Reg#(FloatingPoint#(11, 52)) rg_opA_4 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB_4 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opC_4 <- mkReg(0);
   Reg#(CommonState#(11,52)) rg_s_4 <- mkReg(unpack(0));
   Reg#(Bit#(108)) sfdAB_temp_4 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product5_4 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product6_4 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product7_4 <- mkReg(0);
   Reg#(Bit#(108)) rg_partial_product8_4 <- mkReg(0);

   //REG BTW STAGE5 -> STAGE6
   Reg#(Bit#(53)) rg_sfdA_5 <- mkReg(0);
   Reg#(Bit#(53)) rg_sfdB_5 <- mkReg(0);
   Reg#(Int#(13)) rg_expAB_5 <- mkReg(0);
   //Reg#(Bit#(108)) rg_sfdAB_5 <- mkReg(0);
   Reg#(Bool)   rg_acc_5 <- mkReg(False);
   Reg#(Bool) rg_zeroC_5 <- mkReg(False);
   Reg#(Bool) rg_sgnAB_5 <- mkReg(False);
   Reg#(FloatingPoint#(11, 52)) rg_opA_5 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB_5 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opC_5 <- mkReg(0);
   Reg#(CommonState#(11,52)) rg_s_5 <- mkReg(unpack(0));
   Reg#(FloatingPoint#(11, 52))  rg_ab_5 <- mkReg(0);
   Reg#(Bit#(108)) rg_sfdAb_5 <- mkReg(0);
   Reg#(Bit#(106)) rg_sfdAB_5 <- mkReg(0);
   Reg#(Bool) rg_flag_2_5 <- mkReg(False);
   Reg#(Bool) rg_flag_3_5 <- mkReg(False);
   Reg#(Bool) rg_bigEXP_5 <- mkReg(False);
   Reg#(Bit#(1)) rg_bit_flag_5 <- mkReg(0);

   //REG BTW STAGE6 -> STAGE7
   Reg#(Bit#(108)) rg_sfdAb_6 <- mkReg(0);
   Reg#(Int#(13)) rg_expAB_6 <- mkReg(0);
   Reg#(UInt#(7)) rg_zeros_6 <- mkReg(0);
   Reg#(Bit#(106)) rg_sfdAB_6 <- mkReg(0);
   Reg#(Bool)    rg_acc_6 <- mkReg(False);
   Reg#(Bool)  rg_zeroC_6 <- mkReg(False);
   Reg#(Bool) rg_bigEXP_6 <- mkReg(False);
   Reg#(Bool) rg_sgnAB_6  <- mkReg(False);
   Reg#(Bool) rg_flag_2_6 <- mkReg(False);
   Reg#(Bool) rg_flag_3_6 <- mkReg(False);
   Reg#(FloatingPoint#(11, 52))  rg_ab_6 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opC_6 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opA_6 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB_6 <- mkReg(0);
   Reg#(CommonState#(11, 52)) rg_s_6 <- mkReg(unpack(0));

   //REG BTW STAGE7 -> STAGE8
   Reg#(Bool) rg_sgnAB_7 <- mkReg(False);
   Reg#(Bit#(108)) rg_sfdAb_7 <- mkReg(0);
   Reg#(Int#(13)) rg_expAB_7 <- mkReg(0);
   Reg#(Bool)    rg_acc_7 <- mkReg(False);
   Reg#(Bool)  rg_zeroC_7 <- mkReg(False);
   Reg#(Bool) rg_bigEXP_7 <- mkReg(False);
   Reg#(FloatingPoint#(11, 52))  rg_ab_7 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opC_7 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opA_7 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB_7 <- mkReg(0);
   Reg#(CommonState#(11, 52)) rg_s_7 <- mkReg(unpack(0));

   //REG BTW STAGE8 -> STAGE9    
   Reg#(Bool) rg_sgn_8 <- mkReg(False);
   Reg#(Bool) rg_sub_8 <- mkReg(False);
   Reg#(Bit#(108))  rg_sfdX_8 <- mkReg(0);
   Reg#(Bit#(108))  rg_sfdY_8 <- mkReg(0);
   Reg#(Bit#(108)) rg_sfdAb_8 <- mkReg(0);
   Reg#(Int#(13)) rg_expAB_8 <- mkReg(0);
   Reg#(Int#(13))   rg_exp_8 <- mkReg(0);
   Reg#(Bool)    rg_acc_8 <- mkReg(False);
   Reg#(Bool)  rg_zeroC_8 <- mkReg(False);
   Reg#(Bool) rg_bigEXP_8 <- mkReg(False);
   Reg#(FloatingPoint#(11, 52))  rg_ab_8 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opC_8 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opA_8 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB_8 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_out_8 <- mkReg(0);
   Reg#(CommonState#(11,52)) rg_s_8 <- mkReg(unpack(0));

   //REG BTW STAGE9 -> STAGE10
   Reg#(Bool) rg_sgn_9    <- mkReg(False);
   Reg#(Bool) rg_acc_9    <- mkReg(False);
   Reg#(Bool) rg_zeroC_9  <- mkReg(False);
   Reg#(Bool) rg_bigEXP_9 <- mkReg(False);
   Reg#(Bit#(108)) rg_sfdAb_9 <- mkReg(0);
   Reg#(Int#(13)) rg_expAB_9 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opC_9 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opA_9 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB_9 <- mkReg(0);
   Reg#(CommonState#(11, 52)) rg_s_9 <- mkReg(unpack(0));
   Reg#(FloatingPoint#(11, 52)) rg_ab_9 <- mkReg(0);
   Reg#(Int#(13)) rg_exp_9 <- mkReg(0);
   Reg#(Bit#(108)) rg_sfdX_9 <- mkReg(0);
   Reg#(Bit#(108)) rg_sfdY_9 <- mkReg(0);
   Reg#(Bool) rg_sub_9 <- mkReg(False);
   Reg#(FloatingPoint#(11, 52)) rg_out_9 <- mkReg(0);   
   Reg#(Bit#(108)) rg_sfd_9 <- mkReg(0);

   //REG BTW STAGE10 -> STAGE11
   Reg#(CommonState#(11,52)) rg_s2_10 <- mkReg(unpack(0));
   Reg#(FloatingPoint#(11, 52)) rg_out_10 <- mkReg(0);
   Reg#(Bit#(3)) rg_guard_10 <- mkReg(0);
   Reg#(Bool) rg_acc2_10 <- mkReg(False);
   Reg#(FloatingPoint#(11,52)) rg_ab1_10 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opA2_10 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB2_10 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opC2_10 <- mkReg(0);
   Reg#(Bool) rg_sub_10 <- mkReg(False);
   Reg#(Int#(13)) rg_expAB2_10 <- mkReg(0);
   Reg#(UInt#(7)) rg_zeros_10 <- mkReg(0);
   Reg#(Int#(13)) rg_exp_10 <- mkReg(0);
   Reg#(Bit#(108)) rg_sfd_10 <- mkReg(0);
   Reg#(Bool) rg_bigExp_10 <- mkReg(False);

   //REG BTW STAGE11 -> Receive
   Reg#(CommonState#(11, 52)) rg_s2_11 <- mkReg(unpack(0));
   Reg#(FloatingPoint#(11, 52)) rg_out_11 <- mkReg(0);
   Reg#(Bit#(3)) rg_guard_11 <- mkReg(0);
   Reg#(Bool) rg_acc2_11 <- mkReg(False);
   Reg#(FloatingPoint#(11, 52)) rg_ab1_11 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opA2_11 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opB2_11 <- mkReg(0);
   Reg#(FloatingPoint#(11, 52)) rg_opC2_11 <- mkReg(0);
   Reg#(Bool) rg_sub_11 <- mkReg(False);
   Reg#(Int#(13)) rg_expAB2_11 <- mkReg(0);
   /************************************************************************************************************/

   rule rl_fpu_fma_dp_stage_1; 
      match {.opA, .opB, .mopC, .rmode } = wr_operands;
      CommonState#(11,52) s = CommonState {res: tagged Invalid,exc: defaultValue, rmode: rmode }; 
      Bool acc = False;
      FloatingPoint#(11,52) opC = 0;

      if (mopC matches tagged Valid .opC_) begin
         opC = opC_;
         acc = True;
      end
      //zeroC indicates whether operand C is zero or not
      Bool zeroC = (opC.exp == 0 && opC.sfd == 0);
      //If number is subnormal, then exponent will become minimum exponent, it is -126 for sp and -1022 for dp
      //If number is not subnormal, then unbias it, bias is 127 for sp and 1023 for dp
      Int#(13) expA = isSubNormal(opA) ? fromInteger(minexp(opA)) : signExtend(unpack(unbias(opA)));
      Int#(13) expB = isSubNormal(opB) ? fromInteger(minexp(opB)) : signExtend(unpack(unbias(opB)));  

      //significand is simply taken from sfd part of operand. Hidden bit is either 0 or 1, it is 0 for subnormal number and 1 for normal number 
      //subnormal number have exponent as zero. 
      Bit#(53) sfdA = { getHiddenBit(opA), opA.sfd };
      Bit#(53) sfdB = { getHiddenBit(opB), opB.sfd };


      Bool sgnAB = opA.sign != opB.sign;                 //sgnAB maintains sign of product
      Bool infAB = isInfinity(opA) || isInfinity(opB);   //infAB indicates whether result of product is infinity or not
      Bool zeroAB = isZero(opA) || isZero(opB);          //zeroAB indicates product is zero or not.

      // OpA*OpB - add exponents of both the operands and multiplies their mantissas 
      Int#(13) expAB = expA + expB;

      if(((isZero(opA) && isInfinity(opB)) || (isZero(opB) && isInfinity(opA)) ) && isNaN(opC))
         s.exc.invalid_op = True;

      //if opC is signaling NaN, its quiet NaN is the result
      if (isSNaN(opC)) begin
         s.res = tagged Valid nanQuiet(opC);
         s.exc.invalid_op = True;
      end

      //if opA is signaling NaN, it's quiet NaN is the result
      else if (isSNaN(opA)) begin
         s.res = tagged Valid nanQuiet(opA);
         s.exc.invalid_op = True;
      end

      //if opB is signaling NaN, it's Quiet NaN is the result
      else if (isSNaN(opB)) begin
         s.res = tagged Valid nanQuiet(opB);
         s.exc.invalid_op = True;
      end

      //When opA, opB or opC is quiet NaN, the invalid flag will not be set
      else if (isQNaN(opC)) begin
         s.res = tagged Valid opC;
      end

      else if (isQNaN(opA)) begin
         s.res = tagged Valid opA;
      end

      else if (isQNaN(opB)) begin
         s.res = tagged Valid opB;
      end

      else if ((isInfinity(opA) && isZero(opB)) || (isZero(opA) && isInfinity(opB)) || (isInfinity(opC) && infAB && (opC.sign != sgnAB))) begin
         s.res = tagged Valid qnan();
         s.exc.invalid_op = True;
      end

      //if opC is infinity, it is final result
      else if (isInfinity(opC)) begin
         s.res = tagged Valid opC;
      end

      //if opA or opB is infinity, the result is infinity
      else if (infAB) begin
         s.res = tagged Valid infinity(sgnAB);
      end

      //if opC and product is zero, the result is also zero
      else if (isZero(opC) && zeroAB && (opC.sign == sgnAB)) begin
         s.res = tagged Valid opC;
      end

      rg_sfdA_1 <= sfdA;   //53 Bit
      rg_sfdB_1 <= sfdB;
      rg_expAB_1 <= expAB;
      rg_sgnAB_1 <= sgnAB;
      rg_zeroC_1 <= zeroC;
      rg_acc_1 <= acc;
      rg_opC_1 <= opC;
      rg_opA_1 <= opA;
      rg_opB_1 <= opB;
      rg_s_1 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_stage_valid[0] <= 1; // Unintialized
   endrule // STAGE 1
   /************************************************************************************************************/

   rule rl_fpu_fma_dp_stage_2;

      Bit#(53) sfdA = rg_sfdA_1 ;
      Bit#(53) sfdB = rg_sfdB_1 ;
      CommonState#(11,52) s = rg_s_1; 
      Int#(13) expAB = rg_expAB_1;
      Bool sgnAB = rg_sgnAB_1;
      FloatingPoint#(11, 52) opC = rg_opC_1;
      FloatingPoint#(11, 52) opB = rg_opB_1;
      FloatingPoint#(11, 52) opA = rg_opA_1;
      Bool acc = rg_acc_1;
      Bool zeroC = rg_zeroC_1;

      Bit#(11) expA1 = opA.exp;
      Bit#(11) expB1 = opB.exp;
      Bit#(52) sfdA1 = opA.sfd;
      Bit#(52) sfdB1 = opB.sfd;

      // GENERATING PARTIAL PRODUCTS
      rg_partial_product0_2 <= fn_gen_pp_dp({1'b0,(|expA1),sfdA1},sfdB1[5:0]);
      rg_partial_product1_2 <= fn_gen_pp_dp({1'b0,(|expA1),sfdA1},sfdB1[11:6]);
      rg_partial_product2_2 <= fn_gen_pp_dp({1'b0,(|expA1),sfdA1},sfdB1[17:12]); 
      rg_partial_product3_2 <= fn_gen_pp_dp({1'b0,(|expA1),sfdA1},sfdB1[23:18]);  
      rg_partial_product4_2 <= fn_gen_pp_dp({1'b0,(|expA1),sfdA1},sfdB1[29:24]);  
      rg_partial_product5_2 <= fn_gen_pp_dp({1'b0,(|expA1),sfdA1},sfdB1[35:30]);  
      rg_partial_product6_2 <= fn_gen_pp_dp({1'b0,(|expA1),sfdA1},sfdB1[41:36]);  
      rg_partial_product7_2 <= fn_gen_pp_dp({1'b0,(|expA1),sfdA1},sfdB1[47:42]);
      rg_partial_product8_2 <= fn_gen_pp_dp({1'b0,(|expA1),sfdA1},{1'b0,(|expB1),sfdB1[51:48]});
      
      rg_sfdA_2 <= sfdA;   //53 Bits
      rg_sfdB_2 <= sfdB;
      rg_expAB_2 <= expAB;
      rg_acc_2 <= acc;
      rg_sgnAB_2 <= sgnAB;
      rg_zeroC_2 <= zeroC;
      rg_opA_2 <= opA;
      rg_opB_2 <= opB;
      rg_opC_2 <= opC;
      rg_s_2 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_stage_valid[1] <= rg_stage_valid[0];
   endrule // STAGE 2
   /************************************************************************************************************/

   rule rl_fpu_fma_dp_stage_3;

      Bit#(53) sfdA = rg_sfdA_2 ;
      Bit#(53) sfdB = rg_sfdB_2 ;
      CommonState#(11,52) s = rg_s_2; 
      Int#(13) expAB = rg_expAB_2;
      Bool sgnAB = rg_sgnAB_2;
      FloatingPoint#(11, 52) opC = rg_opC_2;
      FloatingPoint#(11, 52) opB = rg_opB_2;
      FloatingPoint#(11, 52) opA = rg_opA_2;
      Bool acc = rg_acc_2;
      Bool zeroC = rg_zeroC_2;

      // PARTIAL ADDITION OF PARTIAL PRODUCTS
      rg_partial_product0_3 <= rg_partial_product0_2[107:0]+rg_partial_product0_2[215:108];   //108 Bits
      rg_partial_product1_3 <= {(rg_partial_product1_2[107:0]+rg_partial_product1_2[215:108])[101:0],6'd0};
      rg_partial_product2_3 <= {(rg_partial_product2_2[107:0]+rg_partial_product2_2[215:108])[95:0],12'd0};
      rg_partial_product3_3 <= {(rg_partial_product3_2[107:0]+rg_partial_product3_2[215:108])[89:0],18'd0};
      rg_partial_product4_3 <= {(rg_partial_product4_2[107:0]+rg_partial_product4_2[215:108])[83:0],24'd0};
      rg_partial_product5_3 <= {(rg_partial_product5_2[107:0]+rg_partial_product5_2[215:108])[77:0],30'd0};
      rg_partial_product6_3 <= {(rg_partial_product6_2[107:0]+rg_partial_product6_2[215:108])[71:0],36'd0};
      rg_partial_product7_3 <= {(rg_partial_product7_2[107:0]+rg_partial_product7_2[215:108])[65:0],42'd0};
      rg_partial_product8_3 <= {(rg_partial_product8_2[107:0]+rg_partial_product8_2[215:108])[59:0],48'd0};

      rg_sfdA_3 <= sfdA;
      rg_sfdB_3 <= sfdB;
      rg_expAB_3 <= expAB;
      rg_acc_3 <= acc;
      rg_sgnAB_3 <= sgnAB;
      rg_zeroC_3 <= zeroC;
      rg_opA_3 <= opA;
      rg_opB_3 <= opB;
      rg_opC_3 <= opC;
      rg_s_3 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_stage_valid[2] <= rg_stage_valid[1];
   endrule // STAGE 3
   /************************************************************************************************************/

   rule rl_fpu_fma_dp_stage_4;

      Bit#(53) sfdA = rg_sfdA_3 ;
      Bit#(53) sfdB = rg_sfdB_3 ;
      CommonState#(11,52) s = rg_s_3; 
      Int#(13) expAB = rg_expAB_3;
      Bool sgnAB = rg_sgnAB_3;
      FloatingPoint#(11, 52) opC = rg_opC_3;
      FloatingPoint#(11, 52) opB = rg_opB_3;
      FloatingPoint#(11, 52) opA = rg_opA_3;
      Bool acc = rg_acc_3;
      Bool zeroC = rg_zeroC_3;

      Bit#(108) v1 =0;
      Bit#(108) v2 =0;
      Bit#(108) v3 =0;
      Bit#(108) v4 =0;
      Bit#(108) v5 =0;
      Bit#(108) v6 =0;
      Bit#(108) v7 =0;
      Bit#(108) v8 =0;
      Bit#(108) v9 =0;
      Bit#(108) v10 =0;

      v1 = pack(rg_partial_product0_3);
      v2 = pack(rg_partial_product1_3);
      v3 = pack(rg_partial_product2_3);
      v4 = pack(rg_partial_product3_3);
      v5 = pack(rg_partial_product4_3);
      v6 = pack(rg_partial_product5_3);
      v7 = pack(rg_partial_product6_3);
      v8 = pack(rg_partial_product7_3);
      v9 = pack(rg_partial_product8_3);

      // FINAL ADDITION OF PARTIAL PRODUCTS
      Bit#(108) sfdAB_temp_1 =v1+v2+v3+v4+v5;    //108 Bits
      Bit#(108) sfdAB_temp_2 = v6+v7+v8+v9; 
      sfdAB_temp_4 <= sfdAB_temp_1 + sfdAB_temp_2;

      rg_sfdA_4 <= sfdA;
      rg_sfdB_4 <= sfdB;
      rg_s_4 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_expAB_4 <= expAB;
      rg_sgnAB_4 <= sgnAB;
      rg_opC_4 <= opC;
      rg_opA_4 <= opA;
      rg_opB_4 <= opB;
      rg_acc_4 <= acc;
      rg_zeroC_4 <= zeroC;
      rg_stage_valid[3] <= rg_stage_valid[2];
   endrule // STAGE 4
   /************************************************************************************************************/

   rule rl_fpu_fma_dp_stage_5;

      Bit#(53) sfdA = rg_sfdA_4 ;
      Bit#(53) sfdB = rg_sfdB_4 ;
      CommonState#(11,52) s = rg_s_4; 
      Int#(13) expAB = rg_expAB_4;
      Bool sgnAB = rg_sgnAB_4;
      FloatingPoint#(11, 52) opC = rg_opC_4;
      FloatingPoint#(11, 52) opB = rg_opB_4;
      FloatingPoint#(11, 52) opA = rg_opA_4;
      Bool acc = rg_acc_4;
      Bool zeroC = rg_zeroC_4;

      Bit#(108) sfdAB_temp = sfdAB_temp_4;
      Bit#(106) sfdAB = sfdAB_temp[105:0];

      // normalize multiplication result
      Bit#(108) sfdAb =0;
      Bool bigEXP = False; 
      FloatingPoint#(11,52) ab = defaultValue;
      Bool flag_2 = expAB > fromInteger(maxexp(ab));
      Bool flag_3 = expAB < (fromInteger(minexp_subnormal(ab))-2);
      let shift = fromInteger(minexp(ab)) - expAB;
      Bit#(1) bit_flag = |sfdAB;

      // s.res is invalid -> Not a special case  
      if (s.res matches tagged Invalid) begin

         //if product exponent is greater than max exponent(127 for sp and 1023 for dp), normalization need not be done
         //product exponent and product mantissa remains as it is
         if (flag_2) begin
            ab.sign = sgnAB;
            ab.exp = maxBound - 1;
            ab.sfd = maxBound;
            sfdAb = {sfdAB,2'b00};
         end

         //checking for minimum possible exponent, checks if the exponent cannot be in a valid range after a shift  
         else begin 
            if (flag_3) begin
               ab.sign = sgnAB;
               ab.exp = 0;
               ab.sfd = 0;
               sfdAb = {sfdAB,2'b00};
               bigEXP = (bit_flag == 1)? True: False;
            end
         end
      end

      if (s.res matches tagged Invalid) begin
         if(!flag_2 && !flag_3) begin
            //shift > 0 indicates expAB is smaller than minimum exponent. If true, set expAB as minimum possible exponent.
            if (shift > 0) begin
               //subnormal
               `ifdef denormal_support
                  Bit#(108) zero_val = 108'd0;
                  Bit#(1) sfdlsb = |({zero_val+sfdAB[shift:0]});
                  //Bit#(1) sfdlsb = |(sfdAB << (fromInteger(valueOf(106)) - shift));
                  sfdAB = sfdAB >> shift;
                  sfdAB[0] = sfdAB[0] | sfdlsb;
                  expAB = 'd-1022;
               `else
                  ab.sfd = 0;
                  sfdAB = 0;
               `endif
               ab.exp = 0;
            end
            //simply add bias in product exponent 
            else begin
               ab.exp = cExtend(expAB + fromInteger(bias(ab)));
            end
         end
      end

      rg_sfdA_5 <= sfdA;
      rg_sfdB_5 <= sfdB;
      rg_s_5 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_expAB_5 <= expAB;
      rg_sgnAB_5 <= sgnAB;
      rg_opC_5 <= opC;
      rg_opA_5 <= opA;
      rg_opB_5 <= opB;
      rg_acc_5 <= acc;
      rg_zeroC_5 <= zeroC;
      rg_ab_5 <= ab;
      rg_sfdAb_5 <= sfdAb;
      rg_sfdAB_5 <= sfdAB;
      rg_flag_2_5 <= flag_2;
      rg_flag_3_5 <= flag_3;
      rg_bit_flag_5 <= bit_flag;
      rg_bigEXP_5 <= bigEXP;
      rg_stage_valid[4] <= rg_stage_valid[3];
   endrule // STAGE 5
   /************************************************************************************************************/

   rule rl_fpu_fma_dp_stage_6;
 
      //get values from stage1
      Bit#(53) sfdA = rg_sfdA_5 ;
      Bit#(53) sfdB = rg_sfdB_5 ;
      CommonState#(11,52) s = rg_s_5; 
      Int#(13) expAB = rg_expAB_5;
      Bool sgnAB = rg_sgnAB_5;
      FloatingPoint#(11, 52) opC = rg_opC_5;
      FloatingPoint#(11, 52) opB = rg_opB_5;
      FloatingPoint#(11, 52) opA = rg_opA_5;
      Bool acc = rg_acc_5;
      Bool zeroC = rg_zeroC_5;
      Bit#(108) sfdAb =rg_sfdAb_5;
      Bit#(106) sfdAB =rg_sfdAB_5;
      Bool bigEXP = rg_bigEXP_5; 
      FloatingPoint#(11,52) ab = rg_ab_5;
      Bool flag_2 = rg_flag_2_5;
      Bool flag_3 = rg_flag_3_5;
      Bit#(1) bit_flag =rg_bit_flag_5;
      UInt#(7) zeros =0;
    
      if (s.res matches tagged Invalid) begin
         if(!flag_2 && !flag_3) begin
            ab.sign = sgnAB;
            expAB = (bit_flag == 0)? 'd-1023: expAB;
            //Normalization 1 Part 1
            zeros = normalize_dp_1_part_1(sfdAB);
         end
      end

      rg_sgnAB_6 <= sgnAB;
      rg_sfdAb_6 <= sfdAb;
      rg_expAB_6 <= expAB;
      rg_opC_6 <= opC;
      rg_opA_6 <= opA;
      rg_opB_6 <= opB;
      rg_s_6 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_ab_6 <= ab;
      rg_acc_6 <= acc;
      rg_zeroC_6 <= zeroC;
      rg_bigEXP_6 <= bigEXP;
      rg_sfdAB_6 <= sfdAB;
      rg_zeros_6 <= zeros;
      rg_flag_2_6 <= flag_2;
      rg_flag_3_6 <= flag_3;
      rg_stage_valid[5] <= rg_stage_valid[4];
   endrule // STAGE 6
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_7;

      Bool sgnAB = rg_sgnAB_6;
      Bit#(108) sfdAb = rg_sfdAb_6;
      Int#(13) expAB = rg_expAB_6;
      FloatingPoint#(11, 52) opC = rg_opC_6 ;
      FloatingPoint#(11, 52) opA = rg_opA_6 ;
      FloatingPoint#(11, 52) opB = rg_opB_6 ;
      CommonState#(11, 52) s = rg_s_6; 
      FloatingPoint#(11, 52) ab = rg_ab_6 ;
      Bool acc = rg_acc_6;
      Bool zeroC = rg_zeroC_6;
      Bool bigEXP = rg_bigEXP_6;
      Bit#(106) sfdAB = rg_sfdAB_6;
      UInt#(7) zeros = rg_zeros_6;
      Exception exc = defaultValue;
      Bool flag_2 = rg_flag_2_6;
      Bool flag_3 = rg_flag_3_6;
      
      if (s.res matches tagged Invalid) begin
         if(!flag_2 && !flag_3) begin
            //Normalization1 sp part 2 function gives exception, normalized result. 
            let y1 = normalize_dp_1_part_2(ab, sfdAB, zeros);
            ab = tpl_1(y1);
            s.exc = s.exc | tpl_2(y1);
            sfdAb = tpl_3(y1);
            expAB = isSubNormal(ab) ? fromInteger(minexp(ab)) : signExtend(unpack(unbias(ab)));
         end
      end

      rg_sgnAB_7 <= sgnAB;
      rg_sfdAb_7 <= sfdAb;
      rg_expAB_7 <= expAB;
      rg_opC_7 <= opC;
      rg_opA_7 <= opA;
      rg_opB_7 <= opB;
      rg_s_7 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_ab_7 <= ab;
      rg_acc_7 <= acc;
      rg_zeroC_7 <= zeroC;
      rg_bigEXP_7 <= bigEXP;
      rg_stage_valid[6] <=  rg_stage_valid[5];
   endrule // STAGE 7
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_8;

      Bool sgnAB = rg_sgnAB_7;
      Bit#(108) sfdAb = rg_sfdAb_7;
      Int#(13) expAB = rg_expAB_7;
      FloatingPoint#(11, 52) opC = rg_opC_7 ;
      FloatingPoint#(11, 52) opA = rg_opA_7 ;
      FloatingPoint#(11, 52) opB = rg_opB_7 ;
      CommonState#(11, 52) s = rg_s_7; 
      FloatingPoint#(11, 52) ab = rg_ab_7 ;
      Bool acc = rg_acc_7;
      Bool zeroC = rg_zeroC_7;
      Bool bigEXP = rg_bigEXP_7;

      //compute expC similar to expA,expB
      Int#(13) expC = isSubNormal(opC) ? fromInteger(minexp(opC)) : signExtend(unpack(unbias(opC)));
      `ifdef denormal_support
         opC.sfd = opC.sfd;
      `else
         if (isSubNormal(opC))
            opC.sfd = 0;
      `endif   

      //compute the significand of opC
      Bit#(108) sfdC = {1'b0,getHiddenBit(opC), opC.sfd, 54'b0};
      Bool sub = opC.sign != ab.sign;
      Bit#(1) subflag = sub ? 1:0;
      Int#(13) exp = ?;
      Int#(13) shift = ?;
      Bit#(108) x  = ?;
      Bit#(108) y  = ?;
      Bool sgn = ?;
      FloatingPoint#(11,52) out = defaultValue;

      //Smaller operand among opAB and opC will be y and larger will be x
      //later x+y and x-y is performed for final result

      if ((!acc) || (expAB > expC) || ((expAB == expC) && (sfdAb > sfdC))) begin
         exp = expAB;
         shift = expAB - expC;
         x = sfdAb;
         y = sfdC;
         sgn = ab.sign;
      end

      else  begin
         exp = expC;
         shift = expC - expAB;
         x = sfdC;
         y = sfdAb;
         sgn = opC.sign;
      end      

      //y is shifted to make exponent of opAB and opC equal   
      if (s.res matches tagged Invalid) begin
         if (shift < fromInteger(108)) begin
            Bit#(108) zero_val = 108'd0;
            Bit#(1) test_value = |({zero_val+y[shift:0]});
            y = y >> shift;
            y[0] = y[0] | test_value;
         end

         else if (|y == 1) begin
            y = 1;
         end
      end

      rg_sgn_8 <= sgn;
      rg_sfdAb_8 <= sfdAb;
      rg_expAB_8 <= expAB;
      rg_opC_8 <= opC;
      rg_opA_8 <= opA;
      rg_opB_8 <= opB;
      rg_s_8 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_ab_8 <= ab;
      rg_acc_8 <= acc;
      rg_zeroC_8 <= zeroC;
      rg_bigEXP_8 <= bigEXP;
      rg_exp_8 <= exp;
      rg_sfdX_8 <= x;
      rg_sfdY_8 <= y;
      rg_out_8 <= out;
      rg_sub_8 <= sub;
      rg_stage_valid[7] <=  rg_stage_valid[6];
   endrule // STAGE 8
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_9;

      Bool sgn = rg_sgn_8;
      Bit#(108) sfdAb = rg_sfdAb_8;
      Int#(13) expAB = rg_expAB_8;
      FloatingPoint#(11, 52) opC = rg_opC_8 ;
      FloatingPoint#(11, 52) opA = rg_opA_8 ;
      FloatingPoint#(11, 52) opB = rg_opB_8 ;
      CommonState#(11, 52) s = rg_s_8; 
      FloatingPoint#(11, 52) ab = rg_ab_8 ;
      Bool acc = rg_acc_8;
      Bool zeroC = rg_zeroC_8;
      Bool bigEXP = rg_bigEXP_8;
      Int#(13) exp = rg_exp_8;
      Bit#(108) x = rg_sfdX_8;
      Bit#(108) y =rg_sfdY_8;
      FloatingPoint#(11, 52) out = rg_out_8;
      Bool sub = rg_sub_8;   
      Bit#(1) subflag = sub?1:0;

      // ADDITION USING 108-BIT CLA FUNC
      rg_sfd_9 <= fn_CLA_dp_new(subflag,x,y);
    
      rg_sgn_9 <= sgn;
      rg_sfdAb_9 <= sfdAb;
      rg_expAB_9 <= expAB;
      rg_opC_9 <= opC;
      rg_opA_9 <= opA;
      rg_opB_9 <= opB;
      rg_s_9 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_ab_9 <= ab;
      rg_acc_9 <= acc;
      rg_zeroC_9 <= zeroC;
      rg_bigEXP_9 <= bigEXP;
      rg_exp_9 <= exp;
      rg_sfdX_9 <= x;
      rg_sfdY_9 <= y;
      rg_out_9 <= out;
      rg_sub_9 <= sub;
      rg_stage_valid[8] <=  rg_stage_valid[7];
   endrule // STAGE 9
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_10;

      Bool sgn = rg_sgn_9;
      Bit#(108) sfdAb = rg_sfdAb_9;
      Int#(13) expAB = rg_expAB_9;
      FloatingPoint#(11, 52) opC = rg_opC_9 ;
      FloatingPoint#(11, 52) opA = rg_opA_9 ;
      FloatingPoint#(11, 52) opB = rg_opB_9 ;
      CommonState#(11, 52) s = rg_s_9; 
      FloatingPoint#(11, 52) ab = rg_ab_9 ;
      Bool acc = rg_acc_9;
      Bool zeroC = rg_zeroC_9;
      Bool bigEXP = rg_bigEXP_9;
      Int#(13) exp = rg_exp_9;
      Bit#(108) x = rg_sfdX_9;
      Bit#(108) y =rg_sfdY_9;
      FloatingPoint#(11,52) out = rg_out_9;
      Bool sub = rg_sub_9;
      Bit#(108) sfd = rg_sfd_9;
      UInt#(7) zeros = 0;
      Bit#(3) guard = 0;

      //Checking for overflow while addition - extra carry generated implies overflow, inexact and overflow flag are set
      if((x[107] == 1 || y[107] == 1) && sfd[107] == 0 && !sub) begin
         s.exc.overflow = True;
         s.exc.inexact = True;
      end

      //final significand is either sum or difference, it is decided by value of sub
      if (s.res matches tagged Invalid) begin
         out.sign = sgn;                  
         out.exp = cExtend(exp + fromInteger(bias(out)));   //bias added in exponent
         if(zeroC) begin                                    //if opC is zero, final result is product of opA and op
            out = ab;
         end
         zeros = normalize_dp_2_part_1(sfd);                // normalization after add/sub phase NORMALIZE_2 PART 
      end

      rg_s2_10 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_out_10 <= out;
      rg_guard_10 <= guard;
      rg_ab1_10 <= ab;
      rg_acc2_10 <= acc;
      rg_opA2_10 <= opA;
      rg_opB2_10 <= opB;
      rg_opC2_10 <= opC;
      rg_sub_10 <= sub; 
      rg_expAB2_10 <= expAB;
      rg_zeros_10 <= zeros;
      rg_exp_10 <= exp;
      rg_sfd_10 <= sfd;
      rg_bigExp_10 <= bigEXP;
      rg_stage_valid[9] <=  rg_stage_valid[8];
   endrule //STAGE 10
   /************************************************************************************************************/

   rule rl_fpu_fma_sp_stage_11;

      CommonState#(11,52) s = rg_s2_10; 
      FloatingPoint#(11, 52) out = rg_out_10;
      Bit#(3) guard = rg_guard_10;
      FloatingPoint#(11, 52) ab = rg_ab1_10;
      Bool acc = rg_acc2_10;
      FloatingPoint#(11, 52) opA = rg_opA2_10;
      FloatingPoint#(11, 52) opB = rg_opB2_10;
      FloatingPoint#(11, 52) opC = rg_opC2_10;
      Bool sub = rg_sub_10;
      Int#(13) expAB = rg_expAB2_10;
      UInt#(7) zeros = rg_zeros_10;
      Bit#(108) sfd = rg_sfd_10;
      Int#(13) exp =rg_exp_10;
      Bool bigEXP = rg_bigExp_10;

      //this normalzation function gives result, exception flags and guard bit. guard bits are used for rounding. 
      if (s.res matches tagged Invalid) begin
         let norm = normalize_dp_2_part_2(zeros,exp,out, sfd,s.rmode);
         out = tpl_1(norm);
         guard = tpl_2(norm);
         s.exc = s.exc | tpl_3(norm);
         if(bigEXP)        //prevents by setting underflow flag during product phase. 
            s.exc.underflow = False;
      end   

      rg_s2_11 <= CommonState{res:s.res, exc:s.exc, rmode:s.rmode};
      rg_out_11 <= out;
      rg_guard_11 <= guard;
      rg_ab1_11 <= ab;
      rg_acc2_11 <= acc;
      rg_opA2_11 <= opA;
      rg_opB2_11 <= opB;
      rg_opC2_11 <= opC;
      rg_sub_11 <= sub; 
      rg_expAB2_11 <= expAB;
      rg_stage_valid[10] <= rg_stage_valid[9];
   endrule // STAGE 11
   /************************************************************************************************************/

   rule rl_receive;

      CommonState#(11,52) s = rg_s2_11; 
      FloatingPoint#(11, 52) out = rg_out_11;
      Bit#(3) guard = rg_guard_11;
      FloatingPoint#(11, 52) ab = rg_ab1_11;
      Bool acc = rg_acc2_11;
      FloatingPoint#(11, 52) opA = rg_opA2_11;
      FloatingPoint#(11, 52) opB = rg_opB2_11;
      FloatingPoint#(11, 52) opC = rg_opC2_11;
      Bool sub = rg_sub_11;
      Int#(13) expAB = rg_expAB2_11;

      if (s.res matches tagged Valid .x) begin
         out = x;
      end

      else  begin
         let y = round2(s.rmode, out, guard);                        //rounding done based on rounding mode and guard bit
         out = tpl_1(y);
         s.exc = s.exc | tpl_2(y);

         if(s.exc.overflow == True)
            s.exc.underflow = False;                                // since we do not send the previously set flags to round2()

         if(out.exp == 0 && s.exc.inexact && !s.exc.overflow)       //set underflow for zero exponent
            s.exc.underflow = True;

         if (acc && isZero(out) && !s.exc.inexact && sub)           // adjust sign for exact zero result
            out.sign = (s.rmode == Rnd_Minus_Inf);

         //in round_nearest_even and maxMax rounding mode and overflow, set final result to +inf.
         if(s.exc.overflow == True && (s.rmode == Rnd_Nearest_Even ||s.rmode == Rnd_Nearest_Away_Zero)) begin
            s.exc.underflow = False;
            out.exp = '1;
            out.sfd = '0;
         end

         //in round_plus_inf and overflow, set the result to infinity in case of positive result and largest negative number in case of negative result
         else if(s.exc.overflow == True && s.rmode == Rnd_Plus_Inf) begin
            if(ab.sign) begin
               out.exp = 'b11111111110;
               out.sfd = '1;   
            end

            else begin
               out.exp = '1;
               out.sfd = '0;   
            end
         end

         //in round_minus_inf and overflow, set the result to minus infinity in case of negative result and largest positive number in case of positive result
         else if(s.exc.overflow == True && s.rmode == Rnd_Minus_Inf) begin
            if(ab.sign) begin
               out.exp = '1;
               out.sfd = '0;   
            end
            else begin
               out.exp = 'b11111111110;
               out.sfd = '1;   
            end
         end

         //in round_zero rounding mode and overflow, set result to the maximum normal number
         else if(s.exc.overflow == True && s.rmode == Rnd_Zero) begin
            out.exp = 'b11111111110;
            out.sfd = '1;   
         end

         //if opA, opB and opC are zero, set result to zero 
         else if( (isZero(opA)||isZero(opB)) && isZero(opC)) begin
            out.exp = 0;
            out.sfd = 0;
         end

         //If exponent less than -1022, we cannot represent it, set it to zero. But round up rounding mode, set it to least number in case of positive result
         else if(isZero(opC) && expAB < -1022 && !ab.sign  && s.rmode == Rnd_Plus_Inf) begin
            out.exp = 0;
            out.sfd = 1;
            s.exc.underflow = True;
         end

         else if(isZero(opC) && expAB < -1022 && ab.sign && s.rmode == Rnd_Minus_Inf) begin
            out.exp = 0;
            out.sfd = 1;
            s.exc.underflow = True;
            s.exc.inexact = True;
         end

         else if(isZero(opC) && expAB < -1022) begin
            out.exp = 0;
            out.sfd = 0;
            s.exc.underflow = True;
            s.exc.inexact = True;
         end
      end

      wr_returnvalues <= tuple2(canonicalize(out),s.exc);
   endrule  // Receive
   /************************************************************************************************************/   

   method Action send(Tuple4#(FloatingPoint#(11,52),FloatingPoint#(11,52), Maybe#(FloatingPoint#(11,52)), RoundMode) operands);
      wr_operands <= operands;
   endmethod

   method ReturnType#(11,52) receive();
      return ReturnType{valid:rg_stage_valid[10],value:tpl_1(wr_returnvalues) ,ex:tpl_2(wr_returnvalues)};
   endmethod
   /************************************************************************************************************/   
endmodule

endpackage
  
