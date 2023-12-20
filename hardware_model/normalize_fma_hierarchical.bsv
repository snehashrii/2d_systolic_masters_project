////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011  Bluespec, Inc.   ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
/* 
see LICENSE.iitm
--------------------------------------------------------------------------------------------------
Author: Shalender Kumar, Charulatha Narashiman
Email id: cs18m050@smail.iitm.ac.in, charuswathi112@gmail.com
--------------------------------------------------------------------------------------------------  
*/
//`include "Logger.bsv"
package normalize_fma_hierarchical;
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
`include "fpu_parameters.bsv"

`ifdef fpu_hierarchical 

/*doc:func: function for normalization after product in case of single precision.*/
function ActionValue#(Tuple3#(FloatingPoint#(8,23),Exception,Bit#(50))) normalize1_sp(Tuple2#(FloatingPoint#(8,23), Bit#(48)) operands)=actionvalue
	
   match{.din,.sfdin} = operands;
   Bit#(50) sfdiN = 0;
   FloatingPoint#(8,23) out = din;
   Bit#(2) guard = 0;
   Exception exc = defaultValue;
   //find exponent
   Int#(9) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));
   //counting MSB 0s of sfdin
   Bit#(48) inp = sfdin;
   Vector#(16, Bit#(1)) v1 = unpack(inp[47:32]);                        
   UInt#(6) result1=0;                                  
   Bool done1 = False;
   for( Integer p1 = 15; p1 >=0; p1 = p1 - 1)  begin
      if ((v1[p1] == 0)&&(!done1))  
         result1 = result1+1 ;
      else
         done1 = True;
   end
   UInt#(6) z0 = (result1);
   
   Vector#(16, Bit#(1)) v2 = unpack(inp[31:16]);                        
   UInt#(6) result2=0;                                   
   Bool done2 = False;
   for( Integer p2 = 15; p2 >=0; p2 = p2 - 1) begin
      if ((v2[p2] == 0)&&(!done2))  
         result2 = result2+1 ;
      else
         done2 = True;
   end
   UInt#(6) z1 = result2;
   
   Vector#(16, Bit#(1)) v3 = unpack(inp[15:0]);                        
   UInt#(6) result3=0;                                   
   Bool done3 = False;
   for( Integer p3 = 15; p3 >=0; p3 = p3 - 1) begin
      if ((v3[p3] == 0)&&(!done3))  
         result3 = result3+1 ;
      else
         done3 = True;
   end
   UInt#(6) z2 = result3;
   
   UInt#(6) zeros= (|inp[47:32]==1)?z0:((|inp[31:16]==1)?(z1+16):((|inp[15:0]==1)?(z2+32):48)); //MSB 0s of sfdin computed
   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      sfdiN = {sfdin,2'b0};
      guard = '1;
   end
   else begin
      if (zeros == 0) begin
         // carry, no sfd adjust necessary
	 if (out.exp == 0) begin
	    out.exp = 2;
	 end
	 else begin
            out.exp = out.exp + 1;
	 end

	 // carry bit
	 sfdin = sfdin << 1;
      end
      else if (zeros == 1) begin
         // already normalized
         if (out.exp == 0)
	    out.exp = 1;
         // carry, hidden bits
	 sfdin = sfdin << 2;
      end
      else if (zeros == fromInteger(valueOf(48))) begin
         // exactly zero
         out.exp = 0;
      end
      else begin
         // try to normalize
         Int#(9) shift = zeroExtend(unpack(pack(zeros - 1)));
	 Int#(9) maxshift = exp - fromInteger(minexp(out));
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
      sfdiN = {1'b0,getHiddenBit(out),sfdin};
      sfdin = sfdin << fromInteger(valueOf(23));
      guard[1] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;
      guard[0] = |sfdin;
   end

   return tuple3(out,exc,sfdiN);
endactionvalue;

/*doc:func: function for normalization after addition in case of single precision.*/
function ActionValue#(Tuple3#(FloatingPoint#(8,23),Bit#(3),Exception)) normalize2_sp( Tuple4#(Int#(10),FloatingPoint#(8,23), Bit#(50),RoundMode) operands)=actionvalue
	
   match{.exp_out,.din,.sfdin,.rmode} = operands;
   //let sfdiN = sfdin;
   FloatingPoint#(8,23) out = din;
   Bit#(3) guard = 0;
   Exception exc = defaultValue;
   Int#(9) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));  
   //counting MSB 0s of sfdin
   Bit#(50) inp = sfdin;
   Vector#(16, Bit#(1)) v1 = unpack(inp[49:34]);                        //counting MSB 0's of input
   UInt#(6)  result1=0;                                  
   Bool done1 = False;
   for( Integer p1 = 15; p1 >=0; p1 = p1 - 1) begin
      if ((v1[p1] == 0)&&(!done1))  
         result1 = result1+1 ;
      else
         done1 = True;
   end
   UInt#(6)  z0 = (result1);
   
   Vector#(16, Bit#(1)) v2 = unpack(inp[33:18]);                        
   UInt#(6)  result2=0;                                   
   Bool done2 = False;
   for( Integer p2 = 15; p2 >=0; p2 = p2 - 1) begin
      if ((v2[p2] == 0)&&(!done2))  
         result2 = result2+1 ;
      else
         done2 = True;
   end
   UInt#(6)  z1 = result2;
   
   Vector#(16, Bit#(1)) v3 = unpack(inp[17:2]);                        
   UInt#(6)  result3=0;                                   
   Bool done3 = False;
   for( Integer p3 = 15; p3 >=0; p3 = p3 - 1)  begin
      if ((v3[p3] == 0)&&(!done3))  
         result3 = result3+1 ;
      else
         done3 = True;
   end
   UInt#(6)  z2 = result3;
   
   Vector#(2, Bit#(1)) v4 = unpack(inp[1:0]);                        
   UInt#(6)  result4=0;                                   
   Bool done4 = False;
   for( Integer p4 = 1; p4 >=0; p4 = p4 - 1) begin
      if ((v4[p4] == 0)&&(!done4)) 
         result4 = result4+1 ;
      else
         done4 = True;
   end
   UInt#(6)  z3 = result4;
   
   UInt#(6) zeros= (|inp[49:34]==1)?z0:((|inp[33:18]==1)?(z1+16):((|inp[17:2]==1)?(z2+32):((|inp[1:0]==1)?(z3+48):50))); //MSB 0s of sfdin computed
   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      guard = '1;
      if(rmode == Rnd_Minus_Inf || rmode == Rnd_Zero || rmode == Rnd_Plus_Inf)
         exc.overflow = True;
      exc.inexact = True;
   end
   else begin
      if (zeros == 0) begin
         // carry, no sfd adjust necessary
         if (exp_out + 'd127 == 0) begin
	    out.exp = 2;
	    exp_out = 2;
	 end
	 else begin
	    out.exp = out.exp + 1;
	    exp_out = exp_out + 1;
	    if(exp_out > 127) begin
	       exc.overflow = True;
	       exc.inexact = True;
	    end
	 end

	 // carry bit
	 sfdin = sfdin << 1;
      end
      else if (zeros == 1) begin
      // already normalized
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
      //sfdiN = sfdin;
      sfdin = sfdin << fromInteger(23);
      guard[2] = truncateLSB(sfdin);
      sfdin = sfdin << 1;
      guard[1] = truncateLSB(sfdin);
      sfdin = sfdin << 1;
      guard[0] = |sfdin;
   end

   return tuple3(out,guard,exc);
endactionvalue;

/*doc:func: function for normalization after product in case of double precision.*/
function ActionValue#(Tuple3#(FloatingPoint#(11,52),Exception,Bit#(108))) normalize1_dp(Tuple2#(FloatingPoint#(11,52), Bit#(106)) operands)=actionvalue
	
   match{.din,.sfdin} = operands;
   Bit#(108) sfdiN = 0;
   FloatingPoint#(11,52) out = din;
   Bit#(2) guard = 0;
   Exception exc = defaultValue;
   Int#(12) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));
 
   //counting MSB 0s of sfdin
   Bit#(106) inp = sfdin;
   Vector#(16, Bit#(1)) v1 = unpack(inp[105:90]);                       
   UInt#(7)  result1=0;                                  
   Bool done1 = False;
   for( Integer p1 = 15; p1 >=0; p1 = p1 - 1) begin
      if ((v1[p1] == 0)&&(!done1))     
         result1 = result1+1 ;
      else
         done1 = True;
   end
   UInt#(7) z0 = (result1);
   
   Vector#(16, Bit#(1)) v2 = unpack(inp[89:74]);                        
   UInt#(7) result2=0;                                   
   Bool done2 = False;
   for( Integer p2 = 15; p2 >=0; p2 = p2 - 1) begin
      if ((v2[p2] == 0)&&(!done2)) 
         result2 = result2+1 ;
      else
         done2 = True;
   end
   UInt#(7) z1 = result2;
   
   Vector#(16, Bit#(1)) v3 = unpack(inp[73:58]);                        
   UInt#(7) result3=0;                                   
   Bool done3 = False;
   for( Integer p3 = 15; p3 >=0; p3 = p3 - 1) begin
      if ((v3[p3] == 0)&&(!done3))  
         result3 = result3+1 ;
      else
         done3 = True;
   end
   UInt#(7) z2 = result3;
   
   Vector#(16, Bit#(1)) v4 = unpack(inp[57:42]);                        
   UInt#(7) result4=0;                                   
   Bool done4 = False;
   for( Integer p4 = 15; p4 >=0; p4 = p4 - 1) begin
      if ((v4[p4] == 0)&&(!done4))  
         result4 = result4+1 ;
      else
         done4 = True;
   end
   UInt#(7) z3 = result4;

   Vector#(16, Bit#(1)) v5 = unpack(inp[41:26]);                        
   UInt#(7) result5=0;                                   
   Bool done5 = False;
   for( Integer p5 = 15; p5 >=0; p5 = p5 - 1) begin
      if ((v5[p5] == 0)&&(!done5))  
         result5 = result5+1 ;
      else
         done5 = True;
   end
   UInt#(7) z4 = result5;

   Vector#(16, Bit#(1)) v6 = unpack(inp[25:10]);                        
   UInt#(7) result6=0;                                   
   Bool done6 = False;
   for( Integer p6 = 15; p6 >=0; p6 = p6 - 1) begin
      if ((v6[p6] == 0)&&(!done6))  
         result6 = result6+1 ;
      else
         done6 = True;
     end
   UInt#(7) z5 = result6;

   Vector#(10, Bit#(1)) v7 = unpack(inp[9:0]);                        
   UInt#(7) result7=0;                                   
   Bool done7 = False;
   for( Integer p7 = 9; p7 >=0; p7 = p7 - 1)  begin
      if ((v7[p7] == 0)&&(!done7))  
         result7 = result7+1 ;
      else
         done7 = True;
   end
   UInt#(7) z6 = result7;
   //MSB 0s of sfdin computed
   UInt#(7) zeros= (|inp[105:90]==1)?z0:((|inp[89:74]==1)?(z1+16):((|inp[73:58]==1)?(z2+32):((|inp[57:42]==1)?(z3+48):((|inp[41:26]==1)?(z4+64):((|inp[25:10]==1)?(z5+80):((|inp[9:0]==1)?(z6+96):106))))));

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      sfdiN = {sfdin,2'b0};
      guard = '1;
   end
   else begin
      if (zeros == 0) begin      
         // carry, no sfd adjust necessary
         if (out.exp == 0) begin
	    out.exp = 2;
	 end
	 else  begin
            out.exp = out.exp + 1;
	 end
         // carry bit
	 sfdin = sfdin << 1;
      end
      else if (zeros == 1) begin
         // already normalized
	 if (out.exp == 0)
	    out.exp = 1;
	    // carry, hidden bits
	    sfdin = sfdin << 2;
	 end
	 else if (zeros == fromInteger(valueOf(106))) begin
            // exactly zero
	    out.exp = 0;
	 end
	 else begin
            // try to normalize
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
      sfdiN = {1'b0,getHiddenBit(out),sfdin};
      sfdin = sfdin << fromInteger(valueOf(52));
      guard[1] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;
      guard[0] = |sfdin;

   end

   return tuple3(out,exc,sfdiN);
endactionvalue;

/*doc:func: function for normalize after addition in case of double precision.*/
function ActionValue#(Tuple3#(FloatingPoint#(11,52),Bit#(3),Exception)) normalize2_dp( Tuple4#(Int#(13),FloatingPoint#(11,52), Bit#(108),RoundMode) operands)=actionvalue
	
   match{.exp_out,.din,.sfdin,.rmode} = operands;
   //let sfdiN = sfdin;
   FloatingPoint#(11,52) out = din;
   Bit#(3) guard = 0;
   Exception exc = defaultValue;
   Int#(12) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));
   //counting MSB 0s of sfdin
   Bit#(108) inp = sfdin;
   Vector#(16, Bit#(1)) v1 = unpack(inp[107:92]);                        
   UInt#(7) result1=0;                                  
   Bool done1 = False;
   for( Integer p1 = 15; p1 >=0; p1 = p1 - 1) begin
      if ((v1[p1] == 0)&&(!done1))  
         result1 = result1+1 ;
      else
         done1 = True;
   end
   UInt#(7) z0 = (result1);
   
   Vector#(16, Bit#(1)) v2 = unpack(inp[91:76]);                        
   UInt#(7) result2=0;                                   
   Bool done2 = False;
   for( Integer p2 = 15; p2 >=0; p2 = p2 - 1)  begin
      if ((v2[p2] == 0)&&(!done2))  
         result2 = result2+1 ;
      else
         done2 = True;
   end
   UInt#(7) z1 = result2;
   
   Vector#(16, Bit#(1)) v3 = unpack(inp[75:60]);                        
   UInt#(7) result3=0;                                   
   Bool done3 = False;
   for( Integer p3 = 15; p3 >=0; p3 = p3 - 1) begin
      if ((v3[p3] == 0)&&(!done3))  
         result3 = result3+1 ;
      else
         done3 = True;
     end
   UInt#(7) z2 = result3;
   
   Vector#(16, Bit#(1)) v4 = unpack(inp[59:44]);                       
   UInt#(7) result4=0;                                   
   Bool done4 = False;
   for( Integer p4 = 15; p4 >=0; p4 = p4 - 1) begin
      if ((v4[p4] == 0)&&(!done4))  
         result4 = result4+1 ;
      else
         done4 = True;
   end
   UInt#(7) z3 = result4;

   Vector#(16, Bit#(1)) v5 = unpack(inp[43:28]);                        
   UInt#(7) result5=0;                                   
   Bool done5 = False;
   for( Integer p5 = 15; p5 >=0; p5 = p5 - 1)  begin
      if ((v5[p5] == 0)&&(!done5))  
         result5 = result5+1 ;
      else
         done5 = True;
   end
   UInt#(7) z4 = result5;

   Vector#(16, Bit#(1)) v6 = unpack(inp[27:12]);                       
   UInt#(7) result6=0;                                   
   Bool done6 = False;
   for( Integer p6 = 15; p6 >=0; p6 = p6 - 1) begin
      if ((v6[p6] == 0)&&(!done6))  
         result6 = result6+1 ;
      else
         done6 = True;
   end
   UInt#(7) z5 = result6;

   Vector#(12, Bit#(1)) v7 = unpack(inp[11:0]);                        
   UInt#(7) result7=0;                                   
   Bool done7 = False;
   for( Integer p7 = 11; p7 >=0; p7 = p7 - 1) begin
      if ((v7[p7] == 0)&&(!done7))  
         result7 = result7+1 ;
      else
         done7 = True;
   end
   UInt#(7) z6 = result7;
   
   //MSB 0s of sfdin computed
   UInt#(7) zeros= (|inp[107:92]==1)?z0:((|inp[91:76]==1)?(z1+16):((|inp[75:60]==1)?(z2+32):((|inp[59:44]==1)?(z3+48):((|inp[43:28]==1)?(z4+64):((|inp[27:12]==1)?(z5+80):((|inp[11:0]==1)?(z6+96):108))))));

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      guard = '1;
      if(rmode == Rnd_Minus_Inf || rmode == Rnd_Zero || rmode == Rnd_Plus_Inf)
         exc.overflow = True;
      exc.inexact = True;
   end
   else begin
      if (zeros == 0) begin
			 
         // carry, no sfd adjust necessary
	 if (exp_out + 'd1023 == 0) begin
	    out.exp = 2;
	    exp_out = 2;
	 end
	 else begin
	    out.exp = out.exp + 1;
	    exp_out = exp_out + 1;
	    if(exp_out > 1023) begin
	       exc.overflow = True;
	       exc.inexact = True;
	    end
	 end

	 // carry bit
	 sfdin = sfdin << 1;
      end
      else if (zeros == 1) begin
         // already normalized
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
      //sfdiN = sfdin;
      sfdin = sfdin << fromInteger(52);
      guard[2] = truncateLSB(sfdin);
      sfdin = sfdin << 1;
      guard[1] = truncateLSB(sfdin);
      sfdin = sfdin << 1;
      guard[0] = |sfdin;
   end
   return tuple3(out,guard,exc);
endactionvalue;

`endif

endpackage

