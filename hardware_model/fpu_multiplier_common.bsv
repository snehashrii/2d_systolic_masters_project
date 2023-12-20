////////////////////////////////////////////////////////////////////////////////
//see LICENSE.iitm
////////////////////////////////////////////////////////////////////////////////
/*
---------------------------------------------------------------------------------------------------

Author: Joyanta Mondal, Nagakaushik Moturi
Email id: ee19m058@smail.iitm.ac.in, ee17b111@smail.iitm.ac.in
Details: Common fpu multiplier functions
--------------------------------------------------------------------------------------------------
*/
package fpu_multiplier_common;

import Vector::*;
import DefaultValue      ::*;
import fpu_common ::*;



/*******************Single Precision Functions****************/ 

function Tuple4#(Bit#(8),Int#(32), Bit#(5),Bit#(2) ) fn_find_exp_sp(Bit#(8) e1,Bit#(8) e2,Bit#(23) m1,Bit#(23) m2); 

	 let e11 = (|e1==1'b0)?8'd1:e1;                    // evaluating the exponent 
	 let e22 = (|e2==1'b0)?8'd1:e2;
	 Bit#(10) e_0= zeroExtend(e1)+zeroExtend(e2)+negate(10'd127); 
	 Bit#(10) e= zeroExtend(e11)+zeroExtend(e22)+negate(10'd127);
	 Bit#(2) z =0;

		 

	 Int#(32) shift = unpack(zeroExtend(negate(e[8:0])));
		Int#(32) shift_m =((e[9]==1'b1)&&(shift <= 47))?shift:0;
	 Bit#(5) ex =0;                                      
	 ex[3]=1'b0;
	 ex[0] = 1'b0;  
		                
	 ex[4] = ((&e1==1'b1)&&(|m1==1'b1))||((&e2==1'b1)&&(|m2==1'b1))?1'b1:1'b0;   //invalid opeartion flag or NaN_detect
	 ex[2] = ((e[9]==1'b0)&&(e[8:0]>9'd254)?1'b1:1'b0); 
		 
	 let p = (e[9]==1'b0)&&(e[8:0]>9'd254)?10'd254:e;
	 let res_exp = (e[9]==1'b1)?8'd0:p[7:0];
		z[0] = (((|e1==1'b0)&&(|m1==1'b0))||((|e2==1'b0)&&(|m2==1'b0)))?1'b1:1'b0;
	 ex[1] = (z[0]==1'b1)?1'b0:pack((e[9]==1'b1)); 
	 z[1] = pack((|e==1'b0));

	 return tuple4(res_exp,shift_m,ex,z);
endfunction


function Bit#(48) fn_add_sp(Bit#(48) x1,Bit#(48) x2,Bit#(48) x3,Bit#(48) x4);
	 let x5 = x1+x2+x3+x4;             //generating product from partial products
	 return x5;
endfunction

function Tuple4#(Bit#(48),Bit#(8),Bit#(5),Bit#(2)) fn_norm_sp(Bit#(48) x5,Bit#(8) exp,Int#(32) shift,Bit#(5) exc, Bit#(2) z);

	 let ex= exc;
	 Int#(32) max_shift = unpack(zeroExtend(exp-8'd1));




	//----------output--------------//


		 

	 Vector#(16, Bit#(1)) v1 = unpack(x5[47:32]);                        //counting MSB 0's of input[47:32]
  Integer result1=0;                                  
  Bool done1 = False;
  for( Integer p1 = 15; p1 >=0; p1 = p1 - 1) 
    begin
      if ((v1[p1] == 0)&&(!done1))  result1 = result1+1 ;
      else
        done1 = True;
    end
  //Integer i0 = (done1==True)?(16-result1):0;
  Integer z0 = (result1);

  Vector#(16, Bit#(1)) v2 = unpack(x5[31:16]);                        //counting MSB 0's of input[31:16]
  Integer result2=0;                                   
  Bool done2 = False;
  for( Integer p2 = 15; p2 >=0; p2 = p2 - 1) 
    begin
      if ((v2[p2] == 0)&&(!done2))  result2 = result2+1 ;
      else
        done2 = True;
    end
  //Integer i1 = (done2==True)?(16-result2):0;
  Integer z1 = result2;
		  
  Vector#(16, Bit#(1)) v3 = unpack(x5[15:0]);                        //counting MSB 0's of input[15:0]
  Integer result3=0;                                   
  Bool done3 = False;
  for( Integer p3 = 15; p3 >=0; p3 = p3 - 1) 
    begin
      if ((v3[p3] == 0)&&(!done3))  result3 = result3+1 ;
      else
        done3 = True;
    end
  //Integer i2 = (done3==True)?(16-result3):0;
  Integer z2 = result3;
		  

		  
		Integer i0 = (|x5[47:32]==1)?z0:((|x5[31:16]==1)?(z1+16):((|x5[15:0]==1)?(z2+32):48));  //counting total MSB Zeros

  Bit#(1) norm_valid = (|exp==1'b0)?1'b0:pack((i0-1)>0); 
  Bit#(48) x01 = (|exp==1'b1)?x5:((exc[1]==1'b0)?(x5>>1):((shift==0)?48'd0:(x5>>(shift+1)))); 
  Bit#(48) x02 = (fromInteger(i0)>max_shift)?x5<<(max_shift):x5<<(i0-1);
  let exp1 = ((exp == 8'hFE)?8'hFE:exp+8'd1);
  let exp2 = (fromInteger(i0-1)>max_shift)?(8'd0):((exp-fromInteger(i0)+8'd1));
  let x_out0 = (norm_valid==1'b1)?x02:x01;      //finding output
  let ex1 = ((z[0]==1'b1)?1'b0:((|exp==1'b1)?1'b0:exc[1]));           
  ex[2] = ((exp==8'hFE)&&(i0==0))?1'b1:exc[2];           //overflow flags
  let x_out1 = (ex[2]==1'b1)?48'h7fffffffffff:x_out0;

  let exp_out = (ex1==1'b1)?(8'd0):((i0==0)?exp1:((norm_valid==1'b1)?exp2:(exp)));
  let x_out = ((ex1==1'b1)&&(shift==0))?48'd0:((ex[4]==1'b1)?48'h600000000000:x_out1);

  //--------------------Flags---------------------------//
  //------------underflow---------------//

  Bit#(1) sfd_prec = (exc[1]==1'b1)&&(shift!=0)?|(x5<<(48-shift)):(((exc[1]==1'b1)&&(shift==0))?1'b1:1'b0);
  let x7 = x5>>(shift+1);
  let x8 = x5>>(shift);
  x7[0] = x7[0]|sfd_prec;
  Bit#(2) guard = 0;
  Bit#(2) guard_s = 0;
  Bit#(2) guard_out = 0;
  guard[1] = x7[22];
  guard[0] = |x7[21:0];
  guard_s[1] = x8[22];
  guard_s[0] = |x8[21:0];
  
  let ex12 = (|exp_out==1'b0)&&(guard_s!=0)&&(|x_out[22:0]==1'b0)?1'b0:1'b1;
  let ex11 = (((|x_out[22:0]==1'b0)&&((exc[1]==1'b0)))?1'b0:(((|exp_out==1'b0)&&(guard!=0))?(1'b1):(1'b0)));
  let ex2 = (|exp_out==1'b0)&&(guard_s!=0)?(1'b1):ex11;
  ex[1]=(fromInteger(i0-1)>max_shift)&&(|x02[22:0]==1'b0)?1'b0:ex2;

  //-----------------Inexact Flags--------------//
  Bit#(1) ex00 = ~((|exp_out)&(x_out[47]));
  let ex01 = (((|exp==1'b0)&&(|exp_out==1'b1))?((|x5[22:0]==1'b1)?1'b1:1'b0):1'b0);
  ex[0] =ex01;

  //------------------Guards for rounding---------------------------//
  Bit#(1) grd = (shift>0)?|(x5<<(48-shift)):1'b0;
  Bit#(48) sfd_res = x5>>shift;
  sfd_res[0] = sfd_res[0]|grd;
  if(|exp == 1'b0)  x_out[0] = sfd_res[0];
  guard_out[1] = (norm_valid==1'b1)?1'b0:(sfd_res[22]);
  guard_out[0] = (norm_valid==1'b1)?1'b0:((sfd_prec)|(|sfd_res[21:0]));
  return tuple4(x_out,exp_out,ex,guard_out);
endfunction



function Bit#(3) fn_Special_EX_sp(Bit#(8) e1,Bit#(8) e2,Bit#(23) m1,Bit#(23) m2);

	 Bit#(1) a_inf=pack((&e1==1'b1)&&(|m1==1'b0));
	 Bit#(1) b_inf=pack((&e2==1'b1)&&(|m2==1'b0));
	 Bit#(1) a_z=pack((|e1==1'b0)&&(|m1==1'b0));
	 Bit#(1) b_z=pack((|e2==1'b0)&&(|m2==1'b0));
	 Bit#(1) a_snan=pack((&e1==1'b1)&&(m1[22]==1'b0)&&(|m1[21:0]==1'b1));
	 Bit#(1) b_snan=pack((&e2==1'b1)&&(m2[22]==1'b0)&&(|m2[21:0]==1'b1));
	 Bit#(1) snan_valid = pack((a_z&b_inf)|(a_inf&b_z)|(a_z&b_snan)|(a_snan&b_z)|(b_inf&a_snan)|(a_inf&b_snan));
	 Bit#(1) snan = ((a_z|b_z)==1'b0)?(a_snan|b_snan):snan_valid;


	 Bit#(1) a_qnan=pack((&e1==1'b1)&&(m1[22]==1'b1));
	 Bit#(1) b_qnan=pack((&e2==1'b1)&&(m2[22]==1'b1));
	 Bit#(1) qnan_valid = pack((a_z&b_qnan)|(a_qnan&b_z)|(a_inf&b_qnan)|(b_inf&a_qnan));
	 Bit#(1) qnan = (snan==1'b1)?1'b0:(((a_z|b_z)==1'b0)?(a_qnan|b_qnan):qnan_valid);


	 Bit#(1) inf = (snan==1'b1)||(qnan==1'b1)?1'b0:(a_inf|b_inf);
	 Bit#(1) zero = (snan==1'b1)||(qnan==1'b1)?1'b0:(a_z|b_z);
	 Bit#(1) nan = (snan==1'b1)?snan:qnan;

	 Bit#(3) res = (qnan==1'b1)?3'b111:{nan,inf,zero};
	 return res;
endfunction


function Bit#(48) fn_gen_pp_sp(Bit#(24) m,Bit#(6) b);                    //generates the partial products for mantissa multiplication
	 Bit#(30) y=0;
	 Bit#(30) a=zeroExtend(m);
	 Bit#(30) res1=0;
	 Bit#(30) res2=0;
	 Bit#(30) res3=0;
	 Bit#(30) res4=0;
	 Bit#(30) res6=0;
	 Bit#(30) res5=0;
	 res1 = (b[0]==1'b1)?(a):30'd0;
	 res2 =  (b[1]==1'b1)?(a<<1):30'd0;
	 res3 =  (b[2]==1'b1)?(a<<2):30'd0;
	 res4 =  (b[3]==1'b1)?(a<<3):30'd0;
	 res5 =  (b[4]==1'b1)?(a<<4):30'd0;
	 res6 =  (b[5]==1'b1)?(a<<5):30'd0;
	 y =  res1+res2+res3+res4+res5+res6;
	 return zeroExtend(y);          // extending the result
endfunction




function Exception fn_setEx(Reg#(Exception) a,Int#(32) i);                  //  zero,sNan,infinity detection

	 Exception y=defaultValue;
	 case (i)
	   0: y = defaultValue;
	   1: y = Exception{invalid_op :True, divide_0 : False , overflow : False , underflow : False, inexact : False};
	   2: y = Exception{invalid_op : a.invalid_op , divide_0 : False , overflow : True , underflow : a.underflow, inexact : True};
	   3: y = Exception{invalid_op : a.invalid_op , divide_0 : False , overflow : a.overflow , underflow : True, inexact : True};
	   4: y = Exception{invalid_op : a.invalid_op , divide_0 : False , overflow : a.overflow , underflow : a.underflow, inexact : True};
	   5: y = Exception{invalid_op : a.invalid_op , divide_0 : False , overflow : a.overflow , underflow : a.underflow, inexact : a.inexact};
	   default: y = defaultValue;
	 endcase

	 return y;
endfunction

/*******Round_Off Functions(Single precision)*********/



function Tuple2#(FloatingPoint#(8,23),Exception) fn_rnd_Nearest_Even_sp(Reg#(Exception) exc,Bit#(1) sgn,Bit#(8) exp,Bit#(48) x,Bit#(5) excb,Bit#(2) guard1);
//round to nearest even

	 Bit#(23) y=0;                                          
	 Bit#(8) e= 0;
	 Exception ex= defaultValue;
	 Bit#(23) sfd_out =0;
	 Bit#(8) exp_out=0;
	 Bit#(3) guard = (x[47]==1'b1)?{x[23],x[22],|x[21:0]}:{x[22],x[21],|x[20:0]};
	 let sfd = (x[47]==1'b1)?x[46:24]:x[45:23];
	 let sfd_lsb = (x[47]==1'b1)?|x[23:0]:|x[22:0];
	 Bit#(25) sfd_res = {1'b0, |exp, sfd} + 25'd1; 

	 Bit#(1) sfdlsb = (x[47]==1'b1)?x[24]:x[23]; 
	 if (sfd_res[24] == 1'b1) begin
			 if (exp == 8'hFE) begin
		    exp_out=8'hFF;
		    sfd_out = 23'd0;
			 end
			 else begin
			   exp_out = exp + 8'd1;
			   sfd_out = truncate(sfd_res>> 1);
			 end
	 end
		  else if ((exp == 0) && (truncateLSB(sfd_res) == 2'b01)) begin
		    exp_out = 8'd1;
		    sfd_out = truncate(sfd_res);
		      end
	 else begin
		  exp_out = exp ;
		  sfd_out = truncate(sfd_res);
	 end
	 Bit#(1) e_xu=0;
	 Bit#(1) e_xo=0;
	 if (excb[4] == 1'b1) begin
	   y = truncate(24'h400000);
	   e = 8'hFF;
	 end
	 else begin
    case (guard)
      3'b000 : begin   y=sfd;e=exp;  end
      3'b001 : begin   y=sfd;e=exp;  end
      3'b010 : begin   y=sfd;e=exp;  end
      3'b011 : begin   y=sfd;e=exp;  end
      3'b100 : begin if (sfdlsb==1'b1)
       begin   
         y = sfd_out; e = exp_out; /*e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(|x[21:0]==1'b1)?1'b1:1'b0; */
         e_xo = (exp==8'hFE)&&(&sfd==1'b1)&&(|x[21:0]==1'b1)?1'b1:1'b0;         
       end  
       else if ((pack(sfdlsb==1'b0)&pack(guard1!=0)&pack(exp==0))==1'b1) begin
         y = sfd_out; e = exp_out; /*e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(|x[21:0]==1'b1)?1'b1:1'b0; */
         e_xo = (exp==8'hFE)&&(&sfd==1'b1)&&(|x[22:0]==1'b1)?1'b1:1'b0; end
       else begin    y=sfd;e=exp; end end
     
    
      3'b101 : begin   y = sfd_out; e = exp_out; /*e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(|x[21:0]==1'b1)?1'b1:1'b0; */ //check
      e_xo = (exp==8'hFE)&&(&sfd==1'b1)&&(|x[22:0]==1'b1)?1'b1:1'b0;            end   
      3'b110 : begin   y = sfd_out; e = exp_out; e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(&guard[2:1]==1'b1)?1'b1:1'b0; //check
      e_xo = (exp==8'hFE)&&(&sfd==1'b1)&&(|x[22:0]==1'b1)?1'b1:1'b0;            end   
      3'b111 : begin   y = sfd_out; e = exp_out; e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(&guard[2:1]==1'b1)?1'b1:1'b0; //check
        e_xo = (exp==8'hFE)&&(&sfd==1'b1)&&(|x[22:0]==1'b1)?1'b1:1'b0;            end   
    endcase
			   
			   
	 end

	 ex = (excb[4]==1'b1)?fn_setEx(exc,0):((excb[2]==1'b1)?fn_setEx(exc,2):((excb[1]==1'b1)?((e_xu==1'b1)?(fn_setEx(exc,4)):fn_setEx(exc,3)):(((excb[0]==1'b1)||(guard!=3'b000))?((e_xo==1'b1)?(fn_setEx(exc,2)):fn_setEx(exc,4)):(fn_setEx(exc,0)))));

	 return tuple2(FloatingPoint{sign: unpack(sgn),exp: e,sfd: y},ex);
endfunction


function Tuple2#(FloatingPoint#(8,23),Exception) fn_rnd_Nearest_Away_Zero_sp(Reg#(Exception) exc,Bit#(1) sgn,Bit#(8) exp,Bit#(48) x,Bit#(5) excb,Bit#(2) guard1);     //round to nearest away zero
	 Bit#(23) y=0;                                          
	 Bit#(8) e= 0;
	 Exception ex= defaultValue;
	 Bit#(23) sfd_out =0;
	 Bit#(8) exp_out=0;
	 Bit#(3) guard = (x[47]==1'b1)?{x[23],x[22],|x[21:0]}:{x[22],x[21],|x[20:0]};
	 let sfd = (x[47]==1'b1)?x[46:24]:x[45:23];
	 let sfd_lsb = (x[47]==1'b1)?|x[23:0]:|x[22:0];
	 Bit#(25) sfd_res = {1'b0, |exp, sfd} + 25'd1; 

	 if (sfd_res[24] == 1'b1) begin
			 if (exp == 8'hFE) begin
			   exp_out=8'hFF;
			   sfd_out = 23'd0;
			 end
			 else begin
			   exp_out = exp + 8'd1;
			   sfd_out = truncate(sfd_res>> 1);
			 end
	 end
		else if ((exp == 0) && (truncateLSB(sfd_res) == 2'b01)) begin
		  exp_out = 8'd1;
		  sfd_out = truncate(sfd_res);
	 end
	 else begin
		  exp_out = exp ;
		  sfd_out = truncate(sfd_res);
	 end
	 Bit#(1) e_xu=0;
	 Bit#(1) e_xo=0;
	 if (excb[4] == 1'b1) begin
	 y = truncate(24'h400000);
	 e = 8'hFF;
	 end
	 else begin
    case (guard) matches
      3'b0?? : begin   y=sfd;e=exp;  end
      3'b1?? : begin   y = sfd_out; e = exp_out; e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(guard[1]==1'b1)?1'b1:1'b0; 
      e_xo = (exp==8'hFE)&&(&sfd==1'b1)&&(|x[22:0]==1'b1)?1'b1:1'b0;            end      
    endcase
			   
	 end

	 ex = (excb[4]==1'b1)?fn_setEx(exc,0):((excb[2]==1'b1)?fn_setEx(exc,2):((excb[1]==1'b1)?((e_xu==1'b1)?(fn_setEx(exc,4)):fn_setEx(exc,3)):(((excb[0]==1'b1)||(guard!=3'b000))?((e_xo==1'b1)?(fn_setEx(exc,2)):fn_setEx(exc,4)):(fn_setEx(exc,0)))));

	 return tuple2(FloatingPoint{sign: unpack(sgn),exp: e,sfd: y},ex);
endfunction


function Tuple2#(FloatingPoint#(8,23),Exception) fn_rnd_Plus_Inf_sp(Reg#(Exception) exc,Bit#(1) sgn,Bit#(8) exp,Bit#(48) x,Bit#(5) excb,Bit#(2) guard1);       //round to plus infinity
	 Bit#(23) y=0;                                          
	 Bit#(8) e= 0;
	 Exception ex= defaultValue;
	 Bool tinycheck_underflow = False;
	 Bit#(23) sfd_out =0;
	 Bit#(8) exp_out=0;
	 Bit#(3) guard = (x[47]==1'b1)?{x[23],x[22],|x[21:0]}:{x[22],x[21],|x[20:0]};
	 Bit#(2) guard2 = {guard[2], guard[1]|guard[0]};
	 let sfd = (x[47]==1'b1)?x[46:24]:x[45:23];
	 let sfd_lsb = (x[47]==1'b1)?|x[23:0]:|x[22:0];
	 Bit#(25) sfd_res = {1'b0, |exp, sfd} + 25'd1; 
  
   /*if (guard != 0) begin
	 exc.inexact = True;
      end*/
	 if (sfd_res[24] == 1'b1) begin
			 if (exp == 8'hFE) begin
			   exp_out=8'hFF;
			   sfd_out = 23'd0;
			 end
			 else begin
			   exp_out = exp + 8'd1;
			   sfd_out = truncate(sfd_res>> 1);
			 end
	 end
		else if ((exp == 0) && (truncateLSB(sfd_res) == 2'b01)) begin
	   exp_out = 8'd1;
 		 sfd_out = truncate(sfd_res);
 		 if ((sgn==1'b0) && ((guard2 == 'b10) || (guard2 == 'b01))) begin   // mark tiny
         tinycheck_underflow = True;
         end
  end
	 else begin
		  exp_out = exp ;
		  sfd_out = truncate(sfd_res);
	 end
	 Bit#(1) e_xu=0;
	 Bit#(1) e_xo=0;
	 if (excb[4] == 1'b1) begin
	   y = truncate(24'h400000);
	   e = 8'hFF;
	 end
	 else begin
    if (guard == 0) begin 
      y = (guard1!=0)&&(sgn==1'b0)?(sfd_out):sfd; 
      e= (y==sfd_out)&&(sfd_res[23]!=0)&&(exp==0)?exp+8'd1:exp;
    end
    else if (sgn==1'b0) begin
      y = sfd_out;
      e = exp_out;
      e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(guard[2]==1'b1)&&(|guard[1:0]==1'b1)?1'b1:1'b0;
      e_xo = (exp==8'hFE)&&(&sfd==1'b1)&&(|x[22:0]==1'b1)?1'b1:1'b0;
    end
    else begin
      y=sfd;
      e=exp;
    end

  end

	 ex = (excb[4]==1'b1)?fn_setEx(exc,0):((excb[2]==1'b1)?fn_setEx(exc,2):((excb[1]==1'b1)?((e_xu==1'b1)?(fn_setEx(exc,4)):fn_setEx(exc,3)):(((excb[0]==1'b1)||(guard!=3'b000))?((e_xo==1'b1)?(fn_setEx(exc,2)):fn_setEx(exc,4)):(fn_setEx(exc,0)))));
	if (tinycheck_underflow) ex.underflow = True;
	 return tuple2(FloatingPoint{sign: unpack(sgn),exp: e,sfd: y},ex);
endfunction


function Tuple2#(FloatingPoint#(8,23),Exception) fn_rnd_Minus_Inf_sp(Reg#(Exception) exc,Bit#(1) sgn,Bit#(8) exp,Bit#(48) x,Bit#(5) excb,Bit#(2) guard1);     	//round to minus infinity
  Bit#(23) y=0;                                          
  Bit#(8) e= 0;
  Exception ex= defaultValue;
  Bool tinycheck_underflow = False;
  Bit#(23) sfd_out =0;
  Bit#(8) exp_out=0;
  Bit#(3) guard = (x[47]==1'b1)?{x[23],x[22],|x[21:0]}:{x[22],x[21],|x[20:0]};
	Bit#(2) guard2 = {guard[2], guard[1]|guard[0]};
  let sfd = (x[47]==1'b1)?x[46:24]:x[45:23];
  let sfd_lsb = (x[47]==1'b1)?|x[23:0]:|x[22:0];
  Bit#(25) sfd_res = {1'b0, |exp, sfd} + 25'd1; 

  if (sfd_res[24] == 1'b1) begin
		  if (exp == 8'hFE) begin
		    exp_out=8'hFF;
		    sfd_out = 23'd0;
		  end
		  else begin
		    exp_out = exp + 8'd1;
		    sfd_out = truncate(sfd_res>> 1);
		  end
  end
	 else if ((exp == 0) && (truncateLSB(sfd_res) == 2'b01)) begin
	   exp_out = 8'd1;
	   sfd_out = truncate(sfd_res);
	   if (unpack(sgn) && ((guard2 == 'b10) || (guard2 == 'b01))) begin  // mark tiny
         tinycheck_underflow = True;
         end
	       end
  else begin
	   exp_out = exp ;
	   sfd_out = truncate(sfd_res);
  end
  Bit#(1) e_xu=0;
  Bit#(1) e_xo=0;
  if (excb[4] == 1'b1) begin
    y = truncate(24'h400000);
    e = 8'hFF;
  end
  else begin
    if (guard == 0) begin 
      y = (guard1!=0)&&(sgn==1'b1)?(sfd_out):sfd; 
      e= (y==sfd_out)&&(sfd_res[23]!=0)&&(exp==0)?exp+8'd1:exp;
    end
    else if (sgn==1'b1) begin
      y = sfd_out;
      e = exp_out;
      e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(guard[2]==1'b1)&&(|guard[1:0]==1'b1)?1'b1:1'b0;
      e_xo = (exp==8'hFE)&&(&sfd==1'b1)&&(|x[22:0]==1'b1)?1'b1:1'b0;
    end
    else begin
      y=sfd;
      e=exp;
    end    
  end

  ex = (excb[4]==1'b1)?fn_setEx(exc,0):((excb[2]==1'b1)?fn_setEx(exc,2):((excb[1]==1'b1)?((e_xu==1'b1)?(fn_setEx(exc,4)):fn_setEx(exc,3)):(((excb[0]==1'b1)||(guard!=3'b00))?((e_xo==1'b1)?(fn_setEx(exc,2)):fn_setEx(exc,4)):(fn_setEx(exc,0)))));
  if (tinycheck_underflow) ex.underflow = True;
  return tuple2(FloatingPoint{sign: unpack(sgn),exp: e,sfd: y},ex);
endfunction


function Tuple2#(FloatingPoint#(8,23),Exception) fn_rnd_Zero_sp(Reg#(Exception) exc,Bit#(1) sgn,Bit#(8) exp,Bit#(48) x,Bit#(5) excb,Bit#(2) guard);                //round to zero
  Bit#(23) y=0;                                          
  Bit#(8) e= 0;
  Exception ex= defaultValue;
  let y1 = (x[47]==1'b1)?pack(|x[23:0]==1'b0):pack(|x[22:0]==1'b0);
  let exp1 = (exp == 8'hFE)?8'hFE:exp+8'd1;


  if (excb[4] == 1'b1) begin
    y = truncate(24'h400000);
    e = 8'hFF;
  end

  else begin
	   y= (x[47]==1'b1)?x[46:24]:x[45:23];                                          
	   e= exp; 
  end
  ex = (excb[4]==1'b1)?fn_setEx(exc,0):((excb[2]==1'b1)?fn_setEx(exc,2):((excb[1]==1'b1)?fn_setEx(exc,3):(((excb[0]==1'b1)||(y1==1'b0))?(fn_setEx(exc,4)):(fn_setEx(exc,0)))));
  return tuple2(FloatingPoint{sign: unpack(sgn),exp: e,sfd: y},ex);
endfunction








/*******************Double Precision Functions****************/ 
















function Tuple4#(Bit#(11),Int#(32), Bit#(5),Bit#(2) ) fn_find_exp_dp(Bit#(11) e1,Bit#(11) e2,Bit#(52) m1,Bit#(52) m2); 

	 let e11 = (|e1==1'b0)?11'd1:e1;                    // evaluating the exponent 
	 let e22 = (|e2==1'b0)?11'd1:e2;
	 Bit#(13) e_0= zeroExtend(e1)+zeroExtend(e2)+negate(13'd1023); 
	 Bit#(13) e= zeroExtend(e11)+zeroExtend(e22)+negate(13'd1023);
	 Bit#(2) z =0;

		 

	 Int#(32) shift = unpack(zeroExtend(negate(e[11:0])));
		Int#(32) shift_m =((e[12]==1'b1)&&(shift <= 105))?shift:0;
	 Bit#(5) ex =0;                                      
	 ex[3]=1'b0;
	 ex[0] = 1'b0;  
		                
	 ex[4] = ((&e1==1'b1)&&(|m1==1'b1))||((&e2==1'b1)&&(|m2==1'b1))?1'b1:1'b0;   //invalid opeartion flag or NaN_detect
	 ex[2] = ((e[12]==1'b0)&&(e[11:0]>12'd2046)?1'b1:1'b0); //
		 
	 let p = (e[12]==1'b0)&&(e[11:0]>12'd2046)?13'd2046:e;
	 let res_exp = (e[12]==1'b1)?11'd0:p[10:0];
		z[0] = (((|e1==1'b0)&&(|m1==1'b0))||((|e2==1'b0)&&(|m2==1'b0)))?1'b1:1'b0;
	 ex[1] = (z[0]==1'b1)?1'b0:pack((e[12]==1'b1)); 
	 z[1] = pack((|e==1'b0));

	 return tuple4(res_exp,shift_m,ex,z);
endfunction


function Integer fn_count_zeros_dp(Bit#(108) prod,Bit#(11) exp); 
             
  let x5 = prod;                    //generating product from partial products



  //----------output--------------//
  /*
	  Vector#(108, Bit#(1)) v = unpack(x5);
  Integer result=0;
  Bool done = False;
	  for(Integer i = 0; i <106; i = i + 1) 
	  begin
	       if (v[i] == 1)  result = i+1 ;
	     else
	     done = True;
	  end
  Integer i0 = (done==True)?(106-result):0;
  */

  Vector#(10, Bit#(1)) v0 = unpack(x5[105:96]);                        //counting MSB 0's of input[47:32]
  Integer result0=0;                                  
  Bool done0 = False;
  for( Integer p0 = 9; p0 >=0; p0 = p0 - 1) 
	   begin
	     if ((v0[p0] == 0)&&(!done0))  result0 = result0+1 ;
	     else
	       done0 = True;
	   end
  //Integer i0 = (done1==True)?(16-result1):0;
  Integer z0 = (result0);

  Vector#(16, Bit#(1)) v1 = unpack(x5[95:80]);                        //counting MSB 0's of input[31:16]
  Integer result1=0;                                   
  Bool done1 = False;
  for( Integer p1 = 15; p1 >=0; p1 = p1 - 1) 
	   begin
	     if ((v1[p1] == 0)&&(!done1))  result1 = result1+1 ;
	     else
	       done1 = True;
	   end
  //Integer i1 = (done2==True)?(16-result2):0;
  Integer z1 = result1;

  Vector#(16, Bit#(1)) v2 = unpack(x5[79:64]);                        //counting MSB 0's of input[15:0]
  Integer result2=0;                                   
  Bool done2 = False;
  for( Integer p2 = 15; p2 >=0; p2 = p2 - 1) 
	   begin
	     if ((v2[p2] == 0)&&(!done2))  result2 = result2+1 ;
	     else
	       done2 = True;
	   end
  //Integer i2 = (done3==True)?(16-result3):0;
  Integer z2 = result2;

  Vector#(16, Bit#(1)) v3 = unpack(x5[63:48]);                        //counting MSB 0's of input[15:0]
  Integer result3=0;                                   
  Bool done3 = False;
  for( Integer p3 = 15; p3 >=0; p3 = p3 - 1) 
	   begin
	     if ((v3[p3] == 0)&&(!done3))  result3 = result3+1 ;
	     else
	       done3 = True;
	   end
  //Integer i2 = (done3==True)?(16-result3):0;
  Integer z3 = result3;



  Vector#(16, Bit#(1)) v4 = unpack(x5[47:32]);                        //counting MSB 0's of input[15:0]
  Integer result4=0;                                   
  Bool done4 = False;
  for( Integer p4 = 15; p4 >=0; p4 = p4 - 1) 
	   begin
	     if ((v4[p4] == 0)&&(!done4))  result4 = result4+1 ;
	     else
	       done4 = True;
	   end
  //Integer i2 = (done3==True)?(16-result3):0;
  Integer z4 = result4;


  Vector#(16, Bit#(1)) v5 = unpack(x5[31:16]);                        //counting MSB 0's of input[15:0]
  Integer result5=0;                                   
  Bool done5 = False;
  for( Integer p5 = 15; p5 >=0; p5 = p5 - 1) 
	   begin
	     if ((v5[p5] == 0)&&(!done5))  result5 = result5+1 ;
	     else
	       done5 = True;
	   end
  //Integer i2 = (done3==True)?(16-result3):0;
  Integer z5 = result5;


  Vector#(16, Bit#(1)) v6 = unpack(x5[15:0]);                        //counting MSB 0's of input[15:0]
  Integer result6=0;                                   
  Bool done6 = False;
  for( Integer p6 = 15; p6 >=0; p6 = p6 - 1) 
	   begin
	     if ((v6[p6] == 0)&&(!done6))  result6 = result6+1 ;
	     else
	       done6 = True;
	   end
  //Integer i2 = (done3==True)?(16-result3):0;
  Integer z6 = result6;


  Integer i0 = (|x5[105:96]==1)?z0:((|x5[95:80]==1)?(z1+10):((|x5[79:64]==1)?(z2+26):((|x5[63:48]==1)?(z3+42):((|x5[47:32]==1)?(z4+58):((|x5[31:16]==1)?(z5+74):((|x5[15:0]==1)?(z6+90):106))))));  //counting total MSB Zeros

  return i0;
endfunction

function Tuple4#(Bit#(106),Bit#(11),Bit#(5),Bit#(2)) fn_norm_dp(Bit#(108) prod,Bit#(11) exp,Int#(32) shift,Bit#(5) exc, Bit#(2) z,Bit#(32) i2);

  let x5 = prod;                    //generating product from partial products

  let ex= exc;

  Int#(32) i0 = unpack(i2);

  Int#(32) max_shift = unpack(zeroExtend(exp-11'd1));

  Bit#(1) norm_valid = (|exp==1'b0)?1'b0:pack((i0-1)>0); 
  Bit#(108) x01 = (|exp==1'b1)?x5:((exc[1]==1'b0)?(x5>>1):((shift==0)?108'd0:(x5>>(shift+1)))); 
  Bit#(108) x02 = ((i0)>max_shift)?x5<<(max_shift):x5<<(i0-1);
  let exp1 = ((exp == 11'd2046)?11'd2046:exp+11'd1);
  let exp2 = ((i0-1)>max_shift)?(11'd0):((exp-truncate(i2)+11'd1));
  let x_out0 = (norm_valid==1'b1)?x02:x01;      //finding output
  let ex1 = ((z[0]==1'b1)?1'b0:((|exp==1'b1)?1'b0:exc[1]));           
  ex[2] = ((exp==11'd2046)&&(i0==0))?1'b1:exc[2];           //overflow flags
  let x_out1 = (ex[2]==1'b1)?{108'h1ffffffffffffffffffffffffff}:x_out0;

  let exp_out = (ex1==1'b1)?(11'd0):((i0==0)?exp1:((norm_valid==1'b1)?exp2:(exp)));
  let x_out = ((ex1==1'b1)&&(shift==0))?108'd0:((ex[4]==1'b1)?108'h180000000000000000000000000:x_out1);

  //--------------------Flags---------------------------//
  //------------underflow---------------//

  Bit#(1) sfd_prec = (exc[1]==1'b1)&&(shift!=0)?|(x5<<(108-shift)):(((exc[1]==1'b1)&&(shift==0))?1'b1:1'b0);
  let x07 = x5>>(shift+1);
  let x08 = x5>>(shift);
  x07[0] = x07[0]|sfd_prec;
  Bit#(2) guard = 0;
  Bit#(2) guard_s = 0;
  Bit#(2) guard_out = 0;
  guard[1] = x07[51];
  guard[0] = |x07[50:0];
  guard_s[1] = x08[51];
  guard_s[0] = |x08[50:0];

  let ex12 = (|exp_out==1'b0)&&(guard_s!=0)&&(|x_out[51:0]==1'b0)?1'b0:1'b1;
  let ex11 = (((|x_out[51:0]==1'b0)&&((exc[1]==1'b0)))?1'b0:(((|exp_out==1'b0)&&(guard!=0))?(1'b1):(1'b0)));
  let ex2 = (|exp_out==1'b0)&&(guard_s!=0)?(1'b1):ex11;
  ex[1]=((i0-1)>max_shift)&&(|x02[51:0]==1'b0)?1'b0:ex2;

  //-----------------Inexact Flags--------------//
  Bit#(1) ex00 = ~((|exp_out)&(x_out[105]));

  let ex01 = (((|exp==1'b0)&&(|exp_out==1'b1))?((|x5[51:0]==1'b1)?1'b1:1'b0):1'b0);
  ex[0] =ex01;

  //------------------Guards for rounding---------------------------//
  Bit#(1) grd = (shift>0)?|(x5<<(108-shift)):1'b0;
  Bit#(108) sfd_res = x5>>shift;
  sfd_res[0] = sfd_res[0]|grd;
  if(|exp == 1'b0)  x_out[0] = sfd_res[0];
  guard_out[1] = (norm_valid==1'b1)?1'b0:(sfd_res[51]);
  guard_out[0] = (norm_valid==1'b1)?1'b0:((sfd_prec)|(|sfd_res[50:0]));

  //guard_out = guard|guard_s;


  return tuple4(x_out[105:0],exp_out,ex,guard_out);
endfunction



function Bit#(3) fn_Special_EX_dp(Bit#(11) e1,Bit#(11) e2,Bit#(52) m1,Bit#(52) m2);

  Bit#(1) a_inf=pack((&e1==1'b1)&&(|m1==1'b0));
  Bit#(1) b_inf=pack((&e2==1'b1)&&(|m2==1'b0));
  Bit#(1) a_z=pack((|e1==1'b0)&&(|m1==1'b0));
  Bit#(1) b_z=pack((|e2==1'b0)&&(|m2==1'b0));
  Bit#(1) a_snan=pack((&e1==1'b1)&&(m1[51]==1'b0)&&(|m1[50:0]==1'b1));
  Bit#(1) b_snan=pack((&e2==1'b1)&&(m2[51]==1'b0)&&(|m2[50:0]==1'b1));
  Bit#(1) snan_valid = pack((a_z&b_inf)|(a_inf&b_z)|(a_z&b_snan)|(a_snan&b_z)|(b_inf&a_snan)|(a_inf&b_snan));
  Bit#(1) snan = ((a_z|b_z)==1'b0)?(a_snan|b_snan):snan_valid;


  Bit#(1) a_qnan=pack((&e1==1'b1)&&(m1[51]==1'b1));
  Bit#(1) b_qnan=pack((&e2==1'b1)&&(m2[51]==1'b1));
  Bit#(1) qnan_valid = pack((a_z&b_qnan)|(a_qnan&b_z)|(a_inf&b_qnan)|(b_inf&a_qnan));
  Bit#(1) qnan = (snan==1'b1)?1'b0:(((a_z|b_z)==1'b0)?(a_qnan|b_qnan):qnan_valid);


  Bit#(1) inf = (snan==1'b1)||(qnan==1'b1)?1'b0:(a_inf|b_inf);
  Bit#(1) zero = (snan==1'b1)||(qnan==1'b1)?1'b0:(a_z|b_z);
  Bit#(1) nan = (snan==1'b1)?snan:qnan;

  Bit#(3) res = (qnan==1'b1)?3'b111:{nan,inf,zero};
  return res;
endfunction

function Bit#(216) fn_gen_pp_dp(Bit#(54) m,Bit#(6) b);                    //generates the partial products for mantissa multiplication
  Bit#(60) y=0;
  Bit#(60) y1=0;
  Bit#(60) a=zeroExtend(m);
  Bit#(60) res1=0;
  Bit#(60) res2=0;
  Bit#(60) res3=0;
  Bit#(60) res4=0;
  Bit#(60) res6=0;
  Bit#(60) res5=0;
  Bit#(216) o=0;
  res1 = (b[0]==1'b1)?(a):60'd0;
  res2 = (b[1]==1'b1)?(a<<1):60'd0;
  res3 = (b[2]==1'b1)?(a<<2):60'd0;
  res4 = (b[3]==1'b1)?(a<<3):60'd0;
  res5 = (b[4]==1'b1)?(a<<4):60'd0;
  res6 = (b[5]==1'b1)?(a<<5):60'd0;
  y =  res1+res2+res3;
  y1 = res4+res5+res6;
  o= {48'b0,y,48'b0,y1};
  return o;          // extending the result
endfunction







/*******Round_Off Functions*********/



function Tuple2#(FloatingPoint#(11,52),Exception) fn_rnd_Nearest_Even_dp(Reg#(Exception) exc,Bit#(1) sgn,Bit#(11) exp,Bit#(106) x,Bit#(5) excb,Bit#(2) guard1);        //round to nearest even

  Bit#(52) y=0;                                          
  Bit#(11) e= 0;
  Exception ex= defaultValue;
  Bit#(52) sfd_out =0;
  Bit#(11) exp_out=0;
  Bit#(3) guard = (x[105]==1'b1)?{x[52],x[51],|x[50:0]}:{x[51],x[50],|x[49:0]};
  let sfd = (x[105]==1'b1)?x[104:53]:x[103:52];
  let sfd_lsb = (x[105]==1'b1)?|x[52:0]:|x[51:0];
  Bit#(54) sfd_res = {1'b0, |exp, sfd} + 54'd1; 

  Bit#(1) sfdlsb = (x[105]==1'b1)?x[53]:x[52]; 
  if (sfd_res[53] == 1'b1) begin
	   if (exp == 11'd2046) begin
	     exp_out=11'd2047;
	     sfd_out = 52'd0;
	   end
	   else begin
	     exp_out = exp + 11'd1;
	     sfd_out = truncate(sfd_res>> 1);
	   end
  end
  else if ((exp == 0) && (truncateLSB(sfd_res) == 2'b01)) begin
	   exp_out = 11'd1;
	   sfd_out = truncate(sfd_res);
  end
  else begin
    exp_out = exp ;
    sfd_out = truncate(sfd_res);
  end
  Bit#(1) e_xu=0;
  Bit#(1) e_xo=0;
  if (excb[4] == 1'b1) begin
    y = (52'h8000000000000);
    e = 11'd2047;
  end
  else begin
    case (guard)
      3'b000 : begin   y=sfd;e=exp;  end
      3'b001 : begin   y=sfd;e=exp;  end
      3'b010 : begin   y=sfd;e=exp;  end
      3'b011 : begin   y=sfd;e=exp;  end
      3'b100 : begin if (sfdlsb==1'b1) begin   y = sfd_out; e = exp_out; /*e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(|x[50:0]==1'b1)?1'b1:1'b0; */
      e_xo = (exp==11'd2046)&&(&sfd==1'b1)/*&&(|x[50:0]==1'b1)*/?1'b1:1'b0;            end  


      else if ((pack(sfdlsb==1'b0)&pack(guard1!=0)&pack(exp==0))==1'b1) begin 
                  y = sfd_out; e = exp_out; /*e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(|x[21:0]==1'b1)?1'b1:1'b0; */
      e_xo = (exp==11'd2046)&&(&sfd==1'b1)&&(|x[50:0]==1'b1)?1'b1:1'b0; 
                  end
      else begin    y=sfd;e=exp; end end
 
      3'b101 : begin   y = sfd_out; e = exp_out;/* e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(|x[50:0]==1'b1)?1'b1:1'b0; */
      e_xo = (exp==11'd2046)&&(&sfd==1'b1)&&(|x[50:0]==1'b1)?1'b1:1'b0;            end   
      3'b110 : begin   y = sfd_out; e = exp_out; e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(&guard[2:1]==1'b1)?1'b1:1'b0; 
      e_xo = (exp==11'd2046)&&(&sfd==1'b1)&&(|x[50:0]==1'b1)?1'b1:1'b0;            end   
      3'b111 : begin   y = sfd_out; e = exp_out; e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(&guard[2:1]==1'b1)?1'b1:1'b0; 
      e_xo = (exp==11'd2046)&&(&sfd==1'b1)&&(|x[50:0]==1'b1)?1'b1:1'b0;            end   
    endcase
  end

  ex = (excb[4]==1'b1)?fn_setEx(exc,0):((excb[2]==1'b1)?fn_setEx(exc,2):((excb[1]==1'b1)?((e_xu==1'b1)?(fn_setEx(exc,4)):fn_setEx(exc,3)):(((excb[0]==1'b1)||(guard!=3'b000))?((e_xo==1'b1)?(fn_setEx(exc,2)):fn_setEx(exc,4)):(fn_setEx(exc,0)))));

  return tuple2(FloatingPoint{sign: unpack(sgn),exp: e,sfd: y},ex);
endfunction


function Tuple2#(FloatingPoint#(11,52),Exception) fn_rnd_Nearest_Away_Zero_dp(Reg#(Exception) exc,Bit#(1) sgn,Bit#(11) exp,Bit#(106) x,Bit#(5) excb,Bit#(2) guard1);     //round to nearest away zero
  Bit#(52) y=0;                                          
  Bit#(11) e= 0;
  Exception ex= defaultValue;
  Bit#(52) sfd_out =0;
  Bit#(11) exp_out=0;
  Bit#(3) guard = (x[105]==1'b1)?{x[52],x[51],|x[50:0]}:{x[51],x[50],|x[49:0]};
  let sfd = (x[105]==1'b1)?x[104:53]:x[103:52];
  let sfd_lsb = (x[105]==1'b1)?|x[52:0]:|x[51:0];
  Bit#(54) sfd_res = {1'b0, |exp, sfd} + 54'd1; 

  if (sfd_res[53] == 1'b1) begin
	   if (exp == 11'd2046) begin
	     exp_out=11'd2047;
	     sfd_out = 52'd0;
	   end
	   else begin
	     exp_out = exp + 11'd1;
	     sfd_out = truncate(sfd_res>> 1);
	   end
  end
  else if ((exp == 0) && (truncateLSB(sfd_res) == 2'b01)) begin
	   exp_out = 11'd1;
	   sfd_out = truncate(sfd_res);
  end
  else begin
    exp_out = exp ;
    sfd_out = truncate(sfd_res);
  end
  Bit#(1) e_xu=0;
  Bit#(1) e_xo=0;
  if (excb[4] == 1'b1) begin
    y = (52'h8000000000000);
    e = 11'd2047;
  end
  else begin
    case (guard) matches
      3'b0?? : begin   y=sfd;e=exp;  end
      3'b1?? : begin   y = sfd_out; e = exp_out; e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(guard[1]==1'b1)?1'b1:1'b0; 
      e_xo = (exp==11'd2046)&&(&sfd==1'b1)/*&&(|x[50:0]==1'b1)*/?1'b1:1'b0;            end      
    endcase
	     
  end

  ex = (excb[4]==1'b1)?fn_setEx(exc,0):((excb[2]==1'b1)?fn_setEx(exc,2):((excb[1]==1'b1)?((e_xu==1'b1)?(fn_setEx(exc,4)):fn_setEx(exc,3)):(((excb[0]==1'b1)||(guard!=3'b000))?((e_xo==1'b1)?(fn_setEx(exc,2)):fn_setEx(exc,4)):(fn_setEx(exc,0)))));

  return tuple2(FloatingPoint{sign: unpack(sgn),exp: e,sfd: y},ex);
endfunction


function Tuple2#(FloatingPoint#(11,52),Exception) fn_rnd_Plus_Inf_dp(Reg#(Exception) exc,Bit#(1) sgn,Bit#(11) exp,Bit#(106) x,Bit#(5) excb,Bit#(2) guard1);           //round to plus infinity
  Bit#(52) y=0;                                          
  Bit#(11) e= 0;
  Exception ex= defaultValue;
  Bool tinycheck_underflow = False;
  Bit#(52) sfd_out =0;
  Bit#(11) exp_out=0;
  Bit#(3) guard = (x[105]==1'b1)?{x[52],x[51],|x[50:0]}:{x[51],x[50],|x[49:0]};
  Bit#(2) guard2 = (x[105]==1'b1)?{x[52],|x[51:0]}:{x[51],|x[50:0]};
  let sfd = (x[105]==1'b1)?x[104:53]:x[103:52];
  let sfd_lsb = (x[105]==1'b1)?|x[52:0]:|x[51:0];
  Bit#(54) sfd_res = {1'b0, |exp, sfd} + 54'd1; 

  if (sfd_res[53] == 1'b1) begin
	   if (exp == 11'd2046) begin
	     exp_out=11'd2047;
	     sfd_out = 52'd0;
	   end
	   else begin
	     exp_out = exp + 11'd1;
	     sfd_out = truncate(sfd_res>> 1);
	   end
  end
  else if ((exp == 0) && (truncateLSB(sfd_res) == 2'b01)) begin
	   exp_out = 11'd1;
	   sfd_out = truncate(sfd_res);
	   if ((sgn==1'b0) && ((guard2 == 'b10) || (guard2 == 'b01))) begin   // mark tiny
         tinycheck_underflow = True;
         end
  end
  else begin
    exp_out = exp ;
    sfd_out = truncate(sfd_res);
  end
  Bit#(1) e_xu=0;
  Bit#(1) e_xo=0;
  if (excb[4] == 1'b1) begin
    y = truncate(52'h8000000000000);
    e = 11'd2047;
  end
  else begin
    if (guard == 0) begin 
      y = (guard1!=0)&&(sgn==1'b0)?(sfd_out):sfd; 
      e= (y==sfd_out)&&(sfd_res[52]!=0)&&(exp==0)?exp+11'd1:exp;
    end
    else if (sgn==1'b0) begin
      y = sfd_out;
      e = exp_out;
      e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(guard[2]==1'b1)&&(|guard[1:0]==1'b1)?1'b1:1'b0;
      e_xo = (exp==11'd2046)&&(&sfd==1'b1)&&(|x[51:0]==1'b1)?1'b1:1'b0;
    end
    else begin
      y=sfd;
      e=exp;
    end   
  end

  ex = (excb[4]==1'b1)?fn_setEx(exc,0):((excb[2]==1'b1)?fn_setEx(exc,2):((excb[1]==1'b1)?((e_xu==1'b1)?(fn_setEx(exc,4)):fn_setEx(exc,3)):(((excb[0]==1'b1)||(guard!=3'b000))?((e_xo==1'b1)?(fn_setEx(exc,2)):fn_setEx(exc,4)):(fn_setEx(exc,0)))));
  if (tinycheck_underflow) ex.underflow = True;
  return tuple2(FloatingPoint{sign: unpack(sgn),exp: e,sfd: y},ex);
endfunction


function Tuple2#(FloatingPoint#(11,52),Exception) fn_rnd_Minus_Inf_dp(Reg#(Exception) exc,Bit#(1) sgn,Bit#(11) exp,Bit#(106) x,Bit#(5) excb,Bit#(2) guard1); //round to minus infinity
  Bit#(52) y=0;                                          
  Bit#(11) e= 0;
  Exception ex= defaultValue;
  Bool tinycheck_underflow = False;
  Bit#(52) sfd_out =0;
  Bit#(11) exp_out=0;
  Bit#(3) guard = (x[105]==1'b1)?{x[52],x[51],|x[50:0]}:{x[51],x[50],|x[49:0]};
  Bit#(2) guard2 = (x[105]==1'b1)?{x[52],|x[51:0]}:{x[51],|x[50:0]};
  let sfd = (x[105]==1'b1)?x[104:53]:x[103:52];
  let sfd_lsb = (x[105]==1'b1)?|x[52:0]:|x[51:0];
  Bit#(54) sfd_res = {1'b0, |exp, sfd} + 54'd1; 

  if (sfd_res[53] == 1'b1) begin
	   if (exp == 11'd2046) begin
	     exp_out=11'd2047;
	     sfd_out = 52'd0;
	   end
	   else begin
	     exp_out = exp + 11'd1;
	     sfd_out = truncate(sfd_res>> 1);
	   end
  end
  else if ((exp == 0) && (truncateLSB(sfd_res) == 2'b01)) begin
	   exp_out = 11'd1;
	   sfd_out = truncate(sfd_res);
	   if ((sgn==1'b1) && ((guard2 == 'b10) || (guard2 == 'b01))) begin   // mark tiny
         tinycheck_underflow = True;
         end
  end
  else begin
    exp_out = exp ;
    sfd_out = truncate(sfd_res);
  end
  Bit#(1) e_xu=0;
  Bit#(1) e_xo=0;
  if (excb[4] == 1'b1) begin
    y =(52'h8000000000000);
    e = 11'd2047;
  end
  else begin

    if (guard == 0) begin 
      y = (guard1!=0)&&(sgn==1'b1)?(sfd_out):sfd; 
      e= (y==sfd_out)&&(sfd_res[52]!=0)&&(exp==0)?exp+11'd1:exp;
    end
    else if (sgn==1'b1) begin
      y = sfd_out;
      e = exp_out;
      e_xu = (excb[1]==1'b1)&&(&sfd==1'b1)&&(guard[2]==1'b1)&&(|guard[1:0]==1'b1)?1'b1:1'b0;
      e_xo = (exp==11'd2046)&&(&sfd==1'b1)&&(|x[51:0]==1'b1)?1'b1:1'b0;
    end
    else begin
      y=sfd;
      e=exp;
    end
  end

  ex = (excb[4]==1'b1)?fn_setEx(exc,0):((excb[2]==1'b1)?fn_setEx(exc,2):((excb[1]==1'b1)?((e_xu==1'b1)?(fn_setEx(exc,4)):fn_setEx(exc,3)):(((excb[0]==1'b1)||(guard!=3'b000))?((e_xo==1'b1)?(fn_setEx(exc,2)):fn_setEx(exc,4)):(fn_setEx(exc,0)))));
  if (tinycheck_underflow) ex.underflow = True;
  return tuple2(FloatingPoint{sign: unpack(sgn),exp: e,sfd: y},ex);
endfunction


function Tuple2#(FloatingPoint#(11,52),Exception) fn_rnd_Zero_dp(Reg#(Exception) exc,Bit#(1) sgn,Bit#(11) exp,Bit#(106) x,Bit#(5) excb,Bit#(2) guard);      //round to zero
  Bit#(52) y=0;                                          
  Bit#(11) e= 0;
  Exception ex= defaultValue;
  let y1 = (x[105]==1'b1)?pack(|x[52:0]==1'b0):pack(|x[51:0]==1'b0);
  let exp1 = (exp == 11'd2046)?11'd2046:exp+11'd1;


  if (excb[4] == 1'b1) begin
    y = (52'h8000000000000);
    e = 11'd2047;
  end

  else begin
    y= (x[105]==1'b1)?x[104:53]:x[103:52];                                          
    e= exp; 
  end
  ex = (excb[4]==1'b1)?fn_setEx(exc,0):((excb[2]==1'b1)?fn_setEx(exc,2):((excb[1]==1'b1)?fn_setEx(exc,3):(((excb[0]==1'b1)||(y1==1'b0))?(fn_setEx(exc,4)):(fn_setEx(exc,0)))));
  return tuple2(FloatingPoint{sign: unpack(sgn),exp: e,sfd: y},ex);
endfunction


endpackage

