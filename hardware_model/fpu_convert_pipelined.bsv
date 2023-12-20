////////////////////////////////////////////////////////////////////////////////
//see LICENSE.iitm
////////////////////////////////////////////////////////////////////////////////
/*
---------------------------------------------------------------------------------------------------

Author: Joyanta Mondal, Nitya Ranganathan, Nirmaladevi, Nagakaushik Moturi
Email id: ee19m058@smail.iitm.ac.in, nitya.ranganathan@gmail.com, nirmala.sashi@gmail.com, ee17b111@smail.iitm.ac.in
Details: Modules for conversion from sp to dp, dp to sp, integer to sp/dp, sp/dp to integer
         
--------------------------------------------------------------------------------------------------
*/

package fpu_convert_pipelined;

import Vector        ::*;
import Vector        ::*;
import DefaultValue  ::*;
import DReg          ::*;
import fpu_common    ::*;


//////////////// single-precision (float) to double-precision (double) conversion ////////////////
function Tuple2#(FloatingPoint#(11,52),Exception) fn_convert_sp_to_dp(Tuple2#(FloatingPoint#(8,23),RoundMode) operand);

  FloatingPoint#(11,52) out = defaultValue;                    //double precision result
  Exception ex = defaultValue;                                 //double precision exception
  Bit#(5) flag = 0;

  match {.in,.roundmode} = operand;

  Bit#(23) sgfd = in.sfd;
  Bit#(8) expn = in.exp;
  flag[0] = pack((|expn==1'b0)&&(|sgfd==1'b0));           //zero flag
  flag[1] = pack((&expn==1'b1)&&(|sgfd==1'b0));            //infinity flag
  flag[2] = pack((&expn==1'b1)&&(sgfd[22]==1'b1));         //qnan flag
  flag[3] = pack((&expn==1'b1)&&(sgfd[22]==1'b0)&&(|sgfd[21:0]==1'b1));   //snan flag

  Bit#(11) out_exp = zeroExtend(expn)+ 'd896;           //evaluating exponent
  Bit#(52) out_sfd = {sgfd,29'd0};                      //zero-extending lsb for mantissa
  Bit#(52) inp = out_sfd;

  Vector#(13, Bit#(1)) v1 = unpack(inp[51:39]);                        //counting MSB 0's of input
  Integer result1=0;                                  
  Bool done1 = False;
  for( Integer p1 = 12; p1 >=0; p1 = p1 - 1) 
  begin
    if ((v1[p1] == 0)&&(!done1))
      result1 = result1+1 ;
    else
      done1 = True;
  end
  Integer z0 = result1;
   
  Vector#(13, Bit#(1)) v2 = unpack(inp[38:26]);                        //counting MSB 0's of input
  Integer result2=0;                                   
  Bool done2 = False;
  for( Integer p2 = 12; p2 >=0; p2 = p2 - 1) 
  begin
    if ((v2[p2] == 0)&&(!done2))
      result2 = result2+1 ;
    else
      done2 = True;
    end
  Integer z1 = result2;
   
  Vector#(13, Bit#(1)) v3 = unpack(inp[25:13]);                        //counting MSB 0's of input
  Integer result3=0;                                   
  Bool done3 = False;
  for( Integer p3 = 12; p3 >=0; p3 = p3 - 1) 
  begin
    if ((v3[p3] == 0)&&(!done3))
      result3 = result3+1 ;
    else
      done3 = True;
  end
  Integer z2 = result3;
   
  Vector#(13, Bit#(1)) v4 = unpack(inp[12:0]);                        //counting MSB 0's of input
  Integer result4=0;                                   
  Bool done4 = False;
  for( Integer p4 = 12; p4 >=0; p4 = p4 - 1) 
  begin
    if ((v4[p4] == 0)&&(!done4))
      result4 = result4+1 ;
    else
      done4 = True;
  end
  Integer z3 = result4;
   
  Integer i0 = (|inp[51:39]==1)?z0:((|inp[38:26]==1)?(z1+13):((|inp[25:13]==1)?(z2+26):((|inp[12:0]==1)?(z3+39):52)));

  // evaluating output sign
  out.sign = (flag[2]==1'b1)||(flag[3]==1'b1)?unpack(1'b0):in.sign;
  // evaluating output exponent
  out.exp = (((flag[2]==1'b1)||(flag[3]==1'b1)||(flag[1]==1'b1))?11'd2047:((flag[0]==1'b1)?11'd0:(((i0>0)&&(|expn==0))?(out_exp-fromInteger(i0)):out_exp)));
  // evaluating output normalised mantissa
  out.sfd = (flag[3]==1'b1)?{2'b10,50'd0}:((flag[2]==1'b1)?{1'b1,51'd0}:((flag[1]==1'b1)?52'd0:((flag[0]==1'b1)?52'd0:(((i0>0)&&(|expn==0))?out_sfd<<fromInteger(i0+1):((|expn==0)?out_sfd<<1:out_sfd)))));

  // generating output exceptions
  ex = (flag[3]==1'b1)?unpack(5'b10000):defaultValue;

  return tuple2(out,ex);
endfunction

interface Ifc_fpu_convert_sp_to_dp;
  method ReturnType#(11,52) start(Tuple2#(FloatingPoint#(8,23),RoundMode) operand);
endinterface

(*synthesize*)
module mk_fpu_convert_sp_to_dp(Ifc_fpu_convert_sp_to_dp);

  method ReturnType#(11,52) start(Tuple2#(FloatingPoint#(8,23),RoundMode) operand);
    let x = fn_convert_sp_to_dp(operand);
    return ReturnType{valid:1'b1 ,value:tpl_1(x) ,ex:tpl_2(x)};
  endmethod

endmodule // mk_fpu_convert_sp_to_dp


//////////////// double-precision (double) to single-precision (float) conversion ////////////////
function Tuple2#(FloatingPoint#(8,23),Exception) fn_convert_dp_to_sp(Tuple2#(FloatingPoint#(11,52),RoundMode) operand);

  FloatingPoint#(8,23) out = defaultValue;       //single precision result
  Exception ex = defaultValue;                   //single precision exception
  Bit#(5) ex_flag = 0;
  Bit#(4) flags = 0;
  Bit#(1) denormal = 0;

  match {.in,.roundmode} = operand;

  Bit#(52) sgfd = in.sfd;
  Bit#(11) expn = in.exp;

  flags[0] = pack((|expn==1'b0)&&(|sgfd==1'b0));           //zero flag
  flags[1] = pack((&expn==1'b1)&&(|sgfd==1'b0));            //infinity flag
  flags[2] = pack((&expn==1'b1)&&(sgfd[51]==1'b1));         //qnan flag
  flags[3] = pack((&expn==1'b1)&&(sgfd[51]==1'b0)&&(|sgfd[50:0]==1'b1));   //snan flag


  if (flags[0]==1) begin     //input is zero
    out = FloatingPoint{sign:in.sign , exp : 8'd0 , sfd : 23'd0};    //setting output as zero
  end

  else if (flags[2] == 1 || flags[3] == 1) begin  //input is SNAN or QNAN
    ex_flag[4] = flags[3];                                                     //setting NAN flag
    out = FloatingPoint{sign:unpack(1'b0) , exp : 8'hFF, sfd : {1'b1,22'd0}};  //setting output as QNAN
  end

  else if (flags[1] == 1) begin  //input is infinity
    out = FloatingPoint{sign:in.sign , exp : 8'hFF, sfd : 23'd0};          //setting output as infinity
  end

  else if (expn > 'd1150) begin        //overflow cases
   ex_flag[2] = 1;
   ex_flag[0] = 1;
 
   if(roundmode == Rnd_Zero) //Round to zero 
     out = FloatingPoint{sign:in.sign , exp : 8'hFE, sfd : {3'b111,20'hFFFFF}};  //Highest positive number 7f7fffff

   else if (roundmode == Rnd_Minus_Inf)  //Round down
     if(in.sign == False)
       out = FloatingPoint{sign:in.sign , exp : 8'hFE, sfd : {3'b111,20'hFFFFF}};
     else
       out = FloatingPoint{sign:in.sign , exp : 8'hFF, sfd : 23'd0};  

   else if (roundmode == Rnd_Plus_Inf && in.sign == True)  //round up
     out = FloatingPoint{sign:unpack(1'b1) , exp : 8'hFE, sfd : {3'b111,20'hFFFFF}};

   else
     out = FloatingPoint{sign:in.sign , exp : 8'hFF, sfd : 23'd0};  
  end

  else begin       //without overflow cases
    Bit#(32) res = 0;
    Bit#(49) man = 0;
    Bit#(8)  expo = 0;
    bit underflow = 0;
    bit lv_guard = 0;
    bit lv_denormal_roundup = 0;      
    let lv_sticky = |sgfd[26:0];

    if (expn <= 'd872) begin    //Underflow cases
      if(roundmode == Rnd_Minus_Inf && in.sign == True) //Round Down
        res = {1'b1,30'b0,1'b1};
      else if(roundmode == Rnd_Plus_Inf && in.sign == False)
        res = 1;
      else
        res = {pack(in.sign),'0};
      underflow = 1;
      ex_flag[1] = 1;
      ex_flag[0] = 1;
    end

    else if ((expn <= 'd896)&&(expn > 'd872)) begin  //Denormal number //Set sticky bit!!
      let shiftDist = 'd896 - expn;
      man = {1'b1,sgfd[51:27],23'd0} >> shiftDist;
      if(man[23:0] != 0)
        lv_sticky = 1;
      expo = '0;
      denormal = 1;      
    end

    else begin      //Normal number cases
      expo = truncate(expn - 'd896);
      man = {sgfd[51:27],24'd0};
    end

    lv_guard = man[25];
    let lv_round = man[24];
    let lv_inexact = 0;
    let lv_round_up = 0;
    if(((lv_guard | lv_round | lv_sticky)) == 1) begin
      lv_inexact = 1;
      ex_flag[0]=1'b1;
    end
    if(denormal == 1 && lv_inexact == 1) begin
      ex_flag[1]=1;                                                        //setting underflow flag        
      ex_flag[0] = 1;                                                      //setting inexact flag
    end

    //setting round_up bit(rounding_off)
    if(flags[2]==0 && flags[1] == 0 &&  flags[3] == 0) begin
      if(roundmode == Rnd_Nearest_Even)   //roundmode nearest_even
        lv_round_up = lv_guard & (lv_round|lv_sticky|man[26]);
      else if(roundmode == Rnd_Nearest_Away_Zero)      //roundmode nearest_away_zero
        lv_round_up = lv_guard; 
      else if(roundmode == Rnd_Plus_Inf)               //roundmode plus_infinity(round_up)
        lv_round_up = (lv_guard|lv_round|lv_sticky) & ~pack(in.sign);
      else if(roundmode == Rnd_Minus_Inf)begin         //roundmode minus_infinity(round_down)
        lv_round_up = (lv_guard|lv_round|lv_sticky) & pack(in.sign);
      end

      Bit#(24) fman = zeroExtend(man[48:26]);   //rounded result
      //modifying exponent and mantissa        
      if(lv_round_up == 1)                     
        fman = fman + 1;                      //normalised result
      if(fman[23] == 1) begin
        expo = expo + 1;                                                    
        ex_flag[1] = (&sgfd[51:29]==1)&&(ex_flag[1]==1)?((denormal==1)&&(lv_round==0)&&((roundmode==Rnd_Nearest_Away_Zero)||(roundmode==Rnd_Nearest_Even))?1:((lv_sticky==0)?((lv_round==1)?0:1):0)):(ex_flag[1]);   //setting final underflow flag
      end
      if(underflow==0)                                    
        res = {pack(in.sign),expo,fman[22:0]};         //numbers without overflow or underflow
      end
      out = FloatingPoint{sign:unpack(res[31]),exp : res[30:23], sfd: res[22:0]};  //final result
      ex_flag[2]=ex_flag[2]|pack((&res[30:23]==1'b1)&&(|res[22:0]==1'b0));  //setting final overflow flag
  end
  
  ex = unpack(ex_flag);                            //final exception

  return tuple2(out,ex);
endfunction

interface Ifc_fpu_convert_dp_to_sp;                                                 //interface for double-float conversion
  method Action start(Tuple2#(FloatingPoint#(11,52),RoundMode) operand);
  method ReturnType#(8,23) receive();
endinterface

(*synthesize*)
module mk_fpu_convert_dp_to_sp(Ifc_fpu_convert_dp_to_sp);                           //module for double-float conversion

  
  Reg#(Bit#(8)) rg_exp <-mkReg(0);
  Reg#(Bit#(23)) rg_sfd <-mkReg(0); 
  Reg#(FloatingPoint#(8,23)) rg_out <- mkReg(defaultValue);
  Vector#(2,Reg#(Bit#(1))) rg_valid <- replicateM(mkDReg(0));
  
  Reg#(Bit#(1)) rg_without_overflow <-mkDReg(0);
  
  Reg#(bit) rg_lv_guard <-mkReg(0);
  Reg#(Bit#(49)) rg_man <-mkReg(0);
  Reg#(Bit#(32)) rg_res <-mkReg(0);
  Reg#(Bit#(5)) rg_ex_flag <-mkReg(0);
  Reg#(bit) rg_lv_sticky <-mkReg(0);
  Reg#(Bit#(1)) rg_denormal <- mkReg(0);
  Reg#(Bit#(4)) rg_flags <- mkReg(0);
  Reg#(RoundMode) rg_roundmode <- mkReg(defaultValue);
  Reg#(Bit#(1)) rg_sign <- mkReg(0);
  Reg#(Bit#(8)) rg_expo <- mkReg(0);
  Reg#(Bit#(52)) rg_sgfd <- mkReg(0);
  Reg#(bit) rg_underflow <-mkReg(0);
  Wire#(Tuple2#(FloatingPoint#(11,52),RoundMode)) wr_in_operand <- mkWire();
  Wire#(ReturnType#(8,23)) wr_out <- mkWire();
  
  rule rl_stage1;
    FloatingPoint#(8,23) out = defaultValue;       //single precision result
    Exception ex = defaultValue;                   //single precision exception
    Bit#(5) ex_flag = 0;
    Bit#(4) flags = 0;
    Bit#(1) denormal = 0;

    match {.in,.roundmode} = wr_in_operand;

    Bit#(52) sgfd = in.sfd;
    Bit#(11) expn = in.exp;

    flags[0] = pack((|expn==1'b0)&&(|sgfd==1'b0));           //zero flag
    flags[1] = pack((&expn==1'b1)&&(|sgfd==1'b0));            //infinity flag
    flags[2] = pack((&expn==1'b1)&&(sgfd[51]==1'b1));         //qnan flag
    flags[3] = pack((&expn==1'b1)&&(sgfd[51]==1'b0)&&(|sgfd[50:0]==1'b1));   //snan flag


    if (flags[0]==1) begin     //input is zero
      out = FloatingPoint{sign:in.sign , exp : 8'd0 , sfd : 23'd0};    //setting output as zero
    end

    else if (flags[2] == 1 || flags[3] == 1) begin  //input is SNAN or QNAN
      ex_flag[4] = flags[3];                                                     //setting NAN flag
      out = FloatingPoint{sign:unpack(1'b0) , exp : 8'hFF, sfd : {1'b1,22'd0}};  //setting output as QNAN
    end

    else if (flags[1] == 1) begin  //input is infinity
      out = FloatingPoint{sign:in.sign , exp : 8'hFF, sfd : 23'd0};          //setting output as infinity
    end

    else if (expn > 'd1150) begin        //overflow cases
     ex_flag[2] = 1;
     ex_flag[0] = 1;
   
     if(roundmode == Rnd_Zero) //Round to zero 
       out = FloatingPoint{sign:in.sign , exp : 8'hFE, sfd : {3'b111,20'hFFFFF}};  //Highest positive number 7f7fffff

     else if (roundmode == Rnd_Minus_Inf)  //Round down
       if(in.sign == False)
         out = FloatingPoint{sign:in.sign , exp : 8'hFE, sfd : {3'b111,20'hFFFFF}};
       else
         out = FloatingPoint{sign:in.sign , exp : 8'hFF, sfd : 23'd0};  

     else if (roundmode == Rnd_Plus_Inf && in.sign == True)  //round up
       out = FloatingPoint{sign:unpack(1'b1) , exp : 8'hFE, sfd : {3'b111,20'hFFFFF}};

     else
       out = FloatingPoint{sign:in.sign , exp : 8'hFF, sfd : 23'd0};  
    end

    else begin       //without overflow cases
      rg_without_overflow <= 1'b1;             //this branch of code is continued in next rule, this is the variable which will trigger it in the next rule
      Bit#(32) res = 0;
      Bit#(49) man = 0;
      Bit#(8)  expo = 0;
      bit underflow = 0;
      bit lv_guard = 0;
      bit lv_denormal_roundup = 0;      
      let lv_sticky = |sgfd[26:0];

      if (expn <= 'd872) begin    //Underflow cases
        if(roundmode == Rnd_Minus_Inf && in.sign == True) //Round Down
          res = {1'b1,30'b0,1'b1};
        else if(roundmode == Rnd_Plus_Inf && in.sign == False)
          res = 1;
        else
          res = {pack(in.sign),'0};
        underflow = 1;
        ex_flag[1] = 1;
        ex_flag[0] = 1;
      end

      else if ((expn <= 'd896)&&(expn > 'd872)) begin  //Denormal number //Set sticky bit!!
        let shiftDist = 'd896 - expn;
        man = {1'b1,sgfd[51:27],23'd0} >> shiftDist;
        if(man[23:0] != 0)
          lv_sticky = 1;
        expo = '0;
        denormal = 1;      
      end

      else begin      //Normal number cases
        expo = truncate(expn - 'd896);
        man = {sgfd[51:27],24'd0};
      end
      rg_lv_guard <= lv_guard;
      rg_man <= man;
      rg_res <= res;
      rg_lv_sticky <= lv_sticky;
      rg_expo <= expo;
      rg_underflow <= underflow;
    end
    
    rg_ex_flag <= ex_flag;
    rg_denormal <= denormal;
    rg_flags <= flags;
    rg_roundmode <= roundmode ;
    rg_sign <= pack(in.sign);
    rg_sgfd <= sgfd;
    rg_out <= out;
    
    
    rg_valid[0]<=1'b1;
  endrule
  
  rule rl_stage2;
    FloatingPoint#(8,23) out = defaultValue;
    Exception ex = defaultValue;                  
    
    Bit#(5) ex_flag = rg_ex_flag;
    
    if (rg_without_overflow == 1'b1) begin
      
      bit lv_sticky = rg_lv_sticky;
      Bit#(49) man = rg_man;
      Bit#(32) res = rg_res;
      Bit#(1) denormal = rg_denormal;
      Bit#(4) flags = rg_flags;
      RoundMode roundmode = rg_roundmode;
      Bit#(1) sign = rg_sign;
      Bit#(8)  expo = rg_expo;
      Bit#(52) sgfd = rg_sgfd;
      bit underflow = rg_underflow;
      
      let lv_guard = man[25];
      let lv_round = man[24];
      let lv_inexact = 0;
      let lv_round_up = 0;
      if(((lv_guard | lv_round | lv_sticky)) == 1) begin
        lv_inexact = 1;
        ex_flag[0]=1'b1;
      end
      if(denormal == 1 && lv_inexact == 1) begin
        ex_flag[1]=1;                                                        //setting underflow flag        
        ex_flag[0] = 1;                                                      //setting inexact flag
      end

      //setting round_up bit(rounding_off)
      if(flags[2]==0 && flags[1] == 0 &&  flags[3] == 0) begin
        if(roundmode == Rnd_Nearest_Even)   //roundmode nearest_even
          lv_round_up = lv_guard & (lv_round|lv_sticky|man[26]);
        else if(roundmode == Rnd_Nearest_Away_Zero)      //roundmode nearest_away_zero
          lv_round_up = lv_guard; 
        else if(roundmode == Rnd_Plus_Inf)               //roundmode plus_infinity(round_up)
          lv_round_up = (lv_guard|lv_round|lv_sticky) & ~pack(sign);
        else if(roundmode == Rnd_Minus_Inf)begin         //roundmode minus_infinity(round_down)
          lv_round_up = (lv_guard|lv_round|lv_sticky) & pack(sign);
        end

        Bit#(24) fman = zeroExtend(man[48:26]);   //rounded result
        //modifying exponent and mantissa        
        if(lv_round_up == 1)                     
          fman = fman + 1;                      //normalised result
        if(fman[23] == 1) begin
          expo = expo + 1;                                                    
          ex_flag[1] = (&sgfd[51:29]==1)&&(ex_flag[1]==1)?((denormal==1)&&(lv_round==0)&&((roundmode==Rnd_Nearest_Away_Zero)||(roundmode==Rnd_Nearest_Even))?1:((lv_sticky==0)?((lv_round==1)?0:1):0)):(ex_flag[1]);   //setting final underflow flag
        end
        if(underflow==0)                                    
          res = {pack(sign),expo,fman[22:0]};         //numbers without overflow or underflow
        end
        out = FloatingPoint{sign:unpack(res[31]),exp : res[30:23], sfd: res[22:0]};  //final result
        ex_flag[2]=ex_flag[2]|pack((&res[30:23]==1'b1)&&(|res[22:0]==1'b0));  //setting final overflow flag
      end
    else out = rg_out;
    ex = unpack(ex_flag);                            //final exception
    wr_out <= ReturnType{valid:pack(rg_valid[0]) ,value:out ,ex: ex};
    
  endrule
  
  method Action start(Tuple2#(FloatingPoint#(11,52),RoundMode) operand);
    wr_in_operand <= operand;
  endmethod
  
  method ReturnType#(8,23) receive();
    let x = wr_out;
    return x;
  endmethod

endmodule // mk_fpu_convert_dp_to_sp


//////////////// integer (word/long, signed/unsigned) to single-precision (float) conversion ////////////////
interface Ifc_fpu_int_to_sp;
  method Action start(Bit#(64) inp_int, Bit#(1) unsigned_bit, Bit#(1) long, Bit#(3) rounding_mode);
  method ReturnType#(8,23) receive();
endinterface

(*synthesize*)
module mk_fpu_int_to_sp(Ifc_fpu_int_to_sp);

  Reg#(Bit#(1)) rg_stage0_valid <- mkDReg(0);
  Reg#(Bit#(64)) rg_stage0_inp_int <- mkReg(0);
  Reg#(Bit#(1)) rg_stage0_long <- mkReg(0);
  Reg#(Bit#(1)) rg_stage0_sign <- mkReg(0);
  Reg#(Bit#(64)) rg_stage0_inp <- mkReg(0);
  Reg#(Bit#(3)) rg_stage0_rmode <- mkReg(0);
  Vector#(16, Reg#(Bit#(3))) rg_stage0_zcount <- replicateM(mkReg(0));

  Reg#(Bit#(1)) rg_stage1_valid <- mkDReg(0);
  Reg#(Bit#(64)) rg_stage1_inp_int <- mkReg(0);
  Reg#(Bit#(1)) rg_stage1_long <- mkReg(0);
  Reg#(Bit#(1)) rg_stage1_sign <- mkReg(0);
  Reg#(Bit#(64)) rg_stage1_inp <- mkReg(0);
  Reg#(Bit#(3)) rg_stage1_rmode <- mkReg(0);
  Reg#(Bit#(8)) rg_stage1_local_expo <- mkReg(0);

  Wire#(Bit#(1)) wr_valid <- mkDWire(0);
  Wire#(Bit#(64)) wr_inp_int <- mkDWire(0);
  Wire#(Bit#(1)) wr_unsigned_bit <- mkDWire(0);
  Wire#(Bit#(1)) wr_long <- mkDWire(0);
  Wire#(Bit#(3)) wr_rounding_mode <- mkDWire(0);
  Wire#(ReturnType#(8,23)) wr_stage2_result <- mkDWire(unpack(0));


  // stage 0: consider signed/unsigned, word/long, convert to sign & magnitude representation and count zeros for 16 segments
  rule rl_stage0;
    let inp_final = (wr_unsigned_bit==1) ? wr_inp_int 
                      : ((wr_inp_int[63]==0) ? wr_inp_int : (~wr_inp_int+1));
    Bit#(32) inp32_initial = truncate(wr_inp_int);
    Bit#(32) inp32_final = (wr_unsigned_bit==1) ? inp32_initial : ((wr_inp_int[31]==0) ? inp32_initial : (~inp32_initial+1));
    Bit#(64) inp = (wr_long==1) ? inp_final : {inp32_final, 32'd0};
    Bit#(1) sign = (wr_unsigned_bit==1) ? 1'b0
                      : ((wr_long==1) ? ((wr_inp_int[63]==0) ? 1'b0 : 1'b1)
                                      : ((wr_inp_int[31]==0) ? 1'b0 : 1'b1));

    Vector#(4, Bit#(1)) v = unpack('0);
    Bit#(3) count = '0;
    Bool done = False;

    // 64 bits : 16 4-bit partitions
    for (Integer i = 15; i >= 0; i = i - 1) begin
      v = unpack(inp[i*4+3 : i*4]);
      count = '0;
      done = False;
      for (Integer j = 3; j >= 0; j = j - 1) begin
        if ((v[j] == 0) && !done) begin
          count = count + 1;
        end
        else begin
          done = True;
        end
      end

      rg_stage0_zcount[i] <= count;
    end

    rg_stage0_valid <= wr_valid;
    rg_stage0_inp_int <= wr_inp_int;
    rg_stage0_long <= wr_long;
    rg_stage0_sign <= sign;
    rg_stage0_inp <= inp;
    rg_stage0_rmode <= wr_rounding_mode;
  endrule

  // stage 1: final leading zero computation for word and long, determine mantissa and exponent
  rule rl_stage1;
    Bit#(7) count64 = '0;
    Bit#(6) count32 = '0;
    Bool done = False;
    Bit#(7) zeros = '0;
    Bit#(7) index = '0;
    Bit#(8) local_expo = '0;

    let long = rg_stage0_long;
    let inp = rg_stage0_inp;

    done = False;
    for (Integer i = 15; i >= 0; i = i - 1) begin
      if (!done) begin
        if (rg_stage0_zcount[i] == 3'b100) begin // "0000"
          count64 = count64 + 4;
        end
        else begin
          count64 = count64 + zeroExtend(rg_stage0_zcount[i]);
          done = True;
        end
      end
    end

    done = False;
    for (Integer i = 15; i >= 8; i = i - 1) begin
      if (!done) begin
        if (rg_stage0_zcount[i] == 3'b100) begin // "0000"
          count32 = count32 + 4;
        end
        else begin
          count32 = count32 + zeroExtend(rg_stage0_zcount[i]);
          done = True;
        end
      end
    end

    // final zero count; exponent and mantissa computation
    zeros = (long==1) ? count64 : zeroExtend(count32);
    index = (long==1) ? (7'd63-zeros) : (7'd31-zeros);
    local_expo = (8'h7f + zeroExtend(index));
    inp = inp << zeros;

    rg_stage1_valid <= rg_stage0_valid;
    rg_stage1_inp_int <= rg_stage0_inp_int;
    rg_stage1_long <= long;
    rg_stage1_sign <= rg_stage0_sign;
    rg_stage1_inp <= inp;
    rg_stage1_rmode <= rg_stage0_rmode;
    rg_stage1_local_expo <= local_expo;
  endrule

  // stage 2: final leading zero computation for word and long, determine mantissa and exponent
  rule rl_stage2;
    FloatingPoint#(8,23) out = defaultValue;                   
    Exception ex = defaultValue;  
    Bit#(1) guard, round, sticky, lv_roundup;
    Bit#(5) flags;
    Bit#(23) mant_prim;
    Bit#(25) lv_man;

    let inp_int = rg_stage1_inp_int;
    let long = rg_stage1_long;
    let sign = rg_stage1_sign;
    let inp = rg_stage1_inp;
    let rounding_mode = rg_stage1_rmode;
    let local_expo = rg_stage1_local_expo;

    guard = inp[39];
    round = inp[38];
    sticky = (|inp[37:0]);
    flags = {4'd0,(guard|round|sticky)};
    mant_prim = inp[62:40];
    lv_roundup = 0;
    lv_man = {2'b0, mant_prim};

    case(rounding_mode) 
      3'b000:  lv_roundup = guard & (mant_prim[0] | round | sticky);        // rnear_even
      3'b100:  lv_roundup = guard; //& (round | sticky | ~sign);            // rnear_away_zero
      3'b011:  lv_roundup = (guard | round | sticky) & (~sign);             // rmax
      3'b010:  lv_roundup = (guard | round | sticky) & (sign);              // rmin
      default: lv_roundup = 0;
    endcase

    if (lv_roundup == 1) begin
      lv_man = lv_man + 1;
    end
    let final_expo = (lv_man[23] == 1) ? (local_expo + 1) : local_expo;
    out = ((inp_int==0) || ((long==0) && (inp_int[31:0]==0))) ? defaultValue : FloatingPoint{sign:unpack(sign) , exp : final_expo , sfd : lv_man[22:0]};
    ex = ((inp_int==0) || ((long==0) && (inp_int[31:0]==0))) ? unpack(5'd0) : unpack(flags);

    wr_stage2_result <= ReturnType{valid:pack(rg_stage1_valid) ,value:out, ex: ex};
  endrule

  method Action start(Bit#(64) inp_int, Bit#(1) unsigned_bit, Bit#(1) long, Bit#(3) rounding_mode);
    wr_valid <= 1;
    wr_inp_int <= inp_int;
    wr_unsigned_bit <= unsigned_bit;
    wr_long <= long;
    wr_rounding_mode <= rounding_mode;
  endmethod

  method ReturnType#(8,23) receive();
   let stage2_result = wr_stage2_result;
   return stage2_result;
 endmethod

endmodule // mk_fpu_int_to_sp


//////////////// integer (word/long, signed/unsigned) to double-precision (double) conversion ////////////////
interface Ifc_fpu_int_to_dp;
  method Action start(Bit#(64) inp_int, Bit#(1) unsigned_bit, Bit#(1) long, Bit#(3) rounding_mode);
  method ReturnType#(11,52) receive();
endinterface

(*synthesize*)
module mk_fpu_int_to_dp(Ifc_fpu_int_to_dp);

  Reg#(Bit#(1)) rg_stage0_valid <- mkDReg(0);
  Reg#(Bit#(64)) rg_stage0_inp_int <- mkReg(0);
  Reg#(Bit#(1)) rg_stage0_long <- mkReg(0);
  Reg#(Bit#(1)) rg_stage0_sign <- mkReg(0);
  Reg#(Bit#(64)) rg_stage0_inp <- mkReg(0);
  Reg#(Bit#(3)) rg_stage0_rmode <- mkReg(0);
  Vector#(16, Reg#(Bit#(3))) rg_stage0_zcount <- replicateM(mkReg(0));

  Reg#(Bit#(1)) rg_stage1_valid <- mkDReg(0);
  Reg#(Bit#(64)) rg_stage1_inp_int <- mkReg(0);
  Reg#(Bit#(1)) rg_stage1_long <- mkReg(0);
  Reg#(Bit#(1)) rg_stage1_sign <- mkReg(0);
  Reg#(Bit#(64)) rg_stage1_inp <- mkReg(0);
  Reg#(Bit#(3)) rg_stage1_rmode <- mkReg(0);
  Reg#(Bit#(11)) rg_stage1_local_expo <- mkReg(0);

  Wire#(Bit#(1)) wr_valid <- mkDWire(0);
  Wire#(Bit#(64)) wr_inp_int <- mkDWire(0);
  Wire#(Bit#(1)) wr_unsigned_bit <- mkDWire(0);
  Wire#(Bit#(1)) wr_long <- mkDWire(0);
  Wire#(Bit#(3)) wr_rounding_mode <- mkDWire(0);
  Wire#(ReturnType#(11,52)) wr_stage2_result <- mkDWire(unpack(0));


  // stage 0: consider signed/unsigned, word/long, convert to sign & magnitude representation and count zeros for 16 segments
  rule rl_stage0;
    let inp_final = (wr_unsigned_bit==1) ? wr_inp_int 
                      : ((wr_inp_int[63]==0) ? wr_inp_int : (~wr_inp_int+1));
    Bit#(32) inp32_initial = truncate(wr_inp_int);
    Bit#(32) inp32_final = (wr_unsigned_bit==1) ? inp32_initial : ((wr_inp_int[31]==0) ? inp32_initial : (~inp32_initial+1));
    Bit#(64) inp = (wr_long==1) ? inp_final : {inp32_final, 32'd0};
    Bit#(1) sign = (wr_unsigned_bit==1) ? 1'b0
                      : ((wr_long==1) ? ((wr_inp_int[63]==0) ? 1'b0 : 1'b1)
                                      : ((wr_inp_int[31]==0) ? 1'b0 : 1'b1));

    Vector#(4, Bit#(1)) v = unpack('0);
    Bit#(3) count = '0;
    Bool done = False;

    // 64 bits : 16 4-bit partitions
    for (Integer i = 15; i >= 0; i = i - 1) begin
      v = unpack(inp[i*4+3 : i*4]);
      count = '0;
      done = False;
      for (Integer j = 3; j >= 0; j = j - 1) begin
        if ((v[j] == 0) && !done) begin
          count = count + 1;
        end
        else begin
          done = True;
        end
      end

      rg_stage0_zcount[i] <= count;
    end

    rg_stage0_valid <= wr_valid;
    rg_stage0_inp_int <= wr_inp_int;
    rg_stage0_long <= wr_long;
    rg_stage0_sign <= sign;
    rg_stage0_inp <= inp;
    rg_stage0_rmode <= wr_rounding_mode;
  endrule

  // stage 1: final leading zero computation for word and long, determine mantissa and exponent
  rule rl_stage1;
    Bit#(7) count64 = '0;
    Bit#(6) count32 = '0;
    Bool done = False;
    Bit#(7) zeros = '0;
    Bit#(7) index = '0;
    Bit#(11) local_expo = '0;

    let long = rg_stage0_long;
    let inp = rg_stage0_inp;

    done = False;
    for (Integer i = 15; i >= 0; i = i - 1) begin
      if (!done) begin
        if (rg_stage0_zcount[i] == 3'b100) begin // "0000"
          count64 = count64 + 4;
        end
        else begin
          count64 = count64 + zeroExtend(rg_stage0_zcount[i]);
          done = True;
        end
      end
    end

    done = False;
    for (Integer i = 15; i >= 8; i = i - 1) begin
      if (!done) begin
        if (rg_stage0_zcount[i] == 3'b100) begin // "0000"
          count32 = count32 + 4;
        end
        else begin
          count32 = count32 + zeroExtend(rg_stage0_zcount[i]);
          done = True;
        end
      end
    end

    // final zero count; exponent and mantissa computation
    zeros = (long==1) ? count64 : zeroExtend(count32);
    index = (long==1) ? (7'd63-zeros) : (7'd31-zeros);
    local_expo = (11'h3ff + zeroExtend(index));
    inp = inp << zeros;

    rg_stage1_valid <= rg_stage0_valid;
    rg_stage1_inp_int <= rg_stage0_inp_int;
    rg_stage1_long <= long;
    rg_stage1_sign <= rg_stage0_sign;
    rg_stage1_inp <= inp;
    rg_stage1_rmode <= rg_stage0_rmode;
    rg_stage1_local_expo <= local_expo;
  endrule

  // stage 2: final leading zero computation for word and long, determine mantissa and exponent
  rule rl_stage2;
    FloatingPoint#(11,52) out = defaultValue;                   
    Exception ex = defaultValue;  
    Bit#(1) guard, round, sticky, lv_roundup;
    Bit#(5) flags;
    Bit#(52) mant_prim;
    Bit#(54) lv_man;

    let inp_int = rg_stage1_inp_int;
    let long = rg_stage1_long;
    let sign = rg_stage1_sign;
    let inp = rg_stage1_inp;
    let rounding_mode = rg_stage1_rmode;
    let local_expo = rg_stage1_local_expo;

    guard = inp[10];
    round = inp[9];
    sticky = (|inp[8:0]);
    flags = {4'd0,(guard|round|sticky)};
    mant_prim = inp[62:11];
    lv_roundup = 0;
    lv_man = {2'b0, mant_prim};

    case(rounding_mode) 
      3'b000:  lv_roundup = guard & (mant_prim[0] | round | sticky);        // rnear_even
      3'b100:  lv_roundup = guard; //& (round | sticky | ~sign);            // rnear_away_zero
      3'b011:  lv_roundup = (guard | round | sticky) & (~sign);             // rmax
      3'b010:  lv_roundup = (guard | round | sticky) & (sign);              // rmin
      default: lv_roundup = 0;
    endcase

    if (lv_roundup == 1) begin
      lv_man = lv_man + 1;
    end
    let final_expo = (lv_man[52] == 1) ? (local_expo + 1) : local_expo;
    out = ((inp_int==0) || ((long==0) && (inp_int[31:0]==0))) ? defaultValue : FloatingPoint{sign : unpack(sign) , exp : final_expo , sfd : lv_man[51:0]};
    ex = ((inp_int==0) || ((long==0) && (inp_int[31:0]==0))) ? unpack(5'd0) : unpack(flags);

    wr_stage2_result <= ReturnType{valid:pack(rg_stage1_valid) ,value:out, ex: ex};
  endrule

  method Action start(Bit#(64) inp_int, Bit#(1) unsigned_bit, Bit#(1) long, Bit#(3) rounding_mode);
    wr_valid <= 1;
    wr_inp_int <= inp_int;
    wr_unsigned_bit <= unsigned_bit;
    wr_long <= long;
    wr_rounding_mode <= rounding_mode;
  endmethod

  method ReturnType#(11,52) receive();
   let stage2_result = wr_stage2_result;
   return stage2_result;
 endmethod

endmodule // mk_fpu_int_to_dp

/*****************************************************************************/
          ////////Single precision float to integer conversion///////////

interface Ifc_fpu_sp_to_int;
        method Action start(Tuple4#(FloatingPoint#(8,23), RoundMode ,Bit#(1), Bit#(1))data_in);
        method ReturnTypeInt#(64) receive();
endinterface
/**********************************************************************************************/

(*synthesize*)
 module mk_fpu_sp_to_int(Ifc_fpu_sp_to_int);

 Wire#(FloatingPoint#(8,23)) wr_inp_fp <-mkDWire(unpack(0));
 Wire#(RoundMode) wr_rounding_mode <-mkDWire(defaultValue);
 Wire#(Bit#(1)) wr_conv_unsign <-mkDWire(0);
 Wire#(Bit#(1)) wr_conv_long  <-mkDWire(0);
 Wire#(Bit#(1)) wr_valid <- mkDWire(0);
 Wire#(ReturnTypeInt#(64)) wr_stage2_result <- mkDWire(unpack(0));

 Reg#(Bit#(1)) rg_stage0_valid <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_sign <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_conv_unsign <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_conv_long <-mkReg(0);
 Reg#(RoundMode) rg_stage0_rounding_mode <- mkReg(defaultValue);
 Reg#(Bit#(64)) rg_stage0_final_result  <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_inf  <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_invalid <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_zero <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_inexact <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_man_zero <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_overflow <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_denormal <-mkReg(0);
 Reg#(Int#(8)) rg_stage0_original_exponent <-mkReg(0);
 Reg#(Bool) rg_stage0_to_round <-mkReg(False);
 Reg#(Bit#(TAdd#(23, 64))) rg_stage0_final_man <-mkReg(0);
 Reg#(FloatingPoint#(8,23)) rg_stage0_inp_fp <-mkReg(unpack(0));

 Reg#(Bit#(1)) rg_stop_conv <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_valid <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_round_up <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_conv_unsign <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_conv_long <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_inf<-mkReg(0);
 Reg#(Bit#(23)) rg_stage1_sfd1 <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_overflow<-mkReg(0);
 Reg#(Bit#(1))rg_stage1_inexact1<-mkReg(0);
 Reg#(Bit#(1))rg_stage1_inexact2<-mkReg(0);
 Reg#(Bit#(1))rg_stage1_invalid<-mkReg(0);
 Reg#(Int#(8)) rg_stage1_original_exponent <-mkReg(0);
 Reg#(Bit#(64)) rg_stage1_final_result  <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_sign <-mkReg(0);
 Reg#(RoundMode) rg_stage1_rounding_mode <-mkReg(defaultValue);
 Reg#(Bool) rg_stage1_to_round <-mkReg(False);
 Reg#(Bit#(1)) rg_stage1_add_one <-mkReg(0);

/*****************************************************************************************************/
rule rl_stage0;                             /*** checking the special conditions, original_exp***/

  let lv_sign = pack(wr_inp_fp.sign);
  let lv_exponent  = wr_inp_fp.exp;
  let lv_sfd  = wr_inp_fp.sfd;
  let rnd =wr_rounding_mode;

Exception exc = ex_flag(wr_inp_fp);         // calculating the exceptions from input operand

 Bit#(5) lv_exc = pack(exc);
 let conv_unsign = wr_conv_unsign;
 let conv_long = wr_conv_long;
  
Bool rne = (rnd == Rnd_Nearest_Even);
Bool rtz = (rnd == Rnd_Zero);
Bool rdn = (rnd == Rnd_Minus_Inf);
Bool rup = (rnd == Rnd_Plus_Inf);
Bool rmm = (rnd == Rnd_Nearest_Away_Zero);

Bit#(1) lv_denormal =lv_exc[4];             // assigning the flags      
Bit#(1) lv_zero     = lv_exc[3];                  
Bit#(1) lv_inf      = lv_exc[1];                  

Bit#(1) lv_overflow = 0;
Bit#(1) lv_inexact = 0;
Bit#(1) lv_invalid =(lv_exc[0]|lv_exc[2]);
Bit#(1) lv_manzero = |lv_sfd;

Bit#(9) lv_exp = {1'b0,lv_exponent};
Int#(8) lv_original_exponent = unpack(truncate(lv_exp - 127));      //unbiasing the exponent and finding the original value

Bool to_round=False;
Bit#(64) final_result = 0;
Bit#(TAdd#(23, 64)) final_man = {'0,1'b1,lv_sfd};
Bit#(1) stop_conv=0;

if(lv_zero == 1)                            // special condition checking            
  final_result = 0;                         // checking the zero, -ve exponent values and assigning invalid, inexact flags  
else if(lv_denormal == 1 || (lv_original_exponent <= -1 && (lv_inf|lv_invalid) == 0))  begin
  stop_conv=1;                              // stop_conv flag =1 for bypassing the next stages
    if(lv_sign==1 && conv_unsign==1 && ((lv_original_exponent==-1 && (rmm||(rne && lv_manzero==1))) || (lv_original_exponent<=-1 &&rdn)))
      lv_invalid = 1; 
    else 
      lv_inexact = 1;
    if(lv_sign == 0 && rup)     
         final_result = 1;
    else if(rdn && lv_sign == 1 && conv_unsign == 0)
         final_result = '1;
    else if(lv_original_exponent == -1 && (rmm||(rne && lv_manzero == 1))) begin
       if(lv_sign == 0)
         final_result = 1;   
       else if(conv_unsign == 0)
          final_result = '1;
        else 
           final_result = 0;
     end   
     else 
       final_result = 0;
  end 
  
rg_stage0_valid <= wr_valid;
rg_stage0_sign <=lv_sign;
rg_stage0_conv_unsign<=conv_unsign;
rg_stage0_conv_long<=conv_long;
rg_stage0_rounding_mode<=wr_rounding_mode;
rg_stage0_final_result <= final_result;
rg_stage0_inf <=lv_inf;
rg_stage0_invalid<=lv_invalid;
rg_stage0_zero<=lv_zero;
rg_stage0_inexact<=lv_inexact;
rg_stage0_man_zero<=lv_manzero;
rg_stage0_original_exponent<=lv_original_exponent;
rg_stage0_final_man<=final_man;
rg_stage0_overflow<=lv_overflow;
rg_stage0_inp_fp<=wr_inp_fp;
rg_stage0_denormal<=lv_denormal;
rg_stop_conv <=stop_conv;
rg_stage0_to_round<=to_round;
endrule
/**********************************************************************************************************/
rule rl_stage1;                             /***sp to signed,unsigned word & long conversion & final rounding***/

Bit#(1) lv_sign1=rg_stage0_sign;
Bool to_round1 =False;
Bit#(64) final_result1 = 0;
Bit#(TAdd#(23, 64)) final_man1 = 0;
Bit#(23) lv_sfd1 = 0;
Bit#(1) lv_invalid1 = 0;
Bit#(1) lv_inexact1=0;

if (rg_stage0_zero==1)   begin              // Zero condition from stage0
    rg_stage1_final_result<=0;
    to_round1  =rg_stage0_to_round; 
    lv_invalid1=0;       
end
                                            //negative exponent condition from stage0
else if (((rg_stage0_invalid==1)&& (rg_stop_conv==1))||((rg_stage0_inexact==1) && (rg_stop_conv==1)))  begin
      rg_stage1_final_result <=rg_stage0_final_result;
      to_round1= rg_stage0_to_round;
      lv_invalid1=rg_stage0_invalid;
end
    
else begin      

 final_man1 = rg_stage0_final_man;
 lv_sfd1 = rg_stage0_inp_fp.sfd;
 lv_invalid1 = rg_stage0_invalid;

if (rg_stage0_conv_long == 0) begin         //sp to signed word conversion
  if(rg_stage0_conv_unsign == 0) begin
     Bit#(31) all_ones = '1;
      if(rg_stage0_inf == 1 || lv_invalid1 == 1) begin  
        final_result1 = (lv_sign1 ==1) ?(lv_invalid1==1? zeroExtend(all_ones) : signExtend(32'h80000000)) : zeroExtend(all_ones); 
      end
      else if(rg_stage0_original_exponent < 'd31)   begin
        final_man1 =  final_man1 << rg_stage0_original_exponent;
        Bit#(32) y = final_man1[54:23];     
        final_result1 = signExtend(y);
        lv_sfd1 = final_man1[22:0];         
        to_round1 = True;
      end
      else if(rg_stage0_original_exponent >= 'd31) begin
          lv_invalid1 = 1;
          if(lv_sign1 == 0)
             final_result1 = zeroExtend(all_ones);
           else  begin
              if(rg_stage0_original_exponent == 'd31 && rg_stage0_man_zero == 0)
                lv_invalid1 = 0 ;        //Since we are exactly representing the number? 
                final_result1 = signExtend(32'h80000000);
           end
      end
   end
   else begin                              //sp to unsigned word conversion  
       Bit#(32) all_ones = '1;
       if(rg_stage0_inf == 1 || lv_invalid1 == 1)
          final_result1 = (lv_sign1==1) ? (lv_invalid1==1? signExtend(all_ones) : '0) : signExtend(all_ones); 
       else if(rg_stage0_original_exponent < 'd32)  begin
         final_man1 = final_man1 << rg_stage0_original_exponent;
         Bit#(32) y = final_man1[54:23];      
         final_result1 = signExtend(y);
         lv_sfd1 = final_man1[22:0];                     
         to_round1 = True;
       end
       else if(rg_stage0_original_exponent >= 'd32)  begin
          lv_invalid1 = 1;
          if(lv_sign1 == 0)
             final_result1 = signExtend(all_ones);
           else
              final_result1 = '0;
       end
    end
 end
 else begin                                 // sp to signed long conversion
  if(rg_stage0_conv_unsign == 0) begin 
    Bit#(63) all_ones = '1;
     if(rg_stage0_inf == 1 || lv_invalid1 == 1)
       final_result1 = (lv_sign1==1) ?(lv_invalid1==1? zeroExtend(all_ones) : signExtend(64'h8000000000000000)) : zeroExtend(all_ones); 
     else if(rg_stage0_original_exponent < 'd63)  begin
       final_man1 = final_man1 << rg_stage0_original_exponent;
       Bit#(64) y = final_man1[86:23];
       final_result1 = y;
       lv_sfd1 = final_man1[22:0];         
       to_round1 = True;
     end
     else if(rg_stage0_original_exponent >= 'd63)  begin
       lv_invalid1 = 1;
        if(lv_sign1 == 0)
           final_result1 = zeroExtend(all_ones);
        else begin
           if(rg_stage0_original_exponent == 'd63 && rg_stage0_man_zero == 0 )
             lv_invalid1 = 0;  //Since we are exactly representing the input number
             final_result1 = signExtend(64'h8000000000000000);
        end
     end
  end
  else begin                                //sp to unsigned long conversion
    Bit#(64) all_ones = '1;
     if((rg_stage0_inf == 1) || (lv_invalid1 == 1)) begin
       final_result1 = (lv_sign1==1) ? (lv_invalid1==1? signExtend(all_ones) : '0) : signExtend(all_ones);
     end
     else if(rg_stage0_original_exponent < 'd64) begin
       final_man1 = final_man1 << (rg_stage0_original_exponent);
       Bit#(64) y = final_man1[86:23];
       final_result1 = y;
       lv_sfd1 =  final_man1[22:0];   
       to_round1 = True;
     end
     else if(rg_stage0_original_exponent >= 'd64) begin
       lv_invalid1 = 1;
         if(lv_sign1 == 0)
           final_result1 = signExtend(all_ones);
         else
           final_result1 = '0;
     end
  end
end
                                            // final rounding of the result 
  Bit#(1) lv_guard = lv_sfd1[22];	    //MSB of the already shifted mantissa is guard bit
  Bit#(1) lv_round = lv_sfd1[21];	    //next bit is round bit
  Bit#(1) lv_sticky = |(lv_sfd1<<2);	     //remaining bits determine the sticky bit
  Bit#(1) lv_round_up = 0;
  
   lv_inexact1 = lv_guard | lv_round | lv_sticky;

 if(to_round1)  begin
   case(rg_stage0_rounding_mode) 
     Rnd_Nearest_Even     :  lv_round_up = lv_guard & (final_result1[0] | lv_round | lv_sticky);      
     Rnd_Nearest_Away_Zero:  lv_round_up = lv_guard; //& (lv_round | lv_sticky | ~lv_sign1);               
     Rnd_Minus_Inf        :  lv_round_up = lv_inexact1 & (lv_sign1);                
     Rnd_Plus_Inf         : lv_round_up = lv_inexact1 & (~lv_sign1);                
     default              : lv_round_up = 0;
   endcase
            
   if(lv_round_up == 1) begin                      //Should set the overflow flag here right?
     lv_invalid1 = 1;
   
      if(rg_stage0_conv_long == 0 && rg_stage0_conv_unsign == 0 && rg_stage0_original_exponent == 30 && final_result1[30:0] == '1 && lv_sign1 == 1'b0) 
	   final_result1 ='h7fffffff;		       //Overflow..  Beyond representable number after rounding
            
      else if(rg_stage0_conv_long == 0 && rg_stage0_conv_unsign == 1 && rg_stage0_original_exponent == 31 && final_result1[31:0] == '1 && lv_sign1 == 1'b0)
            final_result1 = '1;
       
      else if(rg_stage0_conv_long == 1 && rg_stage0_conv_unsign == 0 && rg_stage0_original_exponent == 62 && final_result1[62:0] == '1 && lv_sign1 == 1'b0) 
 	    final_result1 = 64'h7fffffffffffffff;        //Overflow..  Beyond representable number after rounding
          
      else if(rg_stage0_conv_long == 1 && rg_stage0_conv_unsign == 1 && rg_stage0_original_exponent == 63 && final_result1[63:0] == '1 && lv_sign1 == 1'b0)
           final_result1 = 64'hffffffffffffffff;               
        
      else  begin    
           lv_invalid1 = 0;
           final_result1 = final_result1 + 1;
           if(rg_stage0_conv_long == 0 && final_result1[31]==1)
               final_result1 = signExtend(final_result1[31:0]);
      end        
   end                                     // end to lv_round_up=1  
 end                                        //end to to_round1
 rg_stage1_final_result <=final_result1;
end

rg_stage1_to_round<=to_round1;
rg_stage1_invalid<=lv_invalid1;
rg_stage1_inexact1<=rg_stage0_inexact;
rg_stage1_inexact2<=lv_inexact1;
rg_stage1_original_exponent<= rg_stage0_original_exponent;
rg_stage1_conv_unsign<=rg_stage0_conv_unsign;
rg_stage1_conv_long <=rg_stage0_conv_long;
rg_stage1_inf<= rg_stage0_inf;
rg_stage1_overflow<=rg_stage0_overflow;
rg_stage1_rounding_mode<=rg_stage0_rounding_mode;
rg_stage1_valid <= rg_stage0_valid;
rg_stage1_sign<=lv_sign1;

endrule
/***************************************************************************************************/
rule rl_stage2;                             /*** flag set for integer result***/

Bit#(64) final_result2 =rg_stage1_final_result ;
Bit#(1) lv_invalid2 = rg_stage1_invalid;
Bit#(1) lv_overflow2=rg_stage1_overflow;
Bit#(1) lv_inexact2=0;
Bit#(1) lv_sign1=rg_stage1_sign;

 if((rg_stage1_to_round)) begin
    if(rg_stage1_conv_unsign == 0 && lv_sign1 == 1'b1)begin		//Negating the output if floating point number is negative and converted to signed word/long
       final_result2 = ~final_result2 + 1;
      
       if(rg_stage1_conv_long == 0 && final_result2[31] == 1)
          final_result2 = signExtend(final_result2[31:0]);
    end
    else if(rg_stage1_conv_unsign == 1 && lv_sign1 == 1'b1) begin
        final_result2 = 0;
        lv_invalid2 = 1;
     end
 end 

   lv_inexact2 = rg_stage1_inexact1 | rg_stage1_inexact2;       // inexact flag set for integer result
	  
    if((lv_invalid2|rg_stage1_inf) == 1) begin        //What about Quiet NaN?? What does the Spec Say?
        lv_overflow2 = 0;
        lv_inexact2 = 0;
    end

 Bit#(5) fflags={lv_invalid2|rg_stage1_inf,1'b0,lv_overflow2,1'b0,lv_inexact2};
 Exception flags = unpack(fflags);  

wr_stage2_result <= ReturnTypeInt{valid:rg_stage1_valid,value:final_result2,ex:flags};

endrule
/*********************************************************************************************************/

method Action start(Tuple4#(FloatingPoint#(8,23) , RoundMode ,Bit#(1) , Bit#(1))data_in);
    wr_inp_fp    <= tpl_1(data_in);
wr_rounding_mode <= tpl_2(data_in);
wr_conv_unsign   <= tpl_3(data_in);
wr_conv_long     <= tpl_4(data_in);
wr_valid         <=1'b1;
 endmethod

method ReturnTypeInt#(64) receive();
let stage2_result = wr_stage2_result;
return stage2_result;
endmethod
endmodule                          //end to sp_int

/*****************************************************************************************************/

       ////////Double precision float to integer conversion///////////

interface Ifc_fpu_dp_to_int;
        method Action start(Tuple4#(FloatingPoint#(11,52), RoundMode ,Bit#(1), Bit#(1))data_in);
        method ReturnTypeInt#(64) receive();
endinterface
/****************************************************************************/

(*synthesize*)
    module mk_fpu_dp_to_int(Ifc_fpu_dp_to_int);

 Wire#(FloatingPoint#(11,52)) wr_inp_fp <-mkDWire(unpack(0));
 Wire#(RoundMode) wr_rounding_mode <-mkDWire(defaultValue);
 Wire#(Bit#(1)) wr_conv_unsign <-mkDWire(0);
 Wire#(Bit#(1)) wr_conv_long  <-mkDWire(0);
 Wire#(Bit#(1)) wr_valid <- mkDWire(0);
 Wire#(ReturnTypeInt#(64)) wr_stage2_result <- mkDWire(unpack(0));

 Reg#(Bit#(1)) rg_stage0_valid <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_sign <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_conv_unsign <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_conv_long <-mkReg(0);
 Reg#(RoundMode) rg_stage0_rounding_mode <- mkReg(defaultValue);
 Reg#(Bit#(64)) rg_stage0_final_result  <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_inf  <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_invalid <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_zero <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_inexact <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_man_zero <-mkReg(0);
 Reg#(Bit#(1)) rg_stage0_overflow <-mkReg(0);
 Reg#(Int#(11)) rg_stage0_original_exponent <-mkReg(0);
 Reg#(Bool) rg_stage0_to_round <-mkReg(False);
 Reg#(Bit#(TAdd#(52, 64))) rg_stage0_final_man <-mkReg(0);
 Reg#(FloatingPoint#(11,52)) rg_stage0_inp_fp <-mkReg(unpack(0));
 Reg#(Bit#(52)) rg_stage0_sfd <- mkReg(0);

 Reg#(Bit#(1)) rg_stop_conv <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_valid <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_round_up <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_conv_unsign <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_conv_long <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_inf<-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_overflow<-mkReg(0);
 Reg#(Bit#(1))rg_stage1_inexact1<-mkReg(0);
 Reg#(Bit#(1))rg_stage1_inexact2<-mkReg(0);
 Reg#(Bit#(1))rg_stage1_invalid<-mkReg(0);
 Reg#(Int#(11)) rg_stage1_original_exponent <-mkReg(0);
 Reg#(Bit#(1)) rg_stage1_man_zero <-mkReg(0);
 Reg#(Bit#(64)) rg_stage1_final_result  <-mkReg(0);
 Reg#(Bool) rg_stage1_to_round <-mkReg(False);
 Reg#(Bit#(1)) rg_stage1_sign <-mkReg(0);
 Reg#(FloatingPoint#(11,52)) rg_stage1_inp_fp <-mkReg(unpack(0));
 Reg#(RoundMode) rg_stage1_rounding_mode <- mkReg(defaultValue);

/*****************************************************************************/
   rule rl_stage0;     /*** checking the special conditions, original_exp & dp_signed, unsigned word conversion***/

  let lv_sign = pack(wr_inp_fp.sign);
  let lv_exponent  = wr_inp_fp.exp;
  let lv_sfd  = wr_inp_fp.sfd;
  let rnd =wr_rounding_mode;
  Exception exc = ex_flag(wr_inp_fp);       // calculating the exceptions from input operand
 
 Bit#(5) lv_exc = pack(exc);

 let conv_unsign = wr_conv_unsign;
 let conv_long = wr_conv_long;

Bool rne = (rnd == Rnd_Nearest_Even);
Bool rtz = (rnd == Rnd_Zero);
Bool rdn = (rnd == Rnd_Minus_Inf);
Bool rup = (rnd == Rnd_Plus_Inf);
Bool rmm = (rnd == Rnd_Nearest_Away_Zero);

Bit#(1) lv_denormal =lv_exc[4];             // assigning the flags       
Bit#(1) lv_zero     = lv_exc[3];                  
Bit#(1) lv_inf      = lv_exc[1]; 

Bit#(1) lv_overflow = 0;
Bit#(1) lv_inexact = 0;
Bit#(1) lv_invalid =(lv_exc[0]|lv_exc[2]);
Bit#(1) lv_manzero = |lv_sfd;

Bit#(12) lv_exp = {1'b0,lv_exponent};
Int#(11) lv_original_exponent = unpack(truncate(lv_exp - 1023));  //unbiasing the exponent and finding the original value

Bool to_round=False;
Bit#(64) final_result = 0;
Bit#(TAdd#(52, 64)) final_man = {'0,1'b1,lv_sfd};
Bit#(1) stop_conv=0;
                                            // special condition checking 
if(lv_zero == 1)                            // checking the zero, -ve exponent values and assigning invalid, inexact flags 
   final_result = 0;
else if(lv_denormal == 1 || (lv_original_exponent <= -1 && (lv_inf|lv_invalid) == 0))  begin
   if(lv_sign==1 && conv_unsign==1 && ((lv_original_exponent==-1 && (rmm||(rne && lv_manzero==1))) || (lv_original_exponent<=-1 &&rdn)))begin
      stop_conv=1;                          // stop_conv flag =1 for bypassing the next stages  
      lv_invalid = 1;
   end
   else
      stop_conv=1; 
      lv_inexact = 1;
      if(lv_sign == 0 && rup)
         final_result = 1;
      else if(rdn && lv_sign == 1 && conv_unsign == 0)
         final_result = '1;
      else if(lv_original_exponent == -1 && (rmm||(rne && lv_manzero == 1)))  begin
         if(lv_sign == 0)
            final_result = 1;
         else if(conv_unsign == 0) 
            final_result = '1;
         else
            final_result = 0;
      end
      else 
          final_result = 0;
end

else if (conv_long == 0)   begin            // dp to signed word conversion
   if(conv_unsign == 0)  begin 
      Bit#(31) all_ones = '1;
      if(lv_inf == 1 || lv_invalid == 1)   begin 
         final_result = (lv_sign ==1) ?(lv_invalid==1? zeroExtend(all_ones) : signExtend(32'h80000000)) : zeroExtend(all_ones); 
      end
      else if(lv_original_exponent < 'd31) begin
          final_man =  final_man << lv_original_exponent;
          Bit#(32) y = final_man[83:52];     
          final_result = signExtend(y);
          lv_sfd = final_man[51:0];         
          to_round = True;
      end
      else if(lv_original_exponent >= 'd31) begin
         lv_invalid = 1;
         if(lv_sign == 0)
             final_result = zeroExtend(all_ones);
         else begin
            if(lv_original_exponent == 'd31 && lv_manzero == 0) 
              lv_invalid = 0 ;              //Since we are exactly representing the number? 
              final_result = signExtend(32'h80000000);
         end
      end
   end
   else begin                               //dp to unsigned word conversion 
       Bit#(32) all_ones = '1;
       if(lv_inf == 1 || lv_invalid == 1)
            final_result = (lv_sign==1) ? (lv_invalid==1? signExtend(all_ones) : '0) : signExtend(all_ones); 
       else if(lv_original_exponent < 'd32) begin
            final_man = final_man << lv_original_exponent;
            Bit#(32) y = final_man[83:52];      
            final_result = signExtend(y);
            lv_sfd = final_man[51:0];                     
            to_round = True;
       end
       else if(lv_original_exponent >= 'd32) begin
            lv_invalid = 1;
            if(lv_sign == 0)
                final_result = signExtend(all_ones);
            else
                final_result = '0;
       end
   end
end

rg_stage0_valid <= wr_valid;
rg_stage0_sign <=lv_sign;
rg_stage0_conv_unsign<=conv_unsign;
rg_stage0_conv_long<=conv_long;
rg_stage0_rounding_mode<=wr_rounding_mode;
rg_stage0_final_result <= final_result;
rg_stage0_inf <=lv_inf;
rg_stage0_invalid<=lv_invalid;
rg_stage0_zero<=lv_zero;
rg_stage0_inexact<=lv_inexact;
rg_stage0_man_zero<=lv_manzero;
rg_stage0_original_exponent<=lv_original_exponent;
rg_stage0_final_man<=final_man;
rg_stage0_overflow<=lv_overflow;
rg_stage0_inp_fp<=wr_inp_fp;
rg_stop_conv<=stop_conv;
rg_stage0_to_round <=to_round;
rg_stage0_sfd<=lv_sfd;

endrule
/*********************************************************************/
rule rl_stage1;       /***dp to signed,unsigned long conversion & final rounding***/

Bit#(1) lv_sign1=rg_stage0_sign;
Bool to_round1 =False;
Bit#(64) final_result1 = 0;
Bit#(TAdd#(52, 64)) final_man1 = 0;
Bit#(52) lv_sfd1 = 0;
Bit#(1) lv_invalid1 = 0;
Bit#(1) lv_inexact2=0;

if (rg_stage0_zero==1)   begin              // Zero condition from stage0
   rg_stage1_final_result<=0;
    to_round1  =False; 
    lv_invalid1=0;
    lv_inexact2=0;
end

else if (((rg_stage0_invalid==1)|| (rg_stage0_inexact==1))&&(rg_stop_conv==1)) begin    //negative exponent condition from stage0
      rg_stage1_final_result<=rg_stage0_final_result;
      to_round1= rg_stage0_to_round;
      lv_invalid1=rg_stage0_invalid;
      lv_inexact2=rg_stage0_inexact;
      lv_sfd1=rg_stage0_sfd;
end

else begin
 if ((rg_stage0_conv_long==0)) begin        //conv_long=0 conversion from stage0
     final_man1 = rg_stage0_final_man;
     lv_sfd1 = rg_stage0_sfd;
     lv_invalid1 = rg_stage0_invalid;
     to_round1 = rg_stage0_to_round;
     lv_inexact2 = rg_stage0_inexact;
     final_result1 = rg_stage0_final_result;
     to_round1= rg_stage0_to_round;
 end

 else begin
    final_result1=0;
    final_man1={'0,1'b1,rg_stage0_inp_fp.sfd};      
    lv_invalid1 = rg_stage0_invalid;
    lv_inexact2 = rg_stage0_inexact;
    to_round1=False;

     if (rg_stage0_conv_long==1) begin       //dp to signed long conversion
         if(rg_stage0_conv_unsign == 0) begin 
            Bit#(63) all_ones = '1;
            if(rg_stage0_inf == 1 || lv_invalid1 == 1)
              final_result1 = (lv_sign1==1) ?(lv_invalid1==1? zeroExtend(all_ones) : signExtend(64'h8000000000000000)) : zeroExtend(all_ones); 
            else if(rg_stage0_original_exponent < 'd63) begin
                final_man1 = final_man1 << rg_stage0_original_exponent;
                Bit#(64) y = final_man1[115:52];
                final_result1 =zeroExtend(y);
                lv_sfd1 = final_man1[51:0];         
                to_round1 = True;
            end
            else if(rg_stage0_original_exponent >= 'd63) begin
                 lv_invalid1 = 1;
                 if(lv_sign1 == 0)
                     final_result1 = zeroExtend(all_ones);
                 else begin
                    if(rg_stage0_original_exponent == 'd63 && rg_stage0_man_zero == 0 ) 
                        lv_invalid1 = 0;  //Since we are exactly representing the input number
                        final_result1 = signExtend(64'h8000000000000000);
                 end
            end
         end                                // end to conv_unsign=0
         else begin                         // dp to unsigned long conversion
             Bit#(64) all_ones = '1;
             if(rg_stage0_inf == 1 || lv_invalid1 == 1)
                 final_result1 = (lv_sign1==1) ? (lv_invalid1==1? signExtend(all_ones) : '0) : signExtend(all_ones);
             else if(rg_stage0_original_exponent < 'd64) begin
                 final_man1 = final_man1 << (rg_stage0_original_exponent);
                 Bit#(64) y = final_man1[115:52];
                 final_result1 =zeroExtend(y);
                 lv_sfd1 =  final_man1[51:0];   
                 to_round1 = True;
             end
             else if(rg_stage0_original_exponent >= 'd64) begin
                 lv_invalid1 = 1;
                 if(lv_sign1 == 0)
                    final_result1 = signExtend(all_ones);
                 else
                    final_result1 = '0;
             end
         end 
     end                                    //end to conv_long=1
 end
                                            // final rounding of the result
   Bit#(1) lv_guard = lv_sfd1[51];	        //MSB of the already shifted mantissa is guard bit
   Bit#(1) lv_round = lv_sfd1[50];	        //next bit is round bit
   Bit#(1) lv_sticky = |(lv_sfd1<<2);		//remaining bits determine the sticky bit
   Bit#(1) lv_round_up = 0;
   
    lv_inexact2 = lv_guard | lv_round |lv_sticky;
      
   if(to_round1) begin
      case(rg_stage0_rounding_mode) 
          Rnd_Nearest_Even     :  lv_round_up = lv_guard & (final_result1[0] | lv_round | lv_sticky);      
          Rnd_Nearest_Away_Zero:  lv_round_up = lv_guard; //& (lv_round | lv_sticky | ~lv_sign1);               
          Rnd_Minus_Inf        :  lv_round_up = lv_inexact2 & (lv_sign1);          //lv_inexact1 & (lv_sign1);                
          Rnd_Plus_Inf         : lv_round_up = lv_inexact2 & (~lv_sign1);          // lv_inexact1 & (~lv_sign1);     
          default              : lv_round_up = 0;
       endcase
      
  if(lv_round_up == 1) begin                      //Should set the overflow flag here right?
     lv_invalid1 = 1;
   
     if(rg_stage0_conv_long == 0 && rg_stage0_conv_unsign == 0 && rg_stage0_original_exponent == 30 && final_result1[30:0] == '1 && lv_sign1 == 1'b0) 
	     final_result1 =64'h7fffffff;	  //Overflow..  Beyond representable number after rounding
            
     else if(rg_stage0_conv_long == 0 && rg_stage0_conv_unsign == 1 && rg_stage0_original_exponent == 31 && final_result1[31:0] == '1 && lv_sign1 == 1'b0)
            final_result1 = 64'hffffffffffffffff;
       
     else if(rg_stage0_conv_long == 1 && rg_stage0_conv_unsign == 0 && rg_stage0_original_exponent == 62 && final_result1[62:0] == '1 && lv_sign1 == 1'b0) 
 	     final_result1 = 64'h7fffffffffffffff;  //Overflow..  Beyond representable number after rounding
          
     else if(rg_stage0_conv_long == 1 && rg_stage0_conv_unsign == 1 && rg_stage0_original_exponent == 63 && final_result1[63:0] == '1 && lv_sign1 == 1'b0)
            final_result1 = 64'hffffffffffffffff;               
        
     else  begin    
           lv_invalid1 = 0;
           final_result1 = final_result1 + 1;
           if(rg_stage0_conv_long == 0 && final_result1[31]==1)
               final_result1 = signExtend(final_result1[31:0]);
     end        
  end                     // end to lv_round_up=1
end                       // end to to_round1

rg_stage1_final_result <=final_result1;             
end                               

rg_stage1_to_round<=to_round1;
rg_stage1_invalid<=lv_invalid1;
rg_stage1_original_exponent<= rg_stage0_original_exponent;
rg_stage1_conv_unsign<=rg_stage0_conv_unsign;
rg_stage1_conv_long <=rg_stage0_conv_long;
rg_stage1_inf<= rg_stage0_inf;
rg_stage1_overflow<=rg_stage0_overflow;
rg_stage1_inexact1<=rg_stage0_inexact;
rg_stage1_inexact2<=lv_inexact2;
rg_stage1_rounding_mode<=rg_stage0_rounding_mode;
rg_stage1_valid <= rg_stage0_valid;
rg_stage1_sign<=lv_sign1;
rg_stage1_man_zero<=rg_stage0_man_zero;
rg_stage1_inp_fp <= rg_stage0_inp_fp;

endrule
/*************************************************************************/
rule rl_stage2;                             // flag set for integer result

Bit#(64) final_result2 =rg_stage1_final_result ;
Bit#(1) lv_invalid2 = rg_stage1_invalid;
Bit#(1) lv_overflow2=rg_stage1_overflow;
Bit#(1) lv_inexact2=0;
Bit#(1) lv_sign1=rg_stage1_sign;
Bit#(52) lv_inp_mant=rg_stage1_inp_fp.sfd;
RoundMode rnd =rg_stage1_rounding_mode;

 if((rg_stage1_to_round)) begin
    if(rg_stage1_conv_unsign == 0 && rg_stage1_sign == 1'b1)begin		//Negating the output if floating point number is negative and converted to signed word/long
	final_result2 = ~final_result2 + 1;
        if(rg_stage1_conv_long == 0 && final_result2[31] == 1)
            final_result2 = signExtend(final_result2[31:0]);
    end
    else if(rg_stage1_conv_unsign == 1 && rg_stage1_sign == 1'b1) begin
	final_result2 = 0;
        lv_invalid2 = 1;
    end
end		  
  
lv_inexact2 = rg_stage1_inexact1 | rg_stage1_inexact2;   // inexact flag set for long integers
  
							//inexact flag set for 32 bit integers with exponent value =31
if (rg_stage1_conv_unsign==0 && rg_stage1_conv_long==0 && rg_stage1_original_exponent==31 && rg_stage1_sign==1) begin
    if(rg_stage1_man_zero==0) begin
       lv_invalid2=0;
       lv_inexact2=0;
     end
     else if(|lv_inp_mant[51:21]!=0)          //if inp mantissa has one's in higher order bits which results overflow flag 
          lv_invalid2=1;                      // overflow is ORed with invalid here
          
     else begin                               //special cases of inexact flag set
         if((lv_inp_mant[20]==1 && |lv_inp_mant[19:0]!=0 && rnd==Rnd_Nearest_Even) ||(lv_inp_mant[20]==1 && rnd==Rnd_Nearest_Away_Zero)||(|lv_inp_mant==1 && rnd==Rnd_Minus_Inf) )  
            lv_invalid2=1;
         else begin 
            lv_inexact2=1;
            lv_invalid2=0;
         end
    end 
 end

if((lv_invalid2|rg_stage1_inf) == 1) begin  //What about Quiet NaN?? What does the Spec Say?
      lv_overflow2 = 0;
      lv_inexact2 = 0;
   end
  

    Bit#(5) fflags={lv_invalid2|rg_stage1_inf,1'b0,lv_overflow2,1'b0,lv_inexact2};
    Exception flags = unpack(fflags);  
    wr_stage2_result <= ReturnTypeInt{valid:rg_stage1_valid,value:final_result2,ex:flags};

endrule

/*********************************************************************/

method Action start(Tuple4#(FloatingPoint#(11,52) , RoundMode ,Bit#(1) , Bit#(1))data_in);
    wr_inp_fp    <= tpl_1(data_in);
wr_rounding_mode <= tpl_2(data_in);
wr_conv_unsign   <= tpl_3(data_in);
wr_conv_long     <= tpl_4(data_in);
wr_valid         <=1'b1;
 endmethod

method ReturnTypeInt#(64) receive();
let stage2_result = wr_stage2_result;
return stage2_result;
endmethod
endmodule     // dp_int

/************************************************************************************************************/

endpackage

