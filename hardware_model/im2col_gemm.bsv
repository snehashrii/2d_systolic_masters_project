import Vector ::*;
import mac_array ::*;
import pooling ::*;
import fpu_common::*;
import FloatingPoint::*;

typedef struct { 
    Array#(Array#(int)) windows;
    } Inps deriving  (Bits);

function Inps fn_im2col(int input_data[][]) provisos(Bitwise#(Bit#(32)));
Inps h;
int i, j,k,l,m, count, irow;
count=0;
irow=0;
int input1[9], input2[9][1024];
for (i=0;i<32;i=i+1) begin
   for(j=0;j<32;j=j+1) begin 
     for(k=i;k<i+3;k=k+1)  begin
        for(l=j;l<j+3;l=l+1) begin
           input1[count]=input_data[k][l];
           count=count+1;
           end
        end
        count=0;
        for (m=0;m<9;m=m+1) begin
           input2[m][irow]=input1[m];
           end

        irow=irow+1;
      end
      end
h.windows=input2;



return h;
endfunction


