import FloatingPoint::*;
import fpu_common::*;

function Array#(Array#(int)) zero_padding(FloatingPoint#(11,52) input_data[][]);

int padded_input[34][34];
for (int i=0;i<34;i=i+1) begin
   padded_input[0][i]=3072;
   padded_input[33][i]=3072;
end
for(int j=1;j<33;j=j+1) begin
   for(int k=0;k<34;k=k+1) begin
       if (k==0 || k==33)
        padded_input[j][k]=3072;
       else 
         padded_input[j][k]=(j-1)*32+(k-1);
         end
   end
   return padded_input;
endfunction