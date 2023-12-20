function Array#(Int#(32)) max_pooling(Int#(32) input_data[][]) provisos(Bitwise#(Bit#(32)));
int i, j, k, l, o;
Int#(32) out_pool[4];
o=0;
for (i=0;i<4;i=i+2) begin
 for(j=0;j<4;j=j+2) begin
     for(k=i;k<i+2;k=k+1) begin
        for (l=j;l<j+2;l=l+1) begin
            if(input_data[k][l]>out_pool[o]) begin
               out_pool[o]=input_data[k][l];
               end
               end
               end
               o=o+1;
               end
               end
               return out_pool;
               endfunction

