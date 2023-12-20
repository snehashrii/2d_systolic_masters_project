There are four folders here ;

1. Software model - has the python script that serves as the golden reference model for the accelerator
2. Hardware model - bluespec files with the main top.bsv file .

Commands to run the bluespec file:

Compile : bsc -u -steps 100000000 -verilog top.bsv +RTS -K1720M -RTS 
Simulation : bsc -o sim -e top top.v                                                                            
               ./sim >  ac.out

Steps to download the bluespec compiler in linux:
https://github.com/B-Lang-org/bsc/blob/main/INSTALL.md

3. inputs - contains the input and weight files
4. outputs - has both the software as well as hardware output.
