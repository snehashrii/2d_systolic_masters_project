import re
#Sliding window protocol
def convolution(input, weight):
 output=[]
 for row in range(0, 32):
        for column in range(0, 32):
            sum=0
            for i in range(0,3):
                for j in range(0,3):
                    o=input[i+row][column+j]*weight[i*3+j]
                    sum=sum+o         
            output.append(sum)           
 return output
#zero padding to equalise output and input
def padding(input):
    padded_input=[]
    p=[0 for i in range(0,34)]
    padded_input.append(p)
       
    for i in range(1,33):
        p=[]
        for j in range(0,34):
            if j==0:
                p.append(0)
            elif j==33:
                p.append(0)
            else:
                p.append(input[i-1][j-1])
        padded_input.append(p)
    padded_input.append([0 for i in range(0,34)])
    return padded_input

file=open("/Users/snehashri/Documents/reference_golden_sachin/input.txt","r")
iarr=[]
r=[]
g=[]
b=[]
count=0
for line in file:
    for l in line.split():
        iarr.append(l)
red=[]
green=[]
blue=[]
for i in iarr:
     count=count+1
     if count>2048:
         b.append(i)
     elif count>1024:
         g.append(i)
     else:
         r.append(i)
for i in range(0,32):
    _i=[]
    for j in range(0,32):
        _i.append(int(r[(i*32)+j].replace('0x',''),16))
    red.append(_i)
for i in range(0,32):
    _i=[]
    for j in range(0,32):
        _i.append(int(g[(i*32)+j].replace('0x',''),16))
    green.append(_i)
for i in range(0,32):
    _i=[]
    for j in range(0,32):
        _i.append(int(b[(i*32)+j].replace('0x',''),16))
    blue.append(_i)
with open('weights.txt', 'r') as file:
    file1 = file.read()

# Define a regular expression pattern to capture the desired matrix
pattern = r'tensor\(\[\[\[\s*(.*?)\],\s*\[\s*(.*?)\],\s*\[\s*(.*?)\]\]'

# Find the match using the pattern
match = re.findall(pattern, file1)
     

# Print the result
warr=[]
for i in range(0,64):
    w=[]
    for j in range(0,3):
       for k in range(0,3):
          w.append(float(match[i][j].split(',')[k]))
    warr.append(w)

red=padding(red)
green=padding(green)
blue=padding(blue)
red_output=[]
green_output=[]
blue_output=[]

#final output
for i in range(0, 64):
    output=convolution(red, warr[i])
    red_output.append(output)
for i in range(0, 64):
    output=convolution(green, warr[i])
    green_output.append(output)
for i in range(0, 64):
    output=convolution(blue, warr[i])
    blue_output.append(output)

for i in range(0, len(red_output[0])):
      print(red_output[0][i])
