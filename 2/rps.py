#!/usr/bin/env python

fc = ['A','B','C']
sc = ['X','Y','Z']

def getind(inp,arr):
    return arr.index(inp)

def result(in1,in2):
    i1=getind(in1,fc)
    i2=getind(in2,sc)
    if i1 <= i2:
        res = i2 - i1
    else:
        res = i2 + 3 - i1

#   print(in1,in2,res,i2)

    return {1:6, 2:0, 0:3}[res] + 1 + i2

def parse(line):
    l=line.split()
    return(l[0],l[1])

res = sum([result(*parse(line)) for line in open('data.txt','r')])
print(res)

def result2(in1,inres):
    i1=getind(in1,fc)
    ires=(getind(inres,sc) - 1)%3

    # ires: 0 lose 1 draw 2 win
    #        lose: i2 - i1 = 2
    #        draw: i2 - i1 = 0
    #        win:  i2 - i1 = 1

    in2 = sc[(ires+i1)%3]
#   print(in1,in2)

    return result(in1,in2)

res = sum([result2(*parse(line)) for line in open('data.txt','r')])

#with open('data.txt','r') as f:
#    for line in f:
#        print(parse(line))
#        print(result2(*parse(line)))

print(res)
