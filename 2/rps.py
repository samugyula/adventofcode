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

    return {1:6, 2:0, 0:3}[res] + 1 + i2

def parse(line):
    l=line.split()
    return(l[0],l[1])

res = sum([result(*parse(line)) for line in open('data.txt','r')])
