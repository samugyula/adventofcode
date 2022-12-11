#!/usr/bin/env python
from functools import reduce
import operator

class Monkey:
    def __init__(self,lines,n):

        def getVal(n):
            res = lines[n].split()[-1]
            if res.isnumeric(): res = int(res)
            return res

        for nn,line in enumerate(lines):

            if f'Monkey {n}:' in line:

                items = lines[nn+1].split()[2:]
                items = " ".join(items).split(',')
                self.items = list(map(int,items))

                mult = getVal(nn+2)
                op = lines[nn+2].split()[-2]
                if mult == "old" and op == "*":
                    self.op = lambda x: x*x
                elif (op) == '*':
                    self.op = lambda x: x*mult
                elif (op) == '+':
                    self.op = lambda x: x+mult
                else:
                    raise NotImplementedError(op)

                test = getVal(nn+3)
                self.test = test
                t1 = getVal(nn+4)
                t2 = getVal(nn+5)
                self.to = lambda x: t1 if (x % test == 0) else t2

                break

        self.nInspected = 0

    def throw(self,monkeys,modWith,div=None):
        for item in self.items:
            self.nInspected += 1
            val = self.op(item)
            if not (div is None):
                val = val // div
            val = val % modWith
            monkeys[self.to(val)].items += [val]
        self.items = []

def printRes(monkeys):
    insp = [m.nInspected for m in monkeys]
    insp = sorted(insp)[-2:]
    print(insp[0]*insp[1])

####################################################

lines = [line for line in open('data.txt')]
nMonkeys = len([l for l in lines if f'Monkey' in l])

monkeys = [Monkey(lines,n) for n in range(nMonkeys)]

divtests = [m.test for m in monkeys]
modWith = reduce(operator.mul, divtests, 1)

rounds=20

for r in range(rounds):
    for m in monkeys:
        m.throw(monkeys,modWith,div=3)

printRes(monkeys)

monkeys = [Monkey(lines,n) for n in range(nMonkeys)]
rounds=10000

for r in range(rounds):
    for m in monkeys:
        m.throw(monkeys,modWith)

printRes(monkeys)
