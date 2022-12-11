#!/usr/bin/env python
import resource, sys
resource.setrlimit(resource.RLIMIT_STACK, (2**29,-1))
sys.setrecursionlimit(10**6)

lines = [line for line in open('data.txt')]
nMonkeys = len([l for l in lines if f'Monkey' in l])

class Monkey:
    def __init__(self,lines,n):

        self.ind = n

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
                self.specifics = lines[nn+2]
                for i in range(3,6):
                    self.specifics += lines[nn+i]
                op = lines[nn+2].split()[-2]
                if mult == "old" and op == "*":
                    self.op = lambda x: x*x
#                   self.invop = lambda x: int(x**0.5)
                elif (op) == '*':
                    self.op = lambda x: x*mult
#                   self.invop = lambda x: int(x/mult)
                elif (op) == '+':
                    self.op = lambda x: x+mult
#                   self.invop = lambda x: x-mult
                else:
                    raise NotImplementedError(op)

                test = getVal(nn+3)
                self.modWith = test
                t1 = getVal(nn+4)
                t2 = getVal(nn+5)
                self.to = lambda x: t1 if (x % test == 0) else t2

                self.newItems = list(map(lambda x: self.op(int(x)) % test ,items))
                self.intParts = list(map(lambda x: self.op(int(x)) // test ,items))

                break

        self.nInspected = 0

    def throw(self,monkeys):
        for item in self.items:
            self.nInspected += 1
            val = self.op(item)
            val = int(val/3)
            monkeys[self.to(val)].items += [val]
        self.items = []

    def newThrow(self,monkeys):
        print(self.ind,self.intParts)
        for item,i in zip(self.newItems,self.intParts):
            self.nInspected += 1
            val = item
            throwTo = monkeys[self.to(val)]
            val2 = self.modWith*i+val
#           val2 = val
            throwTo.newItems += [throwTo.op(val2) % throwTo.modWith]
            throwTo.intParts += [throwTo.op(val2) // throwTo.modWith]
        self.newItems = []
        self.intParts = []

    def getString(self):
        return """
Monkey {}:
Starting items: {}
Starting newItems: {}
{}
""".format(self.ind,self.getItemsString(),self.getNewItemsString(),self.specifics)

    def getItemsString(self):
        return ','.join(map(str,self.items))

    def getNewItemsString(self):
        return ','.join(map(str,self.newItems))

monkeys = [Monkey(lines,n) for n in range(nMonkeys)]
rounds=0

for r in range(rounds):
    for m in monkeys:
        m.throw(monkeys)

#for m in monkeys:
#    print(m.getString())

insp = [m.nInspected for m in monkeys]
insp = sorted(insp)[-2:]
#print(insp[0]*insp[1])

monkeys = [Monkey(lines,n) for n in range(nMonkeys)]
rounds=5

for r in range(rounds):
#   print(r)
    for m in monkeys:
        m.newThrow(monkeys)

#for m in monkeys:
#    print(m.getString())
for m in monkeys:
    print(m.nInspected)

#print(monkeys[1].intParts)
#monkeys = [Monkey(lines,n) for n in range(nMonkeys)]
#print(monkeys[3].newItems)
#print(monkeys[3].intParts)
#monkeys[0].newThrow(monkeys)
#print(monkeys[3].newItems)
#print(monkeys[3].intParts)
