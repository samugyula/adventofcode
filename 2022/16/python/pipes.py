#!/usr/bin/env python
import numpy as np
import networkx as nx
from itertools import combinations, permutations
from collections import defaultdict

lines = [l for l in open('data_full.txt')]

names = [l.split()[1] for l in lines]
flows = [int(l.split()[4][5:-1]) for l in lines]
nonzeroflows = list(map(lambda xy: xy[0],filter((lambda xy : xy[1] != 0),zip(names,flows)))) + ["AA"]

G = nx.Graph()

for l in lines:
    name = l.split()[1]
    G.add_node(name)
    conn = ' '.join(l.split()[9:]).replace(",","").split()
    for v in conn:
        G.add_edge(name,v)

G2 = nx.Graph()
for n1 in nonzeroflows:
    G2.add_node(n1)

for n1,n2 in permutations(nonzeroflows,2):
    p = nx.dijkstra_path_length(G,n1,n2) + 1 # 1 for opening the valve
    G2.add_edge(n1,n2,weight=p)


def findPaths(G,u,n,excludeSet = None):
# modified version of https://stackoverflow.com/questions/28095646/finding-all-paths-walks-of-given-length-in-a-networkx-graph
    if excludeSet == None:
        excludeSet = set([u])
    else:
        excludeSet.add(u)

    if n==0:
        excludeSet.remove(u)
        return [[(n,u)]]
    elif n<0:
        excludeSet.remove(u)
        return [[]]

    paths = [[(n,u)]+path for ne in [g for g in G.neighbors(u) if n-G[u][g]['weight'] >= 0] 
                if ne not in excludeSet for path in findPaths(G,ne,n-G[u][ne]['weight'],excludeSet)]

    if paths == []: paths = [[(n,u)]]

    excludeSet.remove(u)
    return paths

def findPaths(G,u,n,excludeSet = None):
# modified version of https://stackoverflow.com/questions/28095646/finding-all-paths-walks-of-given-length-in-a-networkx-graph
    if excludeSet == None:
        excludeSet = set([u])
    else:
        excludeSet.add(u)

    if n==0:
        excludeSet.remove(u)
        return [[(n,u)]]
    elif n<0:
        excludeSet.remove(u)
        return [[]]

    paths = [[(n,u)]+path for ne in [g for g in G.neighbors(u) if n-G[u][g]['weight'] >= 0] 
                if ne not in excludeSet for path in findPaths(G,ne,n-G[u][ne]['weight'],excludeSet)]

    if paths == []: paths = [[(n,u)]]

    excludeSet.remove(u)
    return paths

def pressure(path):
    return sum([n*flows[names.index(r)] for (n,r) in path[1:]])


p = findPaths(G2,"AA",30)
mpres = max([pressure(pp) for pp in p])
print(mpres)

p = findPaths(G2,"AA",26)
mpres = max([pressure(pp) for pp in p])
mppath = [pp for pp in p if pressure(pp) == mpres][0]
ex = [v for (t,v) in mppath[1:]]

p2 = findPaths(G2,"AA",26,excludeSet=set(ex))
mpres2 = max([pressure(pp) for pp in p2])

print(mpres+mpres2)
