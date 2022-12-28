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

for n,l in enumerate(lines):
    G.add_node(n)
    conn = ' '.join(l.split()[9:]).replace(",","").split()
    for v in conn:
        ind = names.index(v)
        G.add_edge(n,ind)

G2 = nx.Graph()
paths = defaultdict(dict)
taken = {}
for n1 in nonzeroflows:
    G2.add_node(n1)

for n1,n2 in permutations(nonzeroflows,2):
    paths[n1][n2] = len(nx.dijkstra_path(G,names.index(n1),names.index(n2)))
    G2.add_edge(n1,n2,weight=paths[n1][n2])



def findPaths(G,u,n):
    if n==0:
        return [[u]]
#   paths = [[u]+path for neighbor in G.neighbors(u) for path in findPaths(G,neighbor,n-G[u][neighbor]['weight']) if u not in path]
#   print(n)
    paths = [[u]+path for neighbor in [g for g in G.neighbors(u) if n-G[u][g]['weight'] >= 0] for path in findPaths(G,neighbor,n-G[u][neighbor]['weight']) if u not in path]
    if paths == []: paths = [[u]]
    return paths

def findPaths2(G,u,n,excludeSet = None):
# modified version of from https://stackoverflow.com/questions/28095646/finding-all-paths-walks-of-given-length-in-a-networkx-graph
    if excludeSet == None:
        excludeSet = set([u])
    else:
        excludeSet.add(u)

    if n==0:
        excludeSet.remove(u)
        return [[u]]
    elif n<0:
        excludeSet.remove(u)
        return [[]]

    paths = [[u]+path for ne in [g for g in G.neighbors(u) if n-G[u][g]['weight'] >= 0] 
                if ne not in excludeSet for path in findPaths2(G,ne,n-G[u][ne]['weight'],excludeSet)]

    if paths == []: paths = [[u]]

    excludeSet.remove(u)
    return paths


p = findPaths2(G2,"AA",30)
print(len(p))
#print(list(set(findPaths(G2,"AA",5))))
print()
#for i,j in paths.items():
#    print(i,j)
