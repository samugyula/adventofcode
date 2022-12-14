#!/usr/bin/env python
import string
import numpy as np
from scipy.linalg.blas import sgemm
from time import perf_counter as pc
import networkx as nx

lines = [l[:-1] for l in open("data.txt")]

nRow = len(lines)
nCol = len(lines[0])

trf = np.zeros((nRow, nCol), dtype=np.uint8)

lc = string.ascii_lowercase

def getInd(c):
    for n, l in zip(range(len(lc)), lc):
        if l == c:
            return n

start = 0
end = 0

for i in range(nRow):
    for j in range(nCol):
        val = lines[i][j]
        if val == "E":
            end = i * nCol + j
            trf[i, j] = getInd("z")
        elif val == "S":
            start = i * nCol + j
            trf[i, j] = getInd("a")
        else:
            trf[i, j] = getInd(val)

G = nx.DiGraph()

def isConnected(x, y, x1, y1, g):
    return (g[x1, y1] + 1) >= g[x, y]

for i1 in range(nRow):
    for j1 in range(nCol):
        ind1 = i1 * nCol + j1
        G.add_node(ind1)
        for i2 in range(i1 - 1, i1 + 2):
            if i2 < 0 or i2 >= nRow:
                continue
            for j2 in range(j1 - 1, j1 + 2):
                if j2 < 0 or j2 >= nCol:
                    continue
                if i1 == i2 and j1 == j2:
                    continue
                if i1 != i2 and j1 != j2:
                    continue
                ind2 = i2 * nCol + j2
                if isConnected(i1, j1, i2, j2, trf):
                    G.add_edge(ind1, ind2, weight=1)

print(nx.dijkstra_path_length(G, end, start))
paths = nx.single_source_dijkstra_path_length(G, end)
apaths = [l for (n, l) in paths.items() if trf[n // nCol, n % nCol] == 0]
print(min(apaths))
