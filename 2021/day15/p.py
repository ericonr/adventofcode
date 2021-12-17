import networkx as nx

vector = []
with open("test6.txt", "r") as f:
    for line in f:
        vector.append([int(c) for c in line if c != '\n'])

l = len(vector)
g = nx.DiGraph()

def gindex(x, y):
    return (x,y)

for x in range(l):
    for y in range(l):
        if x > 0:
            g.add_edge(gindex(x,y), gindex(x-1,y), weight=vector[x-1][y])
        if y > 0:
            g.add_edge(gindex(x,y), gindex(x,y-1), weight=vector[x][y-1])
        if x < l - 1:
            g.add_edge(gindex(x,y), gindex(x+1, y), weight=vector[x+1][y])
        if y < l - 1:
            g.add_edge(gindex(x,y), gindex(x, y+1), weight=vector[x][y+1])

print(g)
print(nx.shortest_path_length(g, gindex(0,0), gindex(l-1,l-1), weight="weight"))
