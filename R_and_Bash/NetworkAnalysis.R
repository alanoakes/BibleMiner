#source: https://kateto.net/netscix2016.html

# + ------------------------------------------------------------------------- +
# + Environment Variables                                                     +
# + ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ +
rm(list = ls()) # Remove all the objects we created so far.
library(igraph) # Load the igraph package

# + ------------------------------------------------------------------------- +
# + Make Simple Network                                                       +
# + ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ +
# Small graphs can also be generated with a description of this kind: 
#   `-` for undirected tie, +- or -+ for directed ties pointing left & right, 
#   `++` for a symmetric tie, and “:” for sets of vertices.
g1 <- graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)
plot(g1)
E(g1)                   # access edges
V(g1)                   # access vertices
g1[]                    # examine network matrix

# Add attributes to the network, vertices, or edges:
V(g1)$name              # automatically generated when we created the network.
E(g1)$type   <- "email" # Edge attribute, assign "email" to all edges
E(g1)$weight <- 10      # Edge weight, setting all existing edges to 10

# set_edge_attr(), set_vertex_attr()
g1 <- set_graph_attr(g1, "name", "Email Network")
graph_attr_names(g1); graph_attr(g1, "name")

# + ------------------------------------------------------------------------- +
# + Turning networks into igraph objects                                      +
# + ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ +
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)