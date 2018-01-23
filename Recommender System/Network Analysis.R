install.packages("igraph")
library("igraph")

#http://kateto.net/networks-r-igraph
#https://cran.r-project.org/web/packages/igraph/igraph.pdf

#Defining the graph
g1 <- graph(edges = c("A","B", "B","C", "C","A", "B","D", 
                      "D","E", "D","G", "D","F", "E","F", 
                      "F","G"), directed = FALSE)
plot(g1) #visualization

#1.Newman-Girvan
#e <- edge.betweenness.community(g1, directed=F)
c <- cluster_edge_betweenness(g1) 
membership(c)
dendPlot(c, mode="hclust")
plot(c,g1)

#2.Label propagation
p <- cluster_label_prop(g1)
plot(p,g1)

#3.Fast greedy
g <- cluster_fast_greedy(g1)
plot(g,g1)
