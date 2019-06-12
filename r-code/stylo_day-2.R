#USES IGRAPH AND NETWORKd3

library(stylo)
library(igraph)
library(networkD3)


setwd("E:/DHSI2019_r-stylometry")
my.corpus = stylo(gui=F,corpus.dir="28british",network=TRUE,frequencies="table_with_frequencies.txt")

edge_list = my.corpus$list.of.edges

#SET UP NETWORK
net <- graph.data.frame(edge_list,directed=T)

#calculate the network degree
deg <- degree(net, mode="all")

wc <- cluster_walktrap(net)
members <- membership(wc)

d3_net <- igraph_to_networkD3(net,group = members,size=deg)

d3_net$nodes$size=deg

network <- forceNetwork(Links = d3_net$links, 
             Nodes = d3_net$nodes,
             Source = 'source', 
             Target = 'target', 
             NodeID = 'name',
             Group = 'group',
             zoom = TRUE,
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
             legend = TRUE,
             opacityNoHover = 0.3,
             Nodesize = "size",
             radiusCalculation = "d.nodesize"
             )

saveNetwork(network = network, file = 'Net1.html')
