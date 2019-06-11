library(stylo)
library(network)
library(igraph)
library(ggplot2)
library(scales)

setwd("E:/DHSI2019_r-stylometry")
my.corpus = stylo(gui=F,corpus.dir="28british")

edge_list = my.corpus$list.of.edges

#SET UP NETWORK
net <- graph.data.frame(edge_list,directed=T)

#calculate the network degree
deg <- degree(net, mode="all")

#EXTRACT AUTHOR NAME
V(net)$author = V(net)$name
V(net)$author = sub("\\_.*", "", V(net)$author)

#GROUPING FUNCTION
group <- function(x) {
  last = as.character(x[1])
  cat = 1
  category = c()
  for (i in 1:length(x)) {
    this = as.character(x[i])
    if (this!=last) cat = cat + 1
    last = as.character(x[i])
    category[i]=cat
  }
  return(category)
}

V(net)$cat = as.numeric(group(V(net)$author))
max_category = max(as.numeric(V(net)$cat))


#DEFINE COLOR RAMP
c_pal <- colorRampPalette(brewer.pal(9,"Reds"))(100)
c_scale_cont <- colorRamp(c_pal)
c_pal_discrete <- colorRampPalette(brewer.pal(max_catgeory,"Paired"))(max_category)
c_scale_discrete <- colorRamp(c_pal_discrete)

#NODE ATTRIBUTES
V(net)$size <- deg                    #set the size of the vertices to degree
V(net)$label.color <- apply(c_scale_discrete(as.numeric(V(net)$cat)/max_category), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) ) #discrete colors
V(net)$color <- adjustcolor("darkred", alpha.f = .5)

#EDGE ATTRIBUTES
E(net)$arrow.size <- .2              #set the arrow size
max_weight = as.numeric(max(E(net)$Weight))
E(net)$Weight <- as.numeric(E(net)$Weight)/max_weight # scale 0 to 1
E(net)$color = apply(c_scale_cont(E(net)$Weight), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) ) #continuous colors
E(net)$width <- as.numeric(E(net)$Weight) * 3        #set the edge weight

#define the layout
#layout <- layout_in_circle(net, order=order(degree(net)))
layout <- layout.fruchterman.reingold
#layout <- layout.circle

#PLOT THE GRAPH
plot.igraph(net,
     edge.arrow.size=.4,
     layout=layout)



