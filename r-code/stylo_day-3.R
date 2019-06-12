library(stylo)
library(network)
library(igraph)
library(ggplot2)
library(scales)
library(RColorBrewer) 


setwd("E:/DHSI2019_r-stylometry")
my.corpus = stylo(gui=F,corpus.dir="28british",network=TRUE,frequencies="table_with_frequencies.txt")

edge_list = my.corpus$list.of.edges

#SET UP NETWORK
net <- graph.data.frame(edge_list,directed=F)

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

#clustering
louvain <- cluster_louvain(net,weights=E(net)$Weight)
wc <- cluster_walktrap(net)
V(net)$members <- membership(louvain)

#DEFINE COLOR RAMP
c_pal <- colorRampPalette(brewer.pal(9,"Reds"))(100)
c_scale_cont <- colorRamp(c_pal)
#c_pal_discrete <- colorRampPalette(brewer.pal(max_category,"Set3"))(max_category)
c_pal_discrete <- brewer.pal(7,"Set3")
c_scale_discrete <- colorRamp(c_pal_discrete)

#NODE ATTRIBUTES
V(net)$size <- deg                    #set the size of the vertices to degree
#V(net)$color <- adjustcolor("darkred", alpha.f = .5)

for (i in 1:length(V(net)$members)) {
  V(net)$color[i]= c_pal_discrete[V(net)$members[i]] #set color based on community
#  V(net)$label.color[i]= c_pal_discrete[V(net)$members[i]] #set color based on community
}

V(net)$label.color="black"
V(net)$label.font=2

#EDGE ATTRIBUTES
E(net)$arrow.size <- .2              #set the arrow size
max_weight = as.numeric(max(E(net)$Weight))
E(net)$Weight <- as.numeric(E(net)$Weight)/max_weight # scale 0 to 1
E(net)$color = apply(c_scale_cont(E(net)$Weight), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) ) #continuous colors
E(net)$width <- as.numeric(E(net)$Weight)        #set the edge weight

#define the layout
#layout <- layout_in_circle(net, order=order(degree(net)))
#layout <- layout.fruchterman.reingold
layout <- layout.fructerman.reingold.grid
#layout <- layout.circle

#PLOT THE GRAPH using default R plotting library
plot(net,
            edge.arrow.size=.4,
            layout=layout,
            main="28 British Novels\nGrouped By Louvain Clustering",
            asp=1)

#add a legend
legend("bottom", 
       legend=levels(as.factor(V(net)$members)), 
       col = c_pal_discrete, 
       bty = "n", 
       pch=20, 
       pt.cex = 3, 
       cex = 1.5, 
       text.col="black", 
       horiz = TRUE, 
       inset = c(-0.2, -0.2)
       )

legend("left", 
       legend=levels(as.factor(deg)), 
       col = "black", 
       bty = "n", 
       pch=10, 
       pt.cex = as.numeric(levels(as.factor(deg)))/3, 
       cex = 1.5, 
       text.col="black", 
       horiz = FALSE, 
       inset = c(0, 0),
       title="Degree")