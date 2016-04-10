########################################
#########PACKAGES#######################
########################################
library(miniCRAN)
library(igraph)
library(magrittr)
library(plyr)
library(stringr)
########################################
#########READ THE DATA##################
########################################
data <- readLines("Wiki-Vote.txt")
data <- data[-c(1:4)]
nodes <- adply(data, 1, .id = NULL, function(x){
  y <- strsplit(x, split = "\t")
  return(c(y[[1]][1], y[[1]][2]))
})

nodes_graph <- graph.data.frame(nodes, direct = FALSE)
nodes_page_rank <- page.rank(nodes_graph)
nodes_rank <- nodes_page_rank$vector

########################################
#########VISUALIZATION##################
########################################

vertex <- V(nodes_graph)
edges <- E(nodes_graph)

small_vertex <- vertex[degree(nodes_graph) < 10]
plot_network <- delete.vertices(nodes_graph, small_vertex)

par(mai=c(0,0,1,0))
plot(plot_network,
     layout=layout.fruchterman.reingold,
     main='Wiki Network',
     vertex.label.dist=0.5,
     vertex.frame.color='blue',
     vertex.label.color='black',
     vertex.label.font=2,	
     vertex.label=V(plot_network)$name,	
     vertex.label.cex = as.numeric(nodes_rank[match(V(plot_network)$name, 
                                                    names(nodes_rank))]) * 1000	
)

########################################
############CLUSTERING##################
########################################
# Extract top 80% of packages ---------------------------------------------

cutoff <- quantile(nodes_rank, probs = 0.2)
popular <- nodes_rank[as.numeric(nodes_rank) >= as.numeric(cutoff)] 
toKeep <- names(popular)

vids <- V(nodes_graph)[toKeep]
gs <- induced.subgraph(nodes_graph, vids = toKeep)


# Determine communities using walktrap algorithm --------------------------

cl <- walktrap.community(gs, steps = 3)

topClusters <- table(cl$membership) %>% sort(decreasing = TRUE) %>% head(25)

plot(topClusters, main="Cluster size", ylab="Number of members", type="b", lwd=2)

cluster <- function(i, clusters, pagerank, n=10){
  group <- clusters$names[clusters$membership == i]
  pagerank[group, ] %>% sort(decreasing = TRUE) %>% head(n)
}

# Display members of cluster "3"
# cluster(3, cl, pr) 


# Display members of top 10 clusters
z <- lapply(names(topClusters)[1:10], cluster, clusters=cl, pagerank=data.frame(nodes_rank), n=20)
z

