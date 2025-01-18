# Transpose the matrix so that athletes are the rows, and segments are the columns
pca_matrix_t <- t(pca_matrix)
library(coop)
cosine_similarity <- coop::cosine(t(pca_matrix))
cosine_distance <- 1 - cosine_similarity



library(psych)
library(igraph)

distancematrix <- cosine_distance

g <- graph_from_adjacency_matrix(as.matrix(distancematrix), mode = "undirected", weighted = TRUE, diag = FALSE)
mst <- minimum.spanning.tree(g)



DM1 <- as.matrix(mst)

## Zero out connections where there is low (absolute) correlation
## Keeps connection for cor ~ -1


# Esto tambien lo comento pq ns q es: source("tsClustAlgs.R")

net <- graph.adjacency(DM1, mode = "undirected", weighted = TRUE, diag = TRUE)
# get the net layout, just to plot the network nodes in the same position
net_layout = layout_components(net)
# plot the network
plot(net, layout=layout.fruchterman.reingold)