library(igraph)


g <- make_graph(edges = c(1,2,1,5,2,3,6,8), n = 10, directed = FALSE)
plot(g)

g <- add_vertices(g, 3)
plot(g)

g <- add_edges(g,edges = c(1, 3))
plot(g)


#using matrix:
edge_matrix <- matrix(c(1, 2,
                        1, 5,
                        2, 3,
                        6, 8), 
                      ncol = 2, byrow = TRUE)
g2 <- graph_from_edgelist(edge_matrix, directed = FALSE)
plot(g2)

#using data frame:
edge_df <- data.frame(from = c(1, 1, 2, 6),
                      to = c(2, 5, 3, 8))
vertices_df <- data.frame(name = as.character(1:8))

g3 <- graph_from_data_frame(d = edge_df, vertices = vertices_df, directed = FALSE)
plot(g3)

#using df + names:
edge_df <- data.frame(from = c("Castanea", "Quercus"),
                      to = c("Amanita", "Venturia"))

g4 <- graph_from_data_frame(d = edge_df, directed = FALSE)
plot(g4)



edge <- ess_pres_long %>%
  filter(presence == 1) %>%
  select(-presence) %>%
  rename(from = species, to = fungi)

net_1 <- graph_from_data_frame(d = edge, directed = FALSE)

##LAYOUT
# layout <- layout_with_drl(net_1)
# layout <- layout_with_fr(net_1)
# layout <- layout_with_kk(net_1) # pretty good
# layout <- layout_with_graphopt(net_1)
layout <- layout_with_fr(net_1, niter = 10000)

#COLOR
nodes <- unique(c(edge$from, edge$to))
colors <- rep(NA, length(nodes))
names(colors) <- nodes

colors[names(colors) %in% edge$from] <- "red"   # Nodes in 'from' column
colors[names(colors) %in% edge$to] <- "blue"    # Nodes in 'to' column

V(net_1)$color <- colors[as.character(V(net_1)$name)]
#SIZE
V(net_1)$size <- ifelse(V(net_1)$name %in% edge$from, 10, 2)
V(net_1)$degree <- degree(net_1)
# #GROWING SIZE
# max_degree <- max(V(net_1)$degree)
# min_degree <- min(V(net_1)$degree)
# label_size_range <- c(0.5, 50)  # Minimum and maximum label sizes
# 
# # Calculate label sizes based on degree
# V(net_1)$label_size <- rescale(V(net_1)$degree, to = label_size_range)

#NAMES
V(net_1)$label <- ifelse(V(net_1)$degree >= 5, V(net_1)$name, "")
#PLOT
plot(net_1, layout = layout,
     main = "Réseau d'interaction entre 9 essences d'arbres",
)




# g <- make_graph("Zachary")
# plot(g)
# 
# g <- add_vertices(g,3)
# g <- add_edges(g, edges = c(1, 35, 1, 36, 34, 37))
# g <- g + edges(c(1, 35, 1, 36, 34, 37))
# 
# 
# g <- g %>%
#   add_edges(edges = c(1, 34)) %>%
#   add_vertices(3) %>%
#   add_edges(edges = c(38, 39, 39, 40, 40, 38, 40, 37))
# g
