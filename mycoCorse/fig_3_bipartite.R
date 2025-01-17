####Tous sites confondus####
edge <- ess_pres_long %>%
  filter(presence == 1) %>%
  select(-presence) %>%
  rename(from = species, to = fungi)

invent_unique <- invent %>%
  select(Taxon.fonge, Phylum) %>%
  distinct() %>%
  group_by(Taxon.fonge) %>%
  slice(1)

edge <- edge %>%
  left_join(invent_unique %>% select(Taxon.fonge, Phylum), by = c("to" = "Taxon.fonge"))


to_phylum <- edge %>% select(to, Phylum) %>% distinct()

for (i in 1:nrow(to_phylum)) {
  node <- to_phylum$to[i]
  phylum <- to_phylum$Phylum[i]

  if (phylum == "Ascomycète") {
    colors[node] <- "#f8766dff"
  } else if (phylum == "Basidiomycète") {
    colors[node] <- "#00bfc4ff"
  } else if (phylum == "Mucoromycète") {
    colors[node] <- "#6800d1ff"
  }
}


from_categories <- data.frame(
  species = unique(edge$from),  # Assuming you know these species
  Category = c("Pioneer species", "Castanea sativa", "End of succession",  # Define categories manually
               "End of succession", "Pioneer species", "End of succession",
               "End of succession", "Pioneer species", "Pioneer species")
)

edge <- edge %>%
  left_join(from_categories, by = c("from" = "species"))

from_category <- edge %>% select(from, Category) %>% distinct()

for (i in 1:nrow(from_category)) {
  node <- from_category$from[i]
  category <- from_category$Category[i]

  if (category == "Pioneer species") {
    colors[node] <- "#ffca22ff"  # Example color for Category 1
  } else if (category == "End of succession") {
    colors[node] <- "lightblue"  # Example color for Category 2
  } else if (category == "Castanea sativa") {
    colors[node] <- "chartreuse4"  # Example color for Category 3
  }
}




net_1 <- graph_from_data_frame(d = edge, directed = FALSE)
#LAYOUT
#layout <- layout_with_fr(net_1, niter = 10000)
layout <- layout_nicely(net_1)
#COLOR
nodes <- unique(c(edge$from, edge$to))
V(net_1)$color <- colors[as.character(V(net_1)$name)]
#SIZE
V(net_1)$size <- ifelse(V(net_1)$name %in% edge$from, 10, 2)
V(net_1)$degree <- degree(net_1)
#NUMBERS AS NAMES
from_vertices <- unique(edge$from)
vertex_labels <- setNames(as.character(1:length(from_vertices)), from_vertices)
V(net_1)$label <- ifelse(V(net_1)$name %in% from_vertices, vertex_labels[V(net_1)$name], "")








plot(net_1, layout = layout)
