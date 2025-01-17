library(igraph)
library(ggraph)
library(tidyverse)

# Assume your 9x9 matrix is named df
df <- data.frame(
  A = c(0, 0.2, 0.5, 0.8, 0.1, 0.3, 0.4, 0.6, 0.7),
  B = c(0.2, 0, 0.3, 0.7, 0.5, 0.1, 0.8, 0.4, 0.6),
  C = c(0.5, 0.3, 0, 0.2, 0.6, 0.8, 0.1, 0.7, 0.4),
  D = c(0.8, 0.7, 0.2, 0, 0.4, 0.6, 0.5, 0.1, 0.3),
  E = c(0.1, 0.5, 0.6, 0.4, 0, 0.7, 0.2, 0.3, 0.8),
  F = c(0.3, 0.1, 0.8, 0.6, 0.7, 0, 0.5, 0.2, 0.4),
  G = c(0.4, 0.8, 0.1, 0.5, 0.2, 0.5, 0, 0.6, 0.3),
  H = c(0.6, 0.4, 0.7, 0.1, 0.3, 0.2, 0.6, 0, 0.5),
  I = c(0.7, 0.6, 0.4, 0.3, 0.8, 0.4, 0.3, 0.5, 0)
)

# Convert data frame into long format
df_long <- df %>%
  rownames_to_column(var = "from") %>%
  pivot_longer(cols = -from, names_to = "to", values_to = "value")

# Remove rows with value 0 or self-loops (if needed)
df_long <- df_long %>%
  filter(value > 0, from != to)

# Create the graph object
graph <- graph_from_data_frame(df_long, directed = FALSE)

# Plot using ggraph with circular layout
ggraph(graph, layout = 'circle') +
  geom_edge_arc(aes(edge_width = value), color = "blue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), vjust = 1.5) +
  scale_edge_width(range = c(0.5, 3)) +  # Adjust the range for line thickness
  theme_void() +
  theme(legend.position = "none")



#cat_essence <- c("End of succession", "Pioneer species", "Castanea sativa", "Pioneer species", "End of succession", "Pioneer species", "Pioneer species", "End of succession", "End of succession")
cat_essence <- c("Pioneer species", "Castanea sativa", "End of succession", "End of succession", "Pioneer species", "End of succession", "End of succession", "Pioneer species", "Pioneer species")
names(cat_essence) <- rownames(weighted_double_pres)
df_categories <- data.frame(node = rownames(weighted_double_pres), category = cat_essence)

# double_long <- double_presence_pond %>%
#   rownames_to_column(var = "from") %>%
#   pivot_longer(cols = -from, names_to = "to", values_to = "value") %>%
#   filter(from != to, value > 0)

weighted_double_pres_long <- as.data.frame(weighted_double_pres) %>%
  rownames_to_column(var = "from") %>%
  pivot_longer(cols = -from, names_to = "to", values_to = "value") %>%
  filter(from != to)


# double_long <- double_long %>%
#   left_join(df_categories, by = c("from" = "node")) %>%
#   rename(from_category = category) %>%
#   left_join(df_categories, by = c("to" = "node")) %>%
#   rename(to_category = category)

graph_pond_all <- graph_from_data_frame(weighted_double_pres_norep, directed = FALSE)
node_categories <- df_categories




ggraph(graph_pond_all, layout = 'circle') +
  geom_edge_arc(aes(edge_width = value)) +  
  geom_node_point(aes(color = cat_essence, size = 3), show.legend = T) +
  geom_node_text(aes(label = name), vjust = 1.5) +
  scale_edge_width(range = c(0.1, 3)) +
  scale_color_manual(values = c("End of succession" = "orange", "Pioneer species" = "lightblue", "Castanea sativa" = "lightgreen")) +
  theme_void() +
  theme(legend.position = "right")



ggraph(graph_pond_all, layout = 'circle') +
  geom_edge_arc(aes(edge_width = value, color = from_category)) +  
  geom_node_point(aes(color = cat_essence, size = 3), show.legend = TRUE) +
  geom_node_text(aes(label = name), vjust = 1.5) +
  scale_edge_width(range = c(0.1, 3)) +
  scale_edge_color_manual(values = c("End of succession" = "orange", 
                                     "Pioneer species" = "lightblue", 
                                     "Castanea sativa" = "lightgreen")) +
  scale_color_manual(values = c("End of succession" = "orange", 
                                "Pioneer species" = "lightblue", 
                                "Castanea sativa" = "lightgreen")) +
  theme_void() +
  theme(legend.position = "right")






































#WEIGHTING DOUBLE PRESENCE
# Sampling effort
sampling_effort <- c(84,206,43,20,73,69,66,22,13)
# Calculate weights
weights <- outer(sampling_effort, sampling_effort, function(x, y) 1 / sqrt(x * y))
# Apply weights to the data frame
weighted_double_pres <- double_presence * weights
# Resulting weighted data
weighted_double_pres
weighted_double_pres_norep <- as.data.frame(weighted_double_pres)

#ENLEVER LES DOUBLONS
weighted_double_pres_norep <- weighted_double_pres_norep %>%
  rownames_to_column(var = "from") %>%
  pivot_longer(-from, names_to = "to", values_to = "value") %>%
  filter(from != to)  

weighted_double_pres_norep <- weighted_double_pres_norep %>%
  mutate(pair = pmap_chr(list(from, to), ~ paste(sort(c(..1, ..2)), collapse = "-"))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)  



proj_all <- weighted_double_pres_norep

proj_all <- proj_all %>%
  left_join(df_categories, by = c("from" = "node")) %>%
  rename(from_category = category) %>%
  left_join(df_categories, by = c("to" = "node")) %>%
  rename(to_category = category)

proj_all$color_edge <- ifelse(proj_all$from == "Castanea sativa" | proj_all$to == "Castanea sativa", "lightgreen", "lightgrey")




################################################
graph_pond_all <- graph_from_data_frame(proj_all, directed = FALSE)
ggraph(graph_pond_all, layout = 'circle') +
  geom_edge_arc(aes(edge_width = value, color = color_edge), curvature = 0.1) +
  geom_node_point(aes(color = cat_essence, size = 3), show.legend = T) +
  geom_node_text(aes(label = name), vjust = 1.5) +
  scale_edge_width(range = c(0.1, 3)) +
  scale_color_manual(values = c("End of succession" = "lightblue", 
                                "Pioneer species" = "orange", 
                                "Castanea sativa" = "chartreuse4")) +
  scale_edge_color_manual(values = c("lightgrey" = "lightgrey",
                                     "lightgreen" = "lightgreen")) +
  theme_void() +
  theme(legend.position = "none")
