proj_all

1 - weighted_double_pres_norep$value

inv_proj <- weighted_double_pres_norep

inv_proj$value <- 1 - inv_proj$value


inv_proj <- inv_proj %>%
  left_join(df_categories, by = c("from" = "node")) %>%
  rename(from_category = category) %>%
  left_join(df_categories, by = c("to" = "node")) %>%
  rename(to_category = category)



inv_proj$color_edge <- ifelse(inv_proj$from == "Castanea sativa" | inv_proj$to == "Castanea sativa", "orangered", "lightgrey")
graph_inv <- graph_from_data_frame(inv_proj, directed = FALSE)


inv_proj_graph <- ggraph(graph_inv, layout = 'circle') +
  geom_edge_fan(aes(edge_width = value, color = color_edge), curvature = 0.1) +
  geom_node_point(aes(color = cat_essence, size = 3), show.legend = T) +
  geom_node_text(aes(label = name), vjust = 1.5) +
  scale_edge_width(range = c(0.1, 3)) +
  scale_color_manual(values = c("End of succession" = "lightblue", 
                                "Pioneer species" = "#ffca22ff", 
                                "Castanea sativa" = "chartreuse4")) +
  scale_edge_color_manual(values = c("lightgrey" = "lightgrey",
                                     "lightgreen" = "lightgreen")) +
  theme_void() +
  theme(legend.position = "none")

inv_proj_graph
