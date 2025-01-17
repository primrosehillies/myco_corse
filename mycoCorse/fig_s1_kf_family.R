plot_kf_family <- ggplot(kf_family, aes(fill = Famille, y = count, x = all)) +
  geom_bar(position = "stack", stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  scale_fill_manual(values = c("Other_Ascomycete" = "#d43d31", 
                               "Other_Basidiomycete" = "#00bfc4ff",
                               "Russulaceae" = "#008b90",
                               "Thelephoraceae" = "#33e6eb",
                               "Dothydeomycetes" = "#f8766dff",
                               "Myxotrichaceae" = "#ff8c42",
                               "Helotiales" = "#ff4b3e",
                               "Pyronemataceae" = "#FFBC27"),
                    name = "Families")


plot_kf_family

