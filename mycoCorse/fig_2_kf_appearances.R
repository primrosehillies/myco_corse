#Kf: degrees of mycorrhiza species, according to appearances and to families / phylums
#####All species####

essence_presence_list <- list()
for (i in 1:10) {
  essence_presence_list[[paste0("essence_presence_", i)]] <- invent %>%
    mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
    group_by(Taxon.fonge) %>%
    filter(n() == i) %>%
    distinct(Essence, Taxon.fonge) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0)) %>%
    as.data.frame()
  
  row.names(essence_presence_list[[paste0("essence_presence_", i)]]) <- essence_presence_list[[paste0("essence_presence_", i)]]$Essence
  essence_presence_list[[paste0("essence_presence_", i)]]$Essence <- NULL
}

essence_presence_list[["essence_presence_11"]] <- invent %>%
  mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
  group_by(Taxon.fonge) %>%
  filter(n() >= 11) %>%
  distinct(Essence, Taxon.fonge) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0)) %>%
  as.data.frame()

row.names(essence_presence_list[["essence_presence_11"]]) <- essence_presence_list[["essence_presence_11"]]$Essence
essence_presence_list[["essence_presence_11"]]$Essence <- NULL


kf_list <- list()
for (i in 1:11) {
  # Get the essence_presence dataset
  essence_presence_data <- essence_presence_list[[paste0("essence_presence_", i)]]
  
  # Calculate colSums for the dataset
  colsums <- colSums(essence_presence_data == 1)
  
  # Create a data frame with the results
  kf_list[[paste0("kf_", i)]] <- data.frame(species = colnames(essence_presence_data), all = colsums, row.names = NULL)
}


kf_added_list <- list()
for (i in 1:11) {
  kf <- kf_list[[paste0("kf_", i)]]
    
  kf_added_list[[paste0("kf_added_", i)]] <- kf %>%
    group_by(all) %>%
    summarize(count = n())
  
  
}


kf_combined <- list()

# Loop through the 11 datasets
for (i in 1:11) {
  kf_added <- kf_added_list[[paste0("kf_added_", i)]]
  
  #add number of appearances
  kf_added$appearances <- i
  
  #put in list
  kf_combined[[i]] <- kf_added
}

#bind datasets 
final_kf_combined <- bind_rows(kf_combined)
final_kf_combined$all <- as.character(final_kf_combined$all)


#####Basidio####
invent_basidio <- invent %>%
  filter(Phylum == "Basidiomycète")

essence_presence_basidio_list <- list()

for (i in 1:10) {
  essence_presence_basidio_list[[paste0("essence_presence_", i)]] <- invent_basidio %>%
    mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
    group_by(Taxon.fonge) %>%
    filter(n() == i) %>%
    distinct(Essence, Taxon.fonge) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0)) %>%
    as.data.frame()
  
  row.names(essence_presence_basidio_list[[paste0("essence_presence_", i)]]) <- essence_presence_basidio_list[[paste0("essence_presence_", i)]]$Essence
  essence_presence_basidio_list[[paste0("essence_presence_", i)]]$Essence <- NULL
}

essence_presence_basidio_list[["essence_presence_11"]] <- invent_basidio %>%
  mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
  group_by(Taxon.fonge) %>%
  filter(n() >= 11) %>%
  distinct(Essence, Taxon.fonge) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0)) %>%
  as.data.frame()

row.names(essence_presence_basidio_list[["essence_presence_11"]]) <- essence_presence_basidio_list[["essence_presence_11"]]$Essence
essence_presence_basidio_list[["essence_presence_11"]]$Essence <- NULL




kf_basidio_list <- list()
for (i in 1:11) {
  # Get the essence_presence dataset
  essence_presence_data <- essence_presence_basidio_list[[paste0("essence_presence_", i)]]
  
  # Calculate colSums for the dataset
  colsums <- colSums(essence_presence_data == 1)
  
  # Create a data frame with the results
  kf_basidio_list[[paste0("kf_", i)]] <- data.frame(species = colnames(essence_presence_data), all = colsums, row.names = NULL)
}






kf_basidio_added_list <- list()
for (i in 1:11) {
  kf <- kf_basidio_list[[paste0("kf_", i)]]
  
  kf_basidio_added_list[[paste0("kf_added_", i)]] <- kf %>%
    group_by(all) %>%
    summarize(count = n())
  
  
}


kf_basidio_combined <- list()

# Loop through the 11 datasets
for (i in 1:11) {
  kf_added <- kf_basidio_added_list[[paste0("kf_added_", i)]]
  
  #add number of appearances
  kf_added$appearances <- i
  
  #put in list
  kf_basidio_combined[[i]] <- kf_added
}

#bind datasets 
final_kf_basidio_combined <- bind_rows(kf_basidio_combined)

final_kf_basidio_combined$all <- as.character(final_kf_basidio_combined$all)





#####Asco####
invent_asco <- invent %>%
  filter(Phylum == "Ascomycète")

essence_presence_asco_list <- list()

for (i in 1:10) {
  essence_presence_asco_list[[paste0("essence_presence_", i)]] <- invent_asco %>%
    mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
    group_by(Taxon.fonge) %>%
    filter(n() == i) %>%
    distinct(Essence, Taxon.fonge) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0)) %>%
    as.data.frame()
  
  row.names(essence_presence_asco_list[[paste0("essence_presence_", i)]]) <- essence_presence_asco_list[[paste0("essence_presence_", i)]]$Essence
  essence_presence_asco_list[[paste0("essence_presence_", i)]]$Essence <- NULL
}

essence_presence_asco_list[["essence_presence_11"]] <- invent_asco %>%
  mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
  group_by(Taxon.fonge) %>%
  filter(n() >= 11) %>%
  distinct(Essence, Taxon.fonge) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0)) %>%
  as.data.frame()

row.names(essence_presence_asco_list[["essence_presence_11"]]) <- essence_presence_asco_list[["essence_presence_11"]]$Essence
essence_presence_asco_list[["essence_presence_11"]]$Essence <- NULL




kf_asco_list <- list()
for (i in 1:11) {
  # Get the essence_presence dataset
  essence_presence_data <- essence_presence_asco_list[[paste0("essence_presence_", i)]]
  
  # Calculate colSums for the dataset
  colsums <- colSums(essence_presence_data == 1)
  
  # Create a data frame with the results
  kf_asco_list[[paste0("kf_", i)]] <- data.frame(species = colnames(essence_presence_data), all = colsums, row.names = NULL)
}






kf_asco_added_list <- list()
for (i in 1:11) {
  kf <- kf_asco_list[[paste0("kf_", i)]]
  
  kf_asco_added_list[[paste0("kf_added_", i)]] <- kf %>%
    group_by(all) %>%
    summarize(count = n())
  
  
}


kf_asco_combined <- list()

# Loop through the 11 datasets
for (i in 1:11) {
  kf_added <- kf_asco_added_list[[paste0("kf_added_", i)]]
  
  #add number of appearances
  kf_added$appearances <- i
  
  #put in list
  kf_asco_combined[[i]] <- kf_added
}

#bind datasets 
final_kf_asco_combined <- bind_rows(kf_asco_combined)

final_kf_asco_combined$all <- as.character(final_kf_asco_combined$all)



#####Family####
kf_all <- data.frame(species = colnames(essence_presence), all = colsums_all, row.names = NULL)

kf_all <- kf_all %>%
  left_join(invent_phylum, by = "species") %>%
  left_join(invent_family, by = "species")


# List of families to keep
families_to_keep <- c("Russulaceae", "Thelephoraceae", "Myxotrichaceae", 
                      "Dothydeomycetes", "Helotiales", "Pyronemataceae")
kf_all$Famille <- as.character(kf_all$Famille) 
kf_all$Famille[!(kf_all$Famille %in% families_to_keep)] <- "Other"
kf_all$Famille <- factor(kf_all$Famille)



kf_all <- kf_all %>%
  filter(!Phylum == "Mucoromycète") %>%
  mutate(Famille = case_when(
    Famille == "Other" & Phylum == "Ascomycète" ~ "Other_Ascomycete",
    Famille == "Other" & Phylum == "Basidiomycète" ~ "Other_Basidiomycete",
    TRUE ~ Famille #keep other family names unchanged
  ))

kf_family <- kf_all %>%
  group_by(Famille, all) %>%
  summarize(count = n()) %>%
  group_by(all)

#####Plotting####
plot_kf_all <- ggplot(final_kf_combined, aes(fill = factor(appearances, levels = 1:11), y = count, x = all)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis_d(direction = -1) +  
  ggtitle("All species") +
  theme_minimal() +
  scale_fill_viridis_d(name = "Number of appearances", direction = -1) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


plot_kf_basidio <- ggplot(final_kf_basidio_combined, aes(fill = factor(appearances, levels = 1:11), y = count, x = all)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Basidiomycetes") +
  theme_minimal() +
  scale_fill_viridis_d(name = "Number of appearances", direction = -1) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")
  

plot_kf_asco <- ggplot(final_kf_asco_combined, aes(fill = factor(appearances, levels = 1:11), y = count, x = all)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Ascomycetes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none") +
  labs(x = NULL, y = NULL) +
  scale_fill_viridis_d(name = "Number of appearances", direction = -1)


# reorder for color-gradient purposes
kf_family$Famille <- factor(kf_family$Famille, levels = c(
  "Pyronemataceae", "Myxotrichaceae", "Dothydeomycetes", "Helotiales", "Other_Ascomycete", # Red
  "Russulaceae", "Thelephoraceae", "Other_Basidiomycete" # Blue
))

kf_family$all <- as.character(kf_family$all)
plot_kf_family <- ggplot(kf_family, aes(fill = Famille, y = count, x = all)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("All species: Families") +
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




figure_kf <- ggarrange(
  plot_kf_all, 
  ggarrange(plot_kf_basidio, plot_kf_asco, ncol = 1, nrow = 2), 
  ncol = 2, 
  widths = c(2, 1) # Adjust widths for left and right panels
)

figure_kf

fig_s1 <- plot_kf_family

plot_kf_family

