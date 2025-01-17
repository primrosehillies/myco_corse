invent_casta <- invent %>%
  filter(Essence == "Castanea sativa")

kf_all <- data.frame(species = colnames(essence_presence), all = colsums_all, row.names = NULL)
kf_all <- kf_all %>%
  left_join(invent_phylum, by = "species") %>%
  left_join(invent_family, by = "species")



casta_myc <- invent_casta %>%
  distinct(Taxon.fonge) %>%
  rename(species = Taxon.fonge)
casta_myc$species <- droplevels(casta_myc$species)

kf_casta <- kf_all %>% 
  filter(species %in% levels(casta_myc$species))

families_to_keep <- c("Russulaceae", "Thelephoraceae", "Myxotrichaceae", 
                      "Dothydeomycetes", "Helotiales", "Pyronemataceae")

kf_casta$Famille <- as.character(kf_casta$Famille) 
kf_casta$Famille[!(kf_casta$Famille %in% families_to_keep)] <- "Other"
kf_casta$Famille <- factor(kf_casta$Famille)

kf_casta <- kf_casta %>%
  filter(!Phylum == "Mucoromycète") %>%
  mutate(Famille = case_when(
    Famille == "Other" & Phylum == "Ascomycète" ~ "Other_Ascomycete",
    Famille == "Other" & Phylum == "Basidiomycète" ~ "Other_Basidiomycete",
    TRUE ~ Famille #keep other family names unchanged
  ))

kf_family_casta <- kf_casta %>%
  group_by(Famille, all) %>%
  summarize(count = n()) %>%
  group_by(all)



kf_family_casta$all <- as.character(kf_family_casta$all)

# reorder for color-gradient purposes
kf_family_casta$Famille <- factor(kf_family_casta$Famille, levels = c(
  "Pyronemataceae", "Myxotrichaceae", "Dothydeomycetes", "Helotiales", "Other_Ascomycete", # Red
  "Russulaceae", "Thelephoraceae", "Other_Basidiomycete" # Blue
))

ggplot(kf_family_casta, aes(fill = Famille, y = count, x = all)) +
  geom_bar(position = "stack", stat = "identity") +
  ggtitle("Mycorrhizae associated with Castanea sativa") +
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










# Get the row names which represent tree species
tree_species <- rownames(essence_presence_casta)

# Initialize a list to hold results
results <- list()

# Loop through each column (species) to check presence across tree species
for (species in colnames(essence_presence_casta)) {
  # Get the presence data for the current species
  presence_data <- essence_presence_casta[[species]]
  
  # Check which tree species are present for this species
  if (any(presence_data == 1)) {
    # Create combination string for present tree species
    combination <- paste(tree_species[presence_data == 1], collapse = " x ")
    results[[species]] <- data.frame(species = species, combination = combination)
  }
}

# Combine all results into a single data frame
casta_combos <- do.call(rbind, results)

# Check the resulting dataset
print(casta_combos)



kf_casta <- kf_casta %>%
  left_join(casta_combos, by = "species")

kf_casta_added <- kf_casta %>%
  group_by(combination, all) %>%
  summarize(count = n()) %>%
  group_by(all)


ggplot(kf_casta_sorted, aes(fill = combination, y = count, x = all)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal()



kf_casta_sorted <- kf_casta_added %>%
  mutate(length_combination = nchar(combination)) %>%  # Calculate the character length
  arrange(length_combination) %>%  # Sort by length
  select(-length_combination)  # Remove the temporary length column

print(kf_casta_sorted)

kf_casta_sorted <- kf_casta_sorted %>%
  mutate(combination = factor(combination, levels = combination))  # Set levels to current order

# Create the plot
ggplot(kf_casta_sorted2, aes(fill = combination, y = count, x = all)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() +
  labs(x = "All", y = "Count", fill = "Combination")  # Optional: add labels




# Assuming your data is in a data frame called 'df' and 'Combination' is your factor variable
df$CombinationCount <- sapply(strsplit(as.character(df$Combination), " x "), length)
kf_casta_sorted$combcount <- sapply(strsplit(as.character(kf_casta_sorted$combination), "x"), length)

kf_casta_sorted2 <- kf_casta_sorted[-1,]
kf_casta_sorted2$all <- as.factor(kf_casta_sorted2$all)

# Now you can use CombinationCount to group the legend
ggplot(kf_casta_sorted, aes(x = all, y = count, fill = combination)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~ all) + # Facet the plot by number of combinations
  theme(legend.position = "bottom") # Adjust legend position












kf_casta_sorted2 <- kf_casta_sorted2 %>%
  mutate(ColorGroup = paste(all, combination, sep = "_")) # Create a new group combining 'All' and 'Combination'


library(RColorBrewer)
color_palette <- brewer.pal(n = length(unique(kf_casta_sorted2$ColorGroup)), "Set3") # Generate a palette with enough distinct colors

extended_palette <- colorRampPalette(brewer.pal(12, "Set3"))(length(unique(kf_casta_sorted2$combination)))


ggplot(kf_casta_sorted2, aes(x = all, y = count, fill = combination)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(
    values = extended_palette) +
  theme_minimal() 
