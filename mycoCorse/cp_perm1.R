#####Setting up original dataset####

essence_presence <- invent %>%
  mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
  distinct(Essence, Taxon.fonge) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0))
essence_presence <- as.data.frame(essence_presence)

row.names(essence_presence) <- essence_presence$Essence
essence_presence$Essence <- NULL

colsums_all <- colSums(essence_presence == 1)
kf_all <- data.frame(species = colnames(essence_presence), all = colsums_all, row.names = NULL)


essence_presence <- essence_presence %>%
  rownames_to_column(var = "species")
# Pivot to long format 
ess_pres_long <- essence_presence %>%
  pivot_longer(cols = -species, names_to = "fungi", values_to = "presence")




#Calculating cp
cp_dataset <- ess_pres_long %>%
  group_by(species) %>%
  filter(presence == "1") %>%
  rename(essence = species) %>%
  rename(species = fungi) %>%
  select(-presence) %>%
  left_join(kf_all, by = "species") %>%
  group_by(essence) %>%
  summarize(cp = mean(all)) 


##### Null Model 1: Simple randomization (preserving link density) ####
null_model_1 <- permatfull(essence_presence_count, mtype = "count", fixedmar = "none", times = 999)
#prab means presence-absence data, fixedmar means that row and column sums are not taken into consideration when making the new matrices --> probability of 0.18 (= link density) of having a 1
null_perm_1 <- null_model_1$perm
names(null_perm_1) <- paste0("perm_", seq_along(null_perm_1))
null_perm_1 <- lapply(null_perm_1, as.data.frame)

null_perm_1 <- lapply(null_perm_1, function(df) {
  rownames(df) <- rownames(essence_presence_count)
  return(df)
})

cp_null_1_list <- list()
kf_null_list <- list()
colsums_list <- list()
#!! make sure essence_presence doesn't have the species variable anymore
# row.names(essence_presence) <- essence_presence$species
# essence_presence$species <- NULL


for (i in seq_along(null_perm_1)) {
  cp_null_1_list[[paste0("cp_", i)]] <- as.data.frame(null_perm_1[[i]]) %>%
    mutate_all(~ if_else(. > 0, 1, 0))
  
  colsums_list[[paste0("colsums_", i)]] <- colSums(cp_null_1_list[[i]] == 1)
  kf_null_list[[paste0("kf_", i)]] <- data.frame(species = colnames(essence_presence_count),
                                                 all = colsums_list[[i]])


  cp_null_1_list[[i]] <- cp_null_1_list[[i]] %>%
    rownames_to_column(var = "species") %>%
    pivot_longer(cols = -species, names_to = "fungi", values_to = "presence") %>%
    group_by(species) %>%
    filter(presence == "1") %>%
    rename(essence = species) %>%
    rename(species = fungi) %>%
    select(-presence) %>%
    left_join(kf_null_list[[i]], by = "species") %>%
    group_by(essence) %>%
    summarize(cp = mean(all))
    
}


# Extract the 'essence' variable (tree species) from the first element
essence <- cp_null_1_list[[1]]$essence

# Extract the 'presence' variable from each dataset in cp_null_1_list as a separate column
cp_data_1 <- lapply(cp_null_1_list, function(df) df$cp)

# Combine all 'presence' columns into a data frame and add the 'essence' column
cp_null_1_all <- as.data.frame(cbind(essence = essence, do.call(cbind, cp_data_1)))






cp_null_1_all <- cp_null_1_all %>%
  mutate(across(-essence, as.numeric))


row.names(cp_null_1_all) <- cp_null_1_all$essence
cp_null_1_all$essence <- NULL


str(cp_null_1_all)


cp_null_1 <- cp_null_1_all %>%
  rowwise() %>%
  summarize(
    null_1_mean = mean(c_across(everything())),
    null_1_lower_ci = null_1_mean - 1.96 * (sd(c_across(everything())) / sqrt(n())),
    null_1_upper_ci = null_1_mean + 1.96 * (sd(c_across(everything())) / sqrt(n()))
  ) %>%
  ungroup()
cp_null_1 <- as.data.frame(cp_null_1)
row.names(cp_null_1) <- row.names(cp_null_1_all)




cp_dataset <- cbind(cp_dataset, cp_null_1)



##### Null Model 2: Degree-constrained randomization ####
null_model_2 <- permatswap(essence_presence_count, mtype = "count", method = "quasiswap", times = 999)
null_perm_2 <- null_model_2$perm
names(null_perm_2) <- paste0("perm_", seq_along(null_perm_2))
null_perm_2 <- lapply(null_perm_2, as.data.frame)

null_perm_2 <- lapply(null_perm_2, function(df) {
  rownames(df) <- rownames(essence_presence_count)
  return(df)
})

cp_null_2_list <- list()
kf_null_2_list <- list()
colsums_2_list <- list()
#!! make sure essence_presence doesn't have the species variable anymore
# row.names(essence_presence) <- essence_presence$species
# essence_presence$species <- NULL


for (i in seq_along(null_perm_2)) {
  cp_null_2_list[[paste0("cp_", i)]] <- as.data.frame(null_perm_2[[i]]) %>%
    mutate_all(~ if_else(. > 0, 1, 0))
  
  colsums_2_list[[paste0("colsums_", i)]] <- colSums(cp_null_2_list[[i]] == 1)
  kf_null_2_list[[paste0("kf_", i)]] <- data.frame(species = colnames(essence_presence_count),
                                                 all = colsums_2_list[[i]])
  
  
  cp_null_2_list[[i]] <- cp_null_2_list[[i]] %>%
    rownames_to_column(var = "species") %>%
    pivot_longer(cols = -species, names_to = "fungi", values_to = "presence") %>%
    group_by(species) %>%
    filter(presence == "1") %>%
    rename(essence = species) %>%
    rename(species = fungi) %>%
    select(-presence) %>%
    left_join(kf_null_2_list[[i]], by = "species") %>%
    group_by(essence) %>%
    summarize(cp = mean(all))
  
}


# Extract the 'essence' variable (tree species) from the first element
essence <- cp_null_2_list[[1]]$essence

# Extract the 'presence' variable from each dataset in cp_null_1_list as a separate column
cp_data_2 <- lapply(cp_null_2_list, function(df) df$cp)

# Combine all 'presence' columns into a data frame and add the 'essence' column
cp_null_2_all <- as.data.frame(cbind(essence = essence, do.call(cbind, cp_data_2)))






cp_null_2_all <- cp_null_2_all %>%
  mutate(across(-essence, as.numeric))


row.names(cp_null_2_all) <- cp_null_2_all$essence
cp_null_2_all$essence <- NULL


str(cp_null_2_all)


cp_null_2 <- cp_null_2_all %>%
  rowwise() %>%
  summarize(
    null_2_mean = mean(c_across(everything())),
    null_2_lower_ci = null_2_mean - 1.96 * (sd(c_across(everything())) / sqrt(n())),
    null_2_upper_ci = null_2_mean + 1.96 * (sd(c_across(everything())) / sqrt(n()))
  ) %>%
  ungroup()
cp_null_2 <- as.data.frame(cp_null_2)
row.names(cp_null_2) <- row.names(cp_null_2_all)



cp_dataset <- cbind(cp_dataset, cp_null_2)


##### Plotting ####

cp_dataset <- as.data.frame(cp_dataset)
cp_dataset$species <- rownames(cp_dataset)


cp_dataset_long <- cp_dataset %>%
  pivot_longer(cols = starts_with("null_"), 
               names_to = c("null_model", ".value"), 
               names_pattern = "(null_\\d+)_(.*)") 


ggplot() +
  # Add original cp points
  geom_point(data = cp_dataset, aes(x = essence, y = cp), color = "blue", size = 3) +
  # Add mean points for null models with colors based on null_model
  geom_point(data = cp_dataset_long, aes(x = essence, y = mean, color = null_model), size = 3, shape = 17) +
  # Add confidence intervals for null models with colors based on null_model
  geom_errorbar(data = cp_dataset_long, aes(x = essence, ymin = lower_ci, ymax = upper_ci, color = null_model), width = 0.2) +
  # Customize plot
  labs(x = "Species", y = "CP Values", title = "CP Values with Null Model Means and Confidence Intervals") +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("null_1" = "red", "null_2" = "orange"))  # Set distinct colors for null models

