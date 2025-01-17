str(essence_presence_count)

kp_observed <- rowSums(essence_presence_count > 0)
kp_observed <- data.frame(kp = kp_observed)
rownames(kp_observed) <- rownames(essence_presence_count)

#Creating main dataset with real kp values from experiment
kp_dataset <- essence_presence_count %>%
  as.data.frame() %>%
  tibble::rownames_to_column("species") %>%
  mutate(kp = rowSums(select(., -species) > 0)) %>%
  select(species, kp) %>%
  column_to_rownames("species")


##### Null Model 1: Simple randomization (preserving link density) ####
null_model_1 <- permatfull(essence_presence_count, mtype = "count", fixedmar = "none") 
#prab means presence-absence data, fixedmar means that row and column sums are not taken into consideration when making the new matrices --> probability of 0.18 (= link density) of having a 1
null_perm_1 <- null_model_1$perm
names(null_perm_1) <- paste0("perm_", seq_along(null_perm_1))
null_perm_1 <- lapply(null_perm_1, as.data.frame)

null_perm_1 <- lapply(null_perm_1, function(df) {
  rownames(df) <- rownames(essence_presence_count)
  return(df)
})


#Making a list with all permuted datasets
kp_null_1_list <- list()

for (i in seq_along(null_perm_1)) {
  kp_null_1_list[[paste0("kp_", i)]] <- null_perm_1[[i]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("species") %>%
    mutate(kp = rowSums(select(., -species) > 0)) %>%
    select(species, kp) %>%
    column_to_rownames("species")
}
#Binding the 99 permuted datasets together
kp_null_1_all <- do.call(cbind, kp_null_1_list)

colnames(kp_null_1_all) <- paste0("kp_", seq_along(kp_null_1_all))


# Calculate mean and confidence intervals of 99 permuted datasets
kp_null_1 <- kp_null_1_all %>%
  rowwise() %>%
  summarize(
    null_1_mean = mean(c_across(everything())),
    null_1_lower_ci_ = null_1_mean - 1.96 * (sd(c_across(everything())) / sqrt(n())),
    null_1_upper_ci = null_1_mean + 1.96 * (sd(c_across(everything())) / sqrt(n()))
  ) %>%
  ungroup()
kp_null_1 <- as.data.frame(kp_null_1)
row.names(kp_null_1) <- row.names(kp_null_1_all)

#Adding null1 model to original dataset
kp_dataset <-cbind(kp_dataset, kp_null_1)




##### Null Model 2: Degree-constrained randomization####
#Exactly the same steps as Null Model 1
#There's no real need for this permutation: it makes us use count data. Wasn't used by Adrien

null_model_2 <- permatswap(essence_presence_count, mtype = "count", method = "quasiswap")
null_perm_2 <- null_model_2$perm
names(null_perm_2) <- paste0("perm_", seq_along(null_perm_2))
null_perm_2 <- lapply(null_perm_2, as.data.frame)

null_perm_2 <- lapply(null_perm_2, function(df) {
  rownames(df) <- rownames(essence_presence_count)
  return(df)
})

kp_null_2_list <- list()

for (i in seq_along(null_perm_2)) {
  kp_null_2_list[[paste0("kp_", i)]] <- null_perm_2[[i]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("species") %>%
    mutate(kp = rowSums(select(., -species) > 0)) %>%
    select(species, kp) %>%
    column_to_rownames("species")
}

kp_null_2_all <- do.call(cbind, kp_null_2_list)

colnames(kp_null_2_all) <- paste0("kp_", seq_along(kp_null_2_all))
# Calculate mean and confidence intervals
kp_null_2 <- kp_null_2_all %>%
  rowwise() %>%
  summarize(
    null_2_mean = mean(c_across(everything())),
    null_2_lower_ci_ = null_2_mean - 1.96 * (sd(c_across(everything())) / sqrt(n())),
    null_2_upper_ci = null_2_mean + 1.96 * (sd(c_across(everything())) / sqrt(n()))
  ) %>%
  ungroup()
kp_null_2 <- as.data.frame(kp_null_2)
row.names(kp_null_2) <- row.names(kp_null_2_all)

kp_dataset <-cbind(kp_dataset, kp_null_2)


#####Plotting####
str(kp_dataset)
kp_dataset <- as.data.frame(kp_dataset)
kp_dataset$species <- rownames(kp_dataset)


kp_dataset_long <- kp_dataset %>%
  pivot_longer(cols = starts_with("null_"),
               names_to = c("null_model", ".value"), 
               names_pattern = "(null_\\d+)_(.*)") 

# Create a plot
# ggplot() +
#   # Add original kp points
#   geom_point(data = kp_dataset, aes(x = rownames(kp_dataset), y = kp), color = "blue", size = 3) +
#   # Add mean points for null models
#   geom_point(data = kp_dataset_long, aes(x = species, y = mean), color = "red", size = 3, shape = 17) +
#   # Add confidence intervals for null models
#   geom_errorbar(data = kp_dataset_long, aes(x = species, ymin = lower_ci_, ymax = upper_ci), color = "red", width = 0.2) +
#   # Customize plot
#   labs(x = "Species", y = "KP Values", title = "KP Values with Null Model Means and Confidence Intervals") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_color_manual(values = c("blue" = "Original KP", "red" = "Null Model Mean"))



kp_dataset$species <- row.names(kp_dataset)


ggplot() +
  # Add original kp points
  geom_point(data = kp_dataset, aes(x = species, y = kp), color = "blue", size = 3) +
  # Add mean points for null models with colors based on null_model
  geom_point(data = kp_dataset_long, aes(x = species, y = mean, color = null_model), size = 3, shape = 17) +
  # Add confidence intervals for null models with colors based on null_model
  geom_errorbar(data = kp_dataset_long, aes(x = species, ymin = lower_ci_, ymax = upper_ci, color = null_model), width = 0.2) +
  # Customize plot
  labs(x = "Species", y = "KP Values", title = "KP Values with Null Model Means and Confidence Intervals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("null_1" = "red", "null_2" = "orange"))  # Set distinct colors for null models
