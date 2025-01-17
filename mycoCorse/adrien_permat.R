adrien <- read.table("adrien.csv", header = T, stringsAsFactors = T, sep = ",")
rownames(adrien) <- adrien$tree
adrien$tree <- NULL





null_model_adri <- permatswap(adrien, mtype = "prab", method = "quasiswap", times = 99)
#prab means presence-absence data, fixedmar means that row and column sums are not taken into consideration when making the new matrices --> probability of 0.18 (= link density) of having a 1
null_perm_adri <- null_model_adri$perm
names(null_perm_adri) <- paste0("perm_", seq_along(null_perm_adri))
null_perm_adri <- lapply(null_perm_adri, as.data.frame)

null_perm_adri <- lapply(null_perm_adri, function(df) {
  rownames(df) <- rownames(adrien)
  return(df)
})

cp_null_adri_list <- list()
kf_null_adri_list <- list()
colsums_adri_list <- list()
#!! make sure essence_presence doesn't have the species variable anymore
# row.names(essence_presence) <- essence_presence$species
# essence_presence$species <- NULL


for (i in seq_along(null_perm_adri)) {
  cp_null_adri_list[[paste0("cp_", i)]] <- as.data.frame(null_perm_adri[[i]]) %>%
    mutate_all(~ if_else(. > 0, 1, 0))
  
  colsums_adri_list[[paste0("colsums_", i)]] <- colSums(cp_null_adri_list[[i]] == 1)
  kf_null_adri_list[[paste0("kf_", i)]] <- data.frame(species = colnames(adrien),
                                                 all = colsums_adri_list[[i]])
  
  
  cp_null_adri_list[[i]] <- cp_null_adri_list[[i]] %>%
    rownames_to_column(var = "species") %>%
    pivot_longer(cols = -species, names_to = "fungi", values_to = "presence") %>%
    group_by(species) %>%
    filter(presence == "1") %>%
    rename(essence = species) %>%
    rename(species = fungi) %>%
    select(-presence) %>%
    left_join(kf_null_adri_list[[i]], by = "species") %>%
    group_by(essence) %>%
    summarize(cp = mean(all))
  
}


# Extract the 'essence' variable (tree species) from the first element
essence_adri <- cp_null_adri_list[[1]]$essence

# Extract the 'presence' variable from each dataset in cp_null_1_list as a separate column
cp_data_adri <- lapply(cp_null_adri_list, function(df) df$cp)

# Combine all 'presence' columns into a data frame and add the 'essence' column
cp_null_adri_all <- as.data.frame(cbind(essence_adri = essence_adri, do.call(cbind, cp_data_adri)))






cp_null_adri_all <- cp_null_adri_all %>%
  mutate(across(-essence_adri, as.numeric))


row.names(cp_null_adri_all) <- cp_null_adri_all$essence_adri
cp_null_adri_all$essence_adri <- NULL


str(cp_null_adri_all)


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