cp_dataset <- cp_dataset %>%
  rename_with(~ paste0("cp_", .), -c(cp, species, essence))

# cp_dataset <- cp_dataset %>%
#   rename(species = essence)

kp_dataset <- kp_dataset %>%
  rename_with(~ paste0("kp_", .), -c(kp, species))


cp_kp <- cp_dataset %>%
  select(-essence) %>%
  left_join(kp_dataset, by = "species")

rownames(cp_kp) <- cp_kp$species


plot(cp ~ kp, data = cp_kp)

str(cp_kp)
cp_kp_long <- cp_kp %>%
  pivot_longer(
    cols = starts_with("cp_null_") | starts_with("kp_null_"),
    names_to = c("metric", "null_model", "stat"),
    names_pattern = "(cp|kp)_(null_\\d)_(mean|lower_ci|upper_ci)"
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  ) %>%
  mutate(null_model = factor(null_model))

cp_long <- cp_kp_long %>%
  filter(metric == "cp")

cp_long_null_1 <- cp_long %>%
  filter(null_model == "null_1")

cp_long_null_2 <- cp_long %>%
  filter(null_model == "null_2")

cp_long_null_2 %>%
  mutate(kp = reorder(kp, -mean))

kp_long <- cp_kp_long %>%
  filter(metric == "kp")

kp_long_null_1 <- kp_long %>%
  filter(null_model == "null_1")

library(ggforce)

x_center <- (21.90025 + 40.55722) / 2
y_center <- (2.121362 + 2.480879) / 2
x_radius <- (40.55722 - 21.90025) / 2
y_radius <- (2.480879 - 2.121362) / 2

# Plot with the added ellipse
ggplot() +
  geom_point(data = cp_kp, aes(x = kp, y = cp), color = "bisque4", size = 3) +
  geom_point(data = cp_long_null_2, aes(x = kp, y = mean, color = null_model), size = 3, shape = 18) +
  geom_errorbar(data = cp_long_null_2, aes(x = kp, ymin = lower_ci, ymax = upper_ci, color = null_model), width = 0.2) +
  geom_ellipse(aes(x0 = x_center, y0 = y_center, a = x_radius, b = y_radius, angle = 0), 
               fill = NA, color = "black", linetype = "solid") +
  theme_minimal_grid()
