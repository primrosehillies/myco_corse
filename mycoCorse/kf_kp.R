#Pinus
myco_pinus <- ess_pres_long %>%
  filter(species == "Pinus spp.") %>%
  filter(presence == "1") %>%
  select(fungi) %>%
  rename(species = fungi) %>%
  left_join(kf_all, by = "species")

mean_pinus <- mean(myco_pinus$all)



myco_all <- ess_pres_long %>%
  group_by(species) %>%
  filter(presence == "1") %>%
  rename(essence = species) %>%
  rename(species = fungi) %>%
  select(-presence) %>%
  left_join(kf_all, by = "species")

#Castanea
myco_casta <- ess_pres_long %>%
  filter(species == "Castanea sativa") %>%
  filter(presence == "1") %>%
  select(fungi) %>%
  rename(species = fungi) %>%
  left_join(kf_all, by = "species")

mean_casta <- mean(myco_casta$all)


#Fagus
myco_fagus <- ess_pres_long %>%
  filter(species == "Fagus sylvatica") %>%
  filter(presence == "1") %>%
  select(fungi) %>%
  rename(species = fungi) %>%
  left_join(kf_all, by = "species")

mean_fagus <- mean(myco_fagus$all)

#Alnus
myco_alnus <- ess_pres_long %>%
  filter(species == "Alnus cordata") %>%
  filter(presence == "1") %>%
  select(fungi) %>%
  rename(species = fungi) %>%
  left_join(kf_all, by = "species")

mean_alnus <- mean(myco_alnus$all)

#Arbutus
myco_arbutus <- ess_pres_long %>%
  filter(species == "Arbutus unedo") %>%
  filter(presence == "1") %>%
  select(fungi) %>%
  rename(species = fungi) %>%
  left_join(kf_all, by = "species")

mean_arbutus <- mean(myco_arbutus$all)

#Qu pu
myco_qupu <- ess_pres_long %>%
  filter(species == "Quercus pubescens") %>%
  filter(presence == "1") %>%
  select(fungi) %>%
  rename(species = fungi) %>%
  left_join(kf_all, by = "species")

mean_qupu <- mean(myco_qupu$all)

#Qu il
myco_quil <- ess_pres_long %>%
  filter(species == "Quercus ilex") %>%
  filter(presence == "1") %>%
  select(fungi) %>%
  rename(species = fungi) %>%
  left_join(kf_all, by = "species")

mean_quil <- mean(myco_quil$all)

#Cistus
myco_cistus <- ess_pres_long %>%
  filter(species == "Cistus salviifolius") %>%
  filter(presence == "1") %>%
  select(fungi) %>%
  rename(species = fungi) %>%
  left_join(kf_all, by = "species")

mean_cistus <- mean(myco_cistus$all)
#Ostrya
myco_ostrya <- ess_pres_long %>%
  filter(species == "Ostrya carpinifolia") %>%
  filter(presence == "1") %>%
  select(fungi) %>%
  rename(species = fungi) %>%
  left_join(kf_all, by = "species")

mean_ostrya <- mean(myco_ostrya$all)





mean_pinus
mean_casta
mean_fagus
mean_alnus
mean_arbutus
mean_qupu
mean_quil
mean_cistus
mean_ostrya



kf_kp <- read.csv(file = "kp_all.csv", header = T, stringsAsFactors = T)

plot(kf_kp$all, kf_kp$mean_kf)
mod1 <- lm(mean_kf ~ all, data = kf_kp)
anova(mod1)
abline(mod1)
