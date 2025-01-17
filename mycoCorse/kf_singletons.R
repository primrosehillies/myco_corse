######Kf no singletons - ALL#########
essence_presence_abund <- invent %>%
  mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
  group_by(Taxon.fonge) %>%
  filter(n() > 1) %>%  
  distinct(Essence, Taxon.fonge) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0))
#on passe de 169 à 91 espèces sans les singletons
essence_presence_abund <- as.data.frame(essence_presence_abund)

row.names(essence_presence_abund) <- essence_presence_abund$Essence
essence_presence_abund$Essence <- NULL

essence_presence_abund_matrix <- as.matrix(essence_presence_abund)


colsums_abund_all <- colSums(essence_presence_abund == 1)

kf_abund_all <- data.frame(species = colnames(essence_presence_abund), all = colsums_abund_all, row.names = NULL)



kf_added_abund_all <- kf_abund_all %>%
  group_by(all) %>%
  summarize(count = n())

kf_added_abund_all$all <- as.character(kf_added_abund_all$all)

ggplot(kf_added_abund_all, aes(x = all, y = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()



######Kf no singletons - S7#########
essence_presence_abund_s7 <- invent_s7 %>%
  mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
  group_by(Taxon.fonge) %>%
  filter(n() > 1) %>% 
  distinct(Essence, Taxon.fonge) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0))
#on passe de 169 à 91 espèces sans les singletons
essence_presence_abund_s7 <- as.data.frame(essence_presence_abund_s7)

row.names(essence_presence_abund_s7) <- essence_presence_abund_s7$Essence
essence_presence_abund_s7$Essence <- NULL

essence_presence_abund_s7_matrix <- as.matrix(essence_presence_abund_s7)


colsums_abund_s7 <- colSums(essence_presence_abund_s7 == 1)

kf_abund_s7 <- data.frame(species = colnames(essence_presence_abund_s7), all = colsums_abund_s7, row.names = NULL)



kf_added_abund_s7 <- kf_abund_s7 %>%
  group_by(all) %>%
  summarize(count = n())

kf_added_abund_s7$all <- as.character(kf_added_abund_s7$all)

ggplot(kf_added_abund_s7, aes(x = all, y = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()



######Kf no singletons - S14#########
essence_presence_abund_s14 <- invent_s14 %>%
  mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
  group_by(Taxon.fonge) %>%
  filter(n() > 1) %>%  
  distinct(Essence, Taxon.fonge) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0))
#on passe de 169 à 91 espèces sans les singletons
essence_presence_abund_s14 <- as.data.frame(essence_presence_abund_s14)

row.names(essence_presence_abund_s14) <- essence_presence_abund_s14$Essence
essence_presence_abund_s14$Essence <- NULL

essence_presence_abund_s14_matrix <- as.matrix(essence_presence_abund_s14)


colsums_abund_s14 <- colSums(essence_presence_abund_s14 == 1)

kf_abund_s14 <- data.frame(species = colnames(essence_presence_abund_s14), all = colsums_abund_s14, row.names = NULL)



kf_added_abund_s14 <- kf_abund_s14 %>%
  group_by(all) %>%
  summarize(count = n())

kf_added_abund_s14$all <- as.character(kf_added_abund_s14$all)

ggplot(kf_added_abund_s14, aes(x = all, y = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()


######Kf no singletons - S15#########
essence_presence_abund_s15 <- invent_s15 %>%
  mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
  group_by(Taxon.fonge) %>%
  filter(n() > 1) %>% 
  distinct(Essence, Taxon.fonge) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = present, values_fill = list(present = 0))
#on passe de 169 à 91 espèces sans les singletons
essence_presence_abund_s15 <- as.data.frame(essence_presence_abund_s15)

row.names(essence_presence_abund_s15) <- essence_presence_abund_s15$Essence
essence_presence_abund_s15$Essence <- NULL

essence_presence_abund_s15_matrix <- as.matrix(essence_presence_abund_s15)


colsums_abund_s15 <- colSums(essence_presence_abund_s15 == 1)

kf_abund_s15 <- data.frame(species = colnames(essence_presence_abund_s15), all = colsums_abund_s15, row.names = NULL)



kf_added_abund_s15 <- kf_abund_s15 %>%
  group_by(all) %>%
  summarize(count = n())

kf_added_abund_s15$all <- as.character(kf_added_abund_s15$all)

ggplot(kf_added_abund_s15, aes(x = all, y = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()





#######Test phylum / famille####

invent_phylum <- invent %>%
  distinct(Taxon.fonge, Phylum) %>%
  rename(species = Taxon.fonge)

kf_abund_all <- kf_abund_all %>%
  left_join(invent_phylum, by = "species")



kf_added_abund_all <- kf_abund_all %>%
  group_by(all) %>%
  summarize(count = n())



kf_abund_all$all <- as.factor(kf_abund_all$all)

kf1 <- kf_abund_all %>%
  group_by(all, Phylum) %>%
  summarize(count = n()) %>%
  group_by(all) 



ggplot(kf1, aes(fill = Phylum, y = count, x = all)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("Ascomycète" = "#f8766dff", 
                               "Basidiomycète" = "#00bfc4ff", 
                               "Mucoromycète" = "#6800d1ff")) +
  theme_minimal()






#creating a dataset only with species and family names
invent_family <- invent %>%
  distinct(Taxon.fonge, Famille) %>%
  rename(species = Taxon.fonge)


#adding family names to kf dataset
kf_abund_all <- kf_abund_all %>%
  left_join(invent_family, by = "species")

kf2 <- kf_abund_all %>%
  group_by(all, Famille) %>%
  summarize(count = n()) %>%
  group_by(all) 

ggplot(kf2, aes(fill = Famille, y = count, x = all)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal()







kf_abund_all$all <- as.numeric(kf_abund_all$all)


#############"
kf_high_deg <- kf_abund_all %>%
  filter(all > 2)

kf_high_fam <- kf_high_deg %>%
  group_by(Famille) %>%
  summarize(count = n())
  

count_phylum_a <- count_phylum_a %>%
  mutate(percentage = round((count / sum(count)) * 100,2))


plot_phy_a_all <- ggplot(count_phylum_a, aes(x="", y=count, fill=Phylum)) +
  ggtitle("Tous sites confondus") +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual(values = c("Ascomycète" = "#f8766dff", 
                               "Basidiomycète" = "#00bfc4ff", 
                               "Mucoromycète" = "#6800d1ff"))
