#####Chi2 Phylums sites####
ratio_s7 <- invent_s7 %>%
  group_by(Phylum) %>%
  summarize(s7 = n()) %>%
  filter(!Phylum == "Mucoromycète")
ratio_s14 <- invent_s14 %>%
  group_by(Phylum) %>%
  summarize(s14 = n()) %>%
  filter(!Phylum == "Mucoromycète")
ratio_s15 <- invent_s15 %>%
  group_by(Phylum) %>%
  summarize(s15 = n()) %>%
  filter(!Phylum == "Mucoromycète")

ratio_sites <- ratio_s7 %>%
  left_join(ratio_s14, by = "Phylum") %>%
  left_join(ratio_s15, by = "Phylum")

ratio_sites <- as.data.frame(ratio_sites)
row.names(ratio_sites) <- ratio_sites$Phylum
ratio_sites$Phylum <- NULL

chi_site_1 <- ratio_sites %>%
  select(s7,s14) %>%
  chisq.test()
chi_site_1$expected
chi_site_1

chi_site_2 <- ratio_sites %>%
  select(s7,s15) %>%
  chisq.test()
chi_site_2$expected
chi_site_2

chi_site_3 <- ratio_sites %>%
  select(s14,s15) %>%
  chisq.test()
chi_site_3$expected
chi_site_3

#####Chi2 Phylums species####
invent_casta
ratio_casta <- invent_casta %>%
  group_by(Phylum) %>%
  summarize(castanea = n())

ratio_qu_il <- invent %>%
  filter(Essence == "Quercus ilex") %>%
  group_by(Phylum) %>%
  summarize(qu_il = n()) %>%
  filter(!Phylum == "Mucoromycète")

ratio_qu_pu <- invent %>%
  filter(Essence == "Quercus pubescens") %>%
  group_by(Phylum) %>%
  summarize(qu_pu = n()) %>%
  filter(!Phylum == "Mucoromycète")

ratio_pi_sp <- invent %>%
  filter(Essence == "Pinus spp.") %>%
  group_by(Phylum) %>%
  summarize(pi_sp = n()) %>%
  filter(!Phylum == "Mucoromycète")

ratio_ar_un <- invent %>%
  filter(Essence == "Arbutus unedo") %>%
  group_by(Phylum) %>%
  summarize(ar_un = n()) %>%
  filter(!Phylum == "Mucoromycète")


ratio <- ratio_casta %>%
  left_join(ratio_qu_il, by = "Phylum") %>%
  left_join(ratio_qu_pu, by = "Phylum") %>%
  left_join(ratio_ar_un, by = "Phylum") %>%
  left_join(ratio_pi_sp, by = "Phylum")

ratio <- as.data.frame(ratio)
row.names(ratio) <- ratio$Phylum
ratio$Phylum <- NULL


chi_ratio_1 <- ratio %>%
  select(castanea,qu_il) %>%
  chisq.test

chi_ratio_1$expected
chi_ratio_1



²chi_ratio_2 <- ratio %>%
  select(castanea,qu_pu) %>%
  chisq.test

chi_ratio_2$expected
chi_ratio_2


chi_ratio_3 <- ratio %>%
  select(castanea,ar_un) %>%
  chisq.test

chi_ratio_3$expected
chi_ratio_3

chi_ratio_4 <- ratio %>%
  select(castanea,pi_sp) %>%
  chisq.test

chi_ratio_4$expected
chi_ratio_4

chi_ratio_4$p.value



chi2_table <- read.csv(file = "chi2_ratio.csv", header = T, stringsAsFactors = T)


row.names(chi2_table) <- chi2_table$Castanea
chi2_table$Castanea <- NULL


chi2_tab <- kable(chi2_table, col.names = c("Quercus ilex", "Quercus pubescens", "Arbutus unedo", "Pinus sp."), format = "html", table.attr = "class='table table-striped'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

chi2_tab



#####Chi2 Families sites####
chi2_cast <- as.data.frame(weighted_double_pres[-c(3),])
chi2_cast$expected <- 1/8

obs <- chi2_cast$`Castanea sativa`
exp <- chi2_cast$expected

chisq.test(x=obs, p=exp)






count_family_chi_s7 <- invent_s7 %>%
  group_by(Famille) %>%
  summarize(s7_count =n())

count_family_chi_s14 <- invent_s14 %>%
  group_by(Famille) %>%
  summarize(s14_count =n())

count_family_chi_s15 <- invent_s15 %>%
  group_by(Famille) %>%
  summarize(s15_count =n())


count_family_chi_all <- count_family_chi_s7 %>%
  full_join(count_family_chi_s14, by = "Famille") %>%
  full_join(count_family_chi_s15, by = "Famille")

count_family_chi_all[is.na(count_family_chi_all)] <- 0

count_family_chi_all <- as.data.frame(count_family_chi_all)
row.names(count_family_chi_all) <- count_family_chi_all$Famille
count_family_chi_all$Famille <- NULL

chi_fam <- chisq.test(count_family_chi_all)
chi_fam$expected
chi_fam


count_family_chi_all_2 <- count_family_chi_all[!(count_family_chi_all$s7_count <= 1 & count_family_chi_all$s14_count <=1 & count_family_chi_all$s15_count <= 1),]


chi_fam_2 <- chisq.test(count_family_chi_all_2)
chi_fam_2$expected
chi_fam_2








count_family_chi_all_3 <- count_family_chi_all_2 %>%
  filter(s7_count != 0, s14_count != 0, s15_count != 0) %>%
  filter(s7_count != 1, s14_count !=1, s15_count != 1)

chi_fam_3 <- chisq.test(count_family_chi_all_3)
chi_fam_3$expected
chi_fam_3



count_family_chi_all_4 <- count_family_chi_all_3[rownames(count_family_chi_all_3) != "Inocybaceae", ]
count_family_chi_all_4 <- count_family_chi_all_4[rownames(count_family_chi_all_4) != "Amanitaceae", ]
count_family_chi_all_4 <- count_family_chi_all_4[rownames(count_family_chi_all_4) != "Tylosporaceae", ]

chi_fam_4 <- chisq.test(count_family_chi_all_4)
chi_fam_4$expected
chi_fam_4



chi_7_14 <- count_family_chi_all_4 %>%
  select(s7_count,s14_count)

chi_fam_5 <- chisq.test(chi_7_14)
chi_fam_5$expected
chi_fam_5


chi_7_15 <- count_family_chi_all_4 %>%
  select(s7_count,s15_count)

chi_fam_6 <- chisq.test(chi_7_15)
chi_fam_6$expected
chi_fam_6


chi_14_15 <- count_family_chi_all_4 %>%
  select(s14_count,s15_count)

chi_fam_7 <- chisq.test(chi_14_15)
chi_fam_7$expected
chi_fam_7
