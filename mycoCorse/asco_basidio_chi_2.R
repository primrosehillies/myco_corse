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
