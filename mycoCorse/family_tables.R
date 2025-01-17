tab_all <- invent %>%
  group_by(Famille) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

tab_all %>%
  select(count) %>%
  summarize(total = sum(count))
#596

tab_all_2 <- tab_all

tab_all_2$perc <- tab_all_2$count / 596 * 100

tab_all_2 <- tab_all_2 %>%
  mutate(Famille = as.character(Famille)) %>%
  arrange(desc(perc)) %>%
  mutate (Famille = ifelse(perc < 5, "Other", Famille)) %>%
  group_by(Famille) %>%
  summarize(perc = sum(perc)) %>%
  arrange(desc(perc))

tab_all_2 <- tab_all_2 %>%
  rename(perc_all = perc)

######################
tab_s7 <- invent_s7%>%
  group_by(Famille) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

tab_s7 %>%
  select(count) %>%
  summarize(total = sum(count))
#163

tab_s7_2 <- tab_s7

tab_s7_2$perc <- tab_s7_2$count_s7 / 163 * 100

tab_s7_2 <- tab_s7_2 %>%
  mutate(Famille = as.character(Famille)) %>%
  arrange(desc(perc)) %>%
  mutate (Famille = ifelse(perc < 5, "Other", Famille)) %>%
  group_by(Famille) %>%
  summarize(perc = sum(perc)) %>%
  arrange(desc(perc))

tab_s7_2 <- tab_s7_2 %>%
  rename(perc_s7 = perc)
###############################
######################
tab_s14 <- invent_s14 %>%
  group_by(Famille) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

tab_s14 %>%
  select(count) %>%
  summarize(total = sum(count))
#171

tab_s14_2 <- tab_s14

tab_s14_2$perc <- tab_s14_2$count / 171 * 100

tab_s14_2 <- tab_s14_2 %>%
  mutate(Famille = as.character(Famille)) %>%
  arrange(desc(perc)) %>%
  mutate (Famille = ifelse(perc < 5, "Other", Famille)) %>%
  group_by(Famille) %>%
  summarize(perc = sum(perc)) %>%
  arrange(desc(perc))

tab_s14_2 <- tab_s14_2 %>%
  rename(perc_s14 = perc)

###############################
###############################
######################
tab_s15 <- invent_s15 %>%
  group_by(Famille) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

tab_s15 %>%
  select(count) %>%
  summarize(total = sum(count))
#262

tab_s15_2 <- tab_s15

tab_s15_2$perc <- tab_s15_2$count / 262 * 100

tab_s15_2 <- tab_s15_2 %>%
  mutate(Famille = as.character(Famille)) %>%
  arrange(desc(perc)) %>%
  mutate (Famille = ifelse(perc < 5, "Other", Famille)) %>%
  group_by(Famille) %>%
  summarize(perc = sum(perc)) %>%
  arrange(desc(perc))

tab_s15_2 <- tab_s15_2 %>%
  rename(perc_s15 = perc)
###############################
tab_full <- tab_all_2 %>%
  full_join(tab_s7_2, by = "Famille") %>%
  full_join(tab_s14_2, by = "Famille") %>%
  full_join(tab_s15_2, by = "Famille")


tab_all <- tab_all %>%
  rename(count_all = count)
tab_s7 <- tab_s7 %>%
  rename(count_s7 = count)
tab_s14 <- tab_s14 %>%
  rename(count_s14 = count)
tab_s15 <- tab_s15 %>%
  rename(count_s15 = count)

tab_pond <- tab_all %>%
  full_join(tab_s7, by = "Famille") %>%
  full_join(tab_s14, by = "Famille") %>%
  full_join(tab_s15, by = "Famille")

tab_pond$s7_pond <- tab_pond$count_s7 * 100/163
tab_pond$s14_pond <- tab_pond$count_s14 * 100/171
tab_pond$s15_pond <- tab_pond$count_s15 * 100/262


kp_long$weighted[is.na(kp_long$weighted)] <- 0
tab_pond[is.na(tab_pond)] <- 0

tab_pond$all_pond <- tab_pond$s7_pond + tab_pond$s14_pond + tab_pond$s15_pond

tab_pond %>%
  select(all_pond) %>%
  summarize(sum(all_pond))
#300: 100 each

tab_pond$all_pond_perc <- tab_pond$all_pond / 300 * 100
tab_pond %>%
  select(all_pond_perc) %>%
  summarize(sum(all_pond_perc))






tab_all_3 <- tab_pond
tab_all_3 <- tab_all_3 %>%
  mutate(Famille = as.character(Famille)) %>%
  arrange(desc(all_pond_perc)) %>%
  mutate (Famille = ifelse(all_pond_perc < 5, "Other", Famille)) %>%
  group_by(Famille) %>%
  summarize(all_pond_perc = sum(all_pond_perc)) %>%
  arrange(desc(all_pond_perc))


tab_all_3 <- tab_all_3 %>%
  full_join(tab_s7_2, by = "Famille") %>%
  full_join(tab_s14_2, by = "Famille") %>%
  full_join(tab_s15_2, by = "Famille")

tab_all_4 <- edit(tab_all_3)
write.csv(tab_all_4, file = "percentage_table.csv")

tab_final <- read.csv(file = "percentage_table.csv", header = T, stringsAsFactors = T)



kable(tab_all, col.names = c("Family", "Count"), format = "html", table.attr = "class='table table-striped'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

kable(tab_final, col.names = c("Family", "All sites", "Site 7", "Site 14", "Site 15"), format = "html", table.attr = "class='table table-striped'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
