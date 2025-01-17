library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)

phy_fam <- invent %>%
  select(Phylum, Famille) %>%
  distinct()

count_family <- invent %>%
  select(Phylum, Famille) %>%
  group_by(Famille) %>%
  summarize(count = n()) %>%
  left_join(phy_fam, by = "Famille") %>%
  arrange(desc(count)) %>%
  mutate(perc = round((count / sum(count)) * 100, 2))


perc_phylum_all <- invent %>%
  select(Phylum, Famille) %>%
  group_by(Phylum) %>%
  summarize(count = n()) %>%
  mutate(perc = round((count/sum(count)) * 100, 2))


perc_phylum_s7 <- invent_s7 %>%
  select(Phylum, Famille) %>%
  group_by(Phylum) %>%
  summarize(count = n()) %>%
  mutate(perc = round((count/sum(count)) * 100, 2))


perc_phylum_s14 <- invent_s14 %>%
  select(Phylum, Famille) %>%
  group_by(Phylum) %>%
  summarize(count = n()) %>%
  mutate(perc = round((count/sum(count)) * 100, 2))


perc_phylum_s15 <- invent_s15 %>%
  select(Phylum, Famille) %>%
  group_by(Phylum) %>%
  summarize(count = n()) %>%
  mutate(perc = round((count/sum(count)) * 100, 2))


thresh_basidio <- 3.5

count_family_basidio <- count_family %>%
  filter(Phylum == "Basidiomycète") 
count_family_basidio$Famille <- as.character(count_family_basidio$Famille)
count_family_basidio <- count_family_basidio %>%
  mutate(Famille = ifelse(perc < thresh_basidio, "Other Basidiomycetes", Famille)) 
  


thresh_asco <- 5
count_family_asco <- count_family %>%
  filter(Phylum == "Ascomycète")
count_family_asco$Famille <- as.character(count_family_asco$Famille)
count_family_asco <- count_family_asco %>%
  mutate(Famille = ifelse(perc < thresh_asco, "Other Ascomycetes", Famille))

count_family_mucoro <- count_family %>%
  filter(Phylum == "Mucoromycète")


count_family_asco_basidio <-count_family_basidio %>%
  rbind(count_family_asco) %>%
  rbind(count_family_mucoro)


count_for_plot <- data.frame(
  Famille = c("Russulaceae", "Thelephoraceae", "Hydnangiaceae", 
             "Other Basidiomycetes", "Dothydeomycetes", "Myxotrichaceae", 
             "Pyronemataceae", "Helotiales", "Other Ascomycetes", "Mucoromycetes"),
  count = c(119, 104, 23, 144, 
            49, 41, 33, 32, 51,
            4)
)

count_for_plot <- count_for_plot %>%
  mutate(perc = round((count/sum(count)) * 100, 2))

count_for_plot$Famille <- factor(count_for_plot$Famille, levels = rev(c("Russulaceae", "Thelephoraceae", "Hydnangiaceae",
                                                                        "Other Basidiomycetes", "Dothydeomycetes", "Myxotrichaceae",
                                                                        "Pyronemataceae", "Helotiales", "Other Ascomycetes", "Mucoromycetes"))
)


colors_camembert <- c(
  "Russulaceae" = "#008b90",       
  "Thelephoraceae" = "#00a0a5",    
  "Hydnangiaceae" = "#00b5bb",     
  "Other Basidiomycetes" = "#00cad0", 
  "Dothydeomycetes" = "#d43d31",  
  "Myxotrichaceae" = "#ff4b3e",  
  "Pyronemataceae" = "#ff8c42",  
  "Helotiales" = "#FFBC27",      
  "Other Ascomycetes" = "#ffd983ff",
  "Mucoromycetes" = "#6800d1ff"
)


ggplot(count_for_plot, aes(x = "", y = count, fill = Famille)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = colors_camembert) +
  theme_pubclean() +
  labs(fill = "Family") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())


ggplot(count_family_asco_basidio, aes(x="", y=count, fill=Famille)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  # geom_text(aes(label = paste0(perc, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) # +
  # scale_fill_manual(values = c("Ascomycète" = "#f8766dff", 
  #                              "Basidiomycète" = "#00bfc4ff", 
  #                              "Mucoromycète" = "#6800d1ff"))






#####Tous sites confondus####
count_phylum <- count_phylum %>%
  mutate(percentage = round((count / sum(count)) * 100,2))


plot_phy_all <- ggplot(count_phylum, aes(x="", y=count, fill=Phylum)) +
  ggtitle("Tous sites confondus") +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual(values = c("Ascomycète" = "#f8766dff", 
                               "Basidiomycète" = "#00bfc4ff", 
                               "Mucoromycète" = "#6800d1ff"))


#####Site 7####
count_phylum_s7 <- count_phylum_s7 %>%
  mutate(percentage = round((count / sum(count)) * 100,2))

plot_phy_7 <- ggplot(count_phylum_s7, aes(x="", y=count, fill=Phylum)) +
  ggtitle("Site 7") +
  theme_nothing() +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  #geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual(values = c("Ascomycète" = "#f8766dff", 
                               "Basidiomycète" = "#00bfc4ff", 
                               "Mucoromycète" = "#6800d1ff"))

#####Site 14####
count_phylum_s14 <- count_phylum_s14 %>%
  mutate(percentage = round((count / sum(count)) * 100,2))


plot_phy_14 <- ggplot(count_phylum_s14, aes(x="", y=count, fill=Phylum)) +
  theme_nothing() +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  #geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual(values = c("Ascomycète" = "#f8766dff", 
                               "Basidiomycète" = "#00bfc4ff", 
                               "Mucoromycète" = "#6800d1ff"))


#####Site 15####
count_phylum_s15 <- count_phylum_s15 %>%
  mutate(percentage = round((count / sum(count)) * 100,2))

plot_phy_15 <- ggplot(count_phylum_s15, aes(x="", y=count, fill=Phylum)) +
  theme_nothing() +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  #geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual(values = c("Ascomycète" = "#f8766dff", 
                               "Basidiomycète" = "#00bfc4ff", 
                               "Mucoromycète" = "#6800d1ff"))

plot_phy <- ggarrange(plot_phy_all, plot_phy_7, plot_phy_14, plot_phy_15, ncol = 2, nrow = 2)
plot_phy


















# Sample data frame
df <- data.frame(
  Family = c("Russulaceae", "Thelephoraceae", "Hydnangiaceae", 
             "Other Basidiomycetes", "Dothydeomycetes", "Myxotrichaceae", 
             "Pyronemataceae", "Helotiales", "Other Ascomycetes"),
  Count = c(300, 200, 100, 150, 180, 250, 130, 170, 210)
)

# Set custom order
df$Family <- factor(df$Family, levels = c(
  "Russulaceae", "Thelephoraceae", "Hydnangiaceae", "Other Basidiomycetes",
  "Dothydeomycetes", "Myxotrichaceae", "Pyronemataceae", "Helotiales", "Other Ascomycetes"
))

# Define custom colors
custom_colors <- c(
  "Russulaceae" = "#1f78b4", # Blue
  "Thelephoraceae" = "#ff7f00", # Orange
  "Hydnangiaceae" = "#33a02c", # Green
  "Other Basidiomycetes" = "#e31a1c", # Red
  "Dothydeomycetes" = "#6a3d9a", # Purple
  "Myxotrichaceae" = "#b15928", # Brown
  "Pyronemataceae" = "#fdbf6f", # Peach
  "Helotiales" = "#fb9a99", # Pink
  "Other Ascomycetes" = "#a6cee3"  # Light Blue
)

# Plot
ggplot(df, aes(x = "", y = Count, fill = Family)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(fill = "Family") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
