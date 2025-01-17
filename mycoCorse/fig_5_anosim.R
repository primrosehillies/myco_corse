library(vegan)
library(tidyverse)

invent_rect <- read.csv("inventory5.csv", header = T, stringsAsFactors = T)
invent_rect$host0 <- sub("-.*", "", invent_rect$Hôte)

invent_rect$host <- paste(invent_rect$Identifiant.site, invent_rect$host0, sep = "_")



#####Host matrix####
matrix_host <- invent_rect %>%
  count(host, Essence, Identifiant.site, Taxon.fonge) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = n, values_fill = list(n = 0))

# matrix_host <- invent_rect %>%
#   select(host, Identifiant.site) %>%
#   distinct(host, Identifiant.site) %>%
#   left_join(matrix_host, by = "host")

host <- matrix_host[,4:ncol(matrix_host)]
m_host <- as.matrix(host)

ano_site <- anosim(m_host, matrix_host$Identifiant.site, permutations = 999, distance = "bray")
summary(ano_site)
plot(ano_site)

ano_essence <- anosim(m_host, matrix_host$Essence, permutations = 999, distance = "bray")
summary(ano_essence)








nmds <- metaMDS(m_ess, distance = "bray")
nmds
plot(nmds)

data.scores = as.data.frame(scores(nmds)$sites)

#add columns to data frame 
data.scores$Identifiant.site = matrix_host$Identifiant.site
data.scores$Identifiant.site <- as.character(data.scores$Identifiant.site)
data.scores$Essence <- matrix_host$Essence

head(data.scores)


library(ggplot2)


xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape = host, colour = Identifiant.site)) + 
  labs(x = "NMDS1", colour = "Site", y = "NMDS2", shape = "Essence")  + 
  scale_colour_manual(values = c("#009E73", "#E69F00", "#009")) + 
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  stat_ellipse(aes(x=NMDS1,y=NMDS2,colour=Identifiant.site),level = 0.50) +
  theme_minimal()
xx




#PERMANOVA#
adonis <- adonis2(m_host ~ matrix_host$Essence, method = "bray")
print(adonis)



adonis2 <- adonis2(m_host ~ matrix_host$Identifiant.site, method = "bray")
print(adonis2)


#####Essence matrix####
matrix_essence <- invent_rect %>%
  count(Essence, Identifiant.site, Taxon.fonge) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = n, values_fill = list(n = 0))





ess <- matrix_essence[,3:ncol(matrix_essence)]
m_ess <- as.matrix(ess)

ano_ess_site <- anosim(m_ess, matrix_essence$Identifiant.site, permutations = 999, distance = "bray")
summary(ano_ess_site)


ano_ess_essence <- anosim(m_ess, matrix_essence$Essence, permutations = 999, distance = "bray")
summary(ano_ess_essence)








nmds_ess <- metaMDS(m_ess, distance = "bray")
nmds_ess
plot(nmds_ess)

data.scores = as.data.frame(scores(nmds_ess)$sites)

#add columns to data frame 
data.scores$Identifiant.site = matrix_essence$Identifiant.site
data.scores$Identifiant.site <- as.character(data.scores$Identifiant.site)
data.scores$Essence <- matrix_essence$Essence

head(data.scores)


library(ggplot2)


xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape = Essence, colour = Identifiant.site)) + 
  labs(x = "NMDS1", colour = "Site", y = "NMDS2", shape = "Essence")  + 
  scale_colour_manual(values = c("#009E73", "#E69F00", "#009")) + 
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  stat_ellipse(aes(x=NMDS1,y=NMDS2,colour=Identifiant.site),level = 0.50) +
  theme_minimal()
xx


#####PCoA####
library(vegan)
library(ggplot2)
distance_matrix <- vegdist(m_host, method = "bray") 
pcoa_result <- cmdscale(distance_matrix, k = 2, eig = TRUE)
plot(pcoa_result$points[, 1], pcoa_result$points[, 2])
pcoa_result
head(distance_matrix)


pcoa_df <- data.frame(
  PC1 = pcoa_result$points[, 1],
  PC2 = pcoa_result$points[, 2]
)

# Add explained variance for axes labels
eigenvalues <- pcoa_result$eig
explained_variance <- eigenvalues / sum(eigenvalues)
x_label <- paste0("PC1 (", round(explained_variance[1] * 100, 1), "%)")
y_label <- paste0("PC2 (", round(explained_variance[2] * 100, 1), "%)")

# Plot
ggplot(pcoa_df, aes(x = PC1, y = PC2)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(x = x_label, y = y_label, title = "PCoA Plot") +
  theme_minimal()




eigenvalues <- pcoa_result$eig
explained_variance <- eigenvalues / sum(eigenvalues)
cumulative_variance <- cumsum(explained_variance)

summary_df <- data.frame(
  Axis = 1:length(eigenvalues),
  Eigenvalue = eigenvalues,
  ExplainedVariance = explained_variance,
  CumulativeVariance = cumulative_variance
)

print(summary_df)

# Show proportion of variance explained by the first two axes
cat("Variance explained by PC1:", round(explained_variance[1] * 100, 2), "%\n")
cat("Variance explained by PC2:", round(explained_variance[2] * 100, 2), "%\n")



pcoa_df <- data.frame(
  PC1 = pcoa_result$points[, 1],
  PC2 = pcoa_result$points[, 2],
  Essence = matrix_host$Essence,  # 9 categories
  IdentifiantSite = matrix_host$Identifiant.site  # 3 categories
)

pcoa_df$Essence <- as.character(pcoa_df$Essence)
pcoa_df$IdentifiantSite <- as.character(pcoa_df$IdentifiantSite)
eigenvalues <- pcoa_result$eig
explained_variance <- eigenvalues / sum(eigenvalues)
x_label <- paste0("PC1 (", round(explained_variance[1] * 100, 1), "%)")
y_label <- paste0("PC2 (", round(explained_variance[2] * 100, 1), "%)")

# Plot
ggplot(pcoa_df, aes(x = PC1, y = PC2, shape = Essence, color = IdentifiantSite)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_shape_manual(values = 1:9) +  # Up to 9 different shapes
  scale_color_manual(values = c("red", "blue", "green")) +
  labs(x = x_label, y = y_label)
  theme_minimal()
