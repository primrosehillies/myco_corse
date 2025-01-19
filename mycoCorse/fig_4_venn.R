#####Venn diagram####
library(VennDiagram)
library(ggVennDiagram)


essence_presence_casta
casta_fungi <- list()

for (tree in rownames(essence_presence_casta)) {
  # Find fungi species with presence (1) for the current tree
  fungi_present <- colnames(essence_presence_casta)[essence_presence_casta[tree, ] == 1]
  
  # Create a one-column matrix with fungi names as rows
  casta_fungi[[tree]] <- matrix(fungi_present, ncol = 1, dimnames = list(NULL, tree))
}


casta_fungi <- lapply(casta_fungi, as.data.frame)




fagaceae <- bind_rows(
  casta_fungi$`Fagus sylvatica`,  
  casta_fungi$`Quercus pubescens`,
  casta_fungi$`Quercus ilex`
) %>%
  pivot_longer(cols = everything(), values_to = "fagaceae") %>%
  filter(!is.na(fagaceae)) %>%
  select(fagaceae) %>%
  distinct()

pinaceae <- casta_fungi$`Pinus spp.`
ericaceae <- casta_fungi$`Arbutus unedo`

other_families <- bind_rows(
  casta_fungi$`Arbutus unedo`,
  casta_fungi$`Cistus salviifolius`,
  casta_fungi$`Ostrya carpinifolia`
) %>%
  pivot_longer(cols = everything(), values_to = "other_families") %>%
  filter(!is.na(other_families)) %>%
  select(other_families) %>%
  distinct()


fagaceae <- fagaceae$fagaceae
pinaceae <- pinaceae$`Pinus spp.`
ericaceae <- ericaceae$`Arbutus unedo`
other_families <- other_families$other_families

myCol <- brewer.pal(4, "Pastel2")

venn_casta <- venn.diagram(
  x = list(fagaceae, pinaceae, ericaceae, other_families),
  category.names = c("Other Fagaceae (30)", "Pinaceae (14)", "Ericaceae (16)", "Other families (18)"),
  filename = NULL, 
  output = FALSE,
  
  #Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  
  #Names
  cat.cex = 1,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.fontfamily = "sans"
)
grid.draw(venn_casta)


upset <- list(Fagaceae = fagaceae, 
              Pinaceae = pinaceae, 
              Ericaceae = ericaceae, 
              Other_families = other_families)
venn <- Venn(upset)
plot_upset(venn)


