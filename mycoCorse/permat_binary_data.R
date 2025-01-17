#####Old models ####
#1
null_model_1 <- permatfull(essence_presence_count)
#Extracting permuted datasets list
null_perm_1 <- null_model_1$perm

names(null_perm_1) <- paste0("perm_", seq_along(null_perm_1))
# Ensure each element in null_perm_1 is explicitly a data frame
null_perm_1 <- lapply(null_perm_1, as.data.frame)

# Now add the row names from essence_presence_count as a new variable
null_perm_1 <- lapply(null_perm_1, function(df) {
  rownames(df) <- rownames(essence_presence_count)
  return(df)
})


#2
null_model_2 <- permatswap(essence_presence_count, method = "quasiswap")
#Extracting permuted datasets list
null_perm_2 <- null_model_2$perm

names(null_perm_2) <- paste0("perm_", seq_along(null_perm_2))
# Ensure each element in null_perm_1 is explicitly a data frame
null_perm_2 <- lapply(null_perm_2, as.data.frame)

# Now add the row names from essence_presence_count as a new variable
null_perm_2 <- lapply(null_perm_2, function(df) {
  rownames(df) <- rownames(essence_presence_count)
  return(df)
})



#####New models####
#Link density (0.18) is used as probability of having a 1 in permuted matrix
#It means that we're gonna be looking if species distribution is different from random or not
null_model_3 <- permatfull(essence_presence, mtype = "prab", fixedmar = "none") $
#prab means presence-absence data, fixedmar means that row and column sums are not taken into consideration when making the new matrices --> probability of 0.18 (= link density) of having a 1
null_perm_3 <- null_model_3$perm
names(null_perm_3) <- paste0("perm_", seq_along(null_perm_3))
null_perm_3 <- lapply(null_perm_3, as.data.frame)

null_perm_3 <- lapply(null_perm_3, function(df) {
  rownames(df) <- rownames(essence_presence)
  return(df)
})

summary(null_model_3)



#Row and column sums are now taken into consideration to build permuted matrices: row and column sums always remain identical.
#Here, we keep the bipartite degree of every plant and mushroom, BUT, we look if the distribution remains the same
null_model_4 <- permatswap(essence_presence, mtype = "prab", method = "quasiswap")
null_perm_4 <- null_model_4$perm
names(null_perm_4) <- paste0("perm_", seq_along(null_perm_4))
null_perm_4 <- lapply(null_perm_4, as.data.frame)

null_perm_4 <- lapply(null_perm_4, function(df) {
  rownames(df) <- rownames(essence_presence)
  return(df)
})

summary(null_model_4)
