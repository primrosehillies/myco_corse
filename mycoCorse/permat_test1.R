## A simple artificial community data matrix.
m <- matrix(c(
  1,3,2,0,3,1,
  0,2,1,0,2,1,
  0,0,1,2,0,3,
  0,0,0,1,4,3
), 4, 6, byrow=TRUE)
## Using the quasiswap algorithm to create a 
## list of permuted matrices, where
## row/columns sums and matrix fill are preserved:
x1 <- permatswap(m, "quasiswap")
summary(x1)
## Unrestricted permutation retaining
## row/columns sums but not matrix fill:
x2 <- permatfull(m)
summary(x2)
## Unrestricted permutation of presence-absence type
## not retaining row/columns sums:
x3 <- permatfull(m, "none", mtype="prab")
x3$orig  ## note: original matrix is binarized!
summary(x3)
## Restricted permutation,
## check sums within strata:
x4 <- permatfull(m, strata=c(1,1,2,2))
summary(x4)

## NOTE: 'times' argument usually needs to be >= 99
## here much lower value is used for demonstration

## Not sequential algorithm
data(BCI)
a <- permatswap(BCI, "quasiswap", times=19)
## Sequential algorithm
b <- permatswap(BCI, "abuswap", fixedmar="col",
                burnin=0, thin=100, times=19)
opar <- par(mfrow=c(2,2))
plot(a, main="Not sequential")
plot(b, main="Sequential")
plot(a, "chisq")
plot(b, "chisq")
par(opar)
## Extract Bray-Curtis dissimilarities
## as time series
bc <- as.ts(b)
## Lag plot
lag.plot(bc)
## First order autoregressive model
mar <- arima(bc, c(1,0,0))
mar
## Ljung-Box test of residuals
Box.test(residuals(mar))
## Graphical diagnostics
tsdiag(mar)







essence_presence_count <- invent %>%
  mutate(Identifiant.site = paste0("S", Identifiant.site)) %>%
  count(Essence, Taxon.fonge) %>%
  pivot_wider(names_from = Taxon.fonge, values_from = n, values_fill = list(n = 0))

essence_presence_count <- as.data.frame(essence_presence_count)

row.names(essence_presence_count) <- essence_presence_count$Essence
essence_presence_count$Essence <- NULL


x4 <- permatswap(essence_presence_count, "quasiswap")
summary(x4)



permuted_matrices <- x4$perm


calculate_metric <- function(matrix) {
  original_bray <- vegdist(essence_presence_count, method = "bray")
  permuted_bray <- vegdist(matrix, method = "bray")
  mean(permuted_bray - original_bray)
}

# Apply function to each permuted matrix
null_distribution <- sapply(permuted_matrices, calculate_metric)


# Calculate the Bray-Curtis dissimilarity for the original matrix
observed_bray <- vegdist(essence_presence_count, method = "bray")
observed_metric <- mean(observed_bray)



# Calculate a p-value for observed metric relative to the null distribution
p_value <- mean(null_distribution >= observed_metric)

# Plot null distribution and observed value
hist(null_distribution, main = "Null Distribution of Bray-Curtis Dissimilarity",
     xlab = "Bray-Curtis Dissimilarity", col = "lightblue", border = "gray")
abline(v = observed_metric, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Observed Metric"), col = "red", lty = 2, lwd = 2)



range(null_distribution)  # Check min and max of the null distribution
observed_metric    




