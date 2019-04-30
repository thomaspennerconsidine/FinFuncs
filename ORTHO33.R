# Generate scaled 4*5 matrix with random std normal samples
N <- 100

PC1 <- rnorm(N,0,10)
PC2 <- rnorm(N,1000,10)
PC3 <- rnorm(N,10,100)
PC4 <- rnorm(N,2,10)



mat <- data.frame(PC1,PC2,PC3,PC4)


# Perform PCA
myPCA <- prcomp(mat, scale. = F, center = F)
myPCA$rotation # loadings
myPCA$x # scores


# Perform SVD
mySVD <- svd(mat)
mySVD # the diagonal of Sigma mySVD$d is given as a vector
sigma <- matrix(0,4,4) # we have 4 PCs, no need for a 5th column
diag(sigma) <- mySVD$d # sigma is now our true sigma matrix


# Compare PCA scores with the SVD's U*Sigma
theoreticalScores <- mySVD$u %*% sigma
all(round(myPCA$x,5) == round(theoreticalScores,5)) # TRUE

# Compare PCA loadings with the SVD's V
all(round(myPCA$rotation,5) == round(mySVD$v,5)) # TRUE

# Show that mat == U*Sigma*t(V)
recoverMatSVD <- theoreticalScores %*% t(mySVD$v)
all(round(mat,5) == round(recoverMatSVD,5)) # TRUE

#Show that mat == scores*t(loadings)
recoverMatPCA <- myPCA$x %*% t(myPCA$rotation)
all(round(mat,5) == round(recoverMatPCA,5)) # TRUE


