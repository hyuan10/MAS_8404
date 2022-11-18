## Load mlbench package
library(mlbench)
## Load the data
data(BreastCancer)

library(tidyverse)

## Removing NA not replacing with Mode/Median
no_NA <- drop_na(BreastCancer)

## Removing ID column
no_NA <- no_NA[,-1]

no_NA <- no_NA[,-10]

## Turn data frame to matrix
mBC <- data.matrix(no_NA, rownames.force=NA)

## Calculating the Variance
VBC <- apply(mBC, 2, var)

## Correlation
cor1 <- cor(mBC)

## Selecting Cell Size and Shape
X = mBC[,c(2,3)]

plot(X[,1], X[,2], xlab = expression(x[1]), ylab = expression(x[2]))

z = scale(X)

plot(z[,1], z[,2], xlab = expression(z[1]), ylab = expression(z[2]))


## Sample Covariance 
S = var(mBC)

(s_sq = diag(S))

## Total Variance
(total_variantion = sum(s_sq))

## PCA on data
(pca1 = prcomp(x=mBC))

summary(pca1)

## Scree diagram for PCA1
plot(pca1, type="lines",main="")
title(xlab="Component Number")


## PCA for scales
(pca2 = prcomp(x=mBC, scale =TRUE ))

summary(pca2)

## Scree diagram for PCA2
plot(pca2, type="lines",main="")
title(xlab="Component Number")