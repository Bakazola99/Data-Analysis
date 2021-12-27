
## Installing PCA library

library(stats)
library(dplyr)

## Loading the Iris dataset
iris
iris1 <-(iris[1:4])
view(iris1)

## Correlation

cor_iris1 = cor(iris1)
cor_iris1

# To visualise the correlation

corrplot(cor_iris1, method = c("number"), type = c('lower'))

## PCA Analysis
pca_iris1 = princomp(iris1)
pca_iris1

# To check how the PCA captures the component i.e PC loading
pca_iris1$loadings

## To evaluate the principal component if they are highly correlated i.e. 
##corr coef must be zero.
pc <- pca_iris1$scores
view(pc)
pc_cor = cor(pc)
corrplot(pc_cor, method = c("ellipse"), type = c('upper'))

## Interpretation of Result
# From the result of the PCA, after grouping the variables into components of
# how correlated they are with one another, we then check the scores of the
# components. We observed that all the components correlating with one another 
# with score value zero show that each components composed of different variables
# and the variables they composed are different from one another.


