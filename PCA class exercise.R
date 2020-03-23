### Random Forest
require(randomForest)
library(rpart)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	# view results
importance(fitKF) # importance of each predictor


### PCA on USArrests dataset
# In this lab, we perform PCA on the USArrests data set, which is part of the base R package. 
# The rows of the data set contain the 50 states, in alphabetical order.
# We will use the USAArrest data that available on RStudio
data("USArrests")
states=row.names(USArrests) 
states
# The columns of the data set contain the four variables.
names(USArrests )

# We first briefly examine the data. We notice that the variables have vastly different means.
# Note that the apply() function allows us to apply a function-in this case, the mean() function 
# to each row or column of the data set.
# The second input here denotes whether we wish to compute the mean of the rows, 1, or the columns, 2.
# We see that there are on average three times as many rapes as murders, 
# and more than eight times as many assaults as rapes.
apply(USArrests , 2, mean)
# We can also examine the variances of the four variables using the apply() function.
apply(USArrests , 2, var)

# Not surprisingly, the variables also have vastly different variances: the UrbanPop variable measures the percentage of the population 
# in each state living in an urban area, which is not a comparable number to the number of rapes in each state per 100,000 individuals. 
# If we failed to scale the variables before performing PCA, 
# then most of the principal components that we observed would be driven by the Assault variable, 
# since it has by far the largest mean and variance. Thus, it is important to standardize the variables to have mean zero and 
# standard deviation one before performing PCA.

# We now perform principal components analysis using the prcomp() function, which is one of several functions in R that perform PCA.
# By default, the prcomp() function centers the variables to have mean zero. By using the option scale=TRUE, 
# we scale the variables to have standard deviation one.
# The output from prcomp() contains a number of useful quantities.
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

# The center and scale components correspond to the means and standard deviations of the 
# variables that were used for scaling prior to implementing PCA.
pr.out$center
pr.out$scale

# The rotation matrix provides the principal component loadings; each column of pr.out$rotation contains 
# the corresponding principal component loading vector
# We see that there are four distinct principal components.
pr.out$rotation

# Using the prcomp() function, we do not need to explicitly multiply the data by 
# the principal component loading vectors in order to obtain the principal component score vectors.
# Rather the 50 × 4 matrix x has as its columns the principal component score vectors. 
# That is, the kth column is the kth principal component score vector.
pr.out$x
dim(pr.out$x)

# We can plot the first two principal components as follows:
biplot(pr.out, scale=0)
# The scale=0 argument to biplot() ensures that the arrows are scaled to represent the loadings;
# other values for scale give slightly different biplots with different interpretations.

# The prcomp() function also outputs the standard deviation of each principal component. 
# For instance, on the USArrests data set, we can access these standard deviations as follows:
pr.out$sdev

# The variance explained by each principal component is obtained by squaring these:
pr.var = pr.out$sdev^2
pr.var

# To compute the propotion of variance explained by each principal component, we simply
# devide the variance explained by each principal component by the total variance explained
# by all four principal components.
pve = pr.var/sum(pr.var)
pve
# We can see the first principal component explains 62.0% of the variance in the data.
# The next principal component explains 24.74% of the variance, and so forth...

### PCA on Iris dataset

# PCA with iris dataset
data("iris")
head(iris)
# creating another dataset from iris dataset that contains the columns from 1 to 4 
irisdata1 <- iris[,1:4]
irisdata1

head(irisdata1)
# Read the documentation for the princomp() function in RStudio.
help("princomp")
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
# cor = a logical value indicating whether the calculation should 
# use the correlation matrix or the covariance matrix.
# (The correlation matrix can only be used if there are no constant variables.)
# score = a logical value indicating whether the score on  
# each principal component should be calculated.
summary(principal_components)
# in the summary you can see that it has four Principal Components it is because the input data has
# four different features.

# using the plot() function, we can plot the principal components.
plot(principal_components)
# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")
# using the biplot() function we can plot the components
biplot(principal_components)


### PCA on Boston dataset
install.packages('MASS')
data(Boston, package="MASS")
# Read the documentation of Boston dataset in RStudio to understand the dataset
help(Boston)
names(Boston)
head(Boston)
dim(Boston)
# Principal Component Analysis
# the prcomp() fucntion computes the principal components and we have turned on scalling
# Read the documentation for prcompt() function in RStudio
help(prcomp)
pca_out <- prcomp(Boston,scale. = T)
# pca_out shows the loadings that used.
pca_out
plot(pca_out)

# plotting using the biplot()
# Read the documentation for biplot() function in RStudio
help(biplot)

biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
# boston_pc has the Princial Components having the same number of rows in the original dataset
head(boston_pc)
summary(boston_pc)
