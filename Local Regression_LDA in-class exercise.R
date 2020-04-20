# LOESS Example from:
# http://r-statistics.co/Loess-Regression-With-R.html
data(economics, package="ggplot2")  # load data
economics$index <- 1:nrow(economics)  # create index variable
head(economics)
economics <- economics[1:80, ]  # retail 80rows for better graphical understanding
help("loess")
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span
loessMod50 <- loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span

# Predict Loess
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 
# From the plot, you would notice that as the span increases, the smoothing of the curve also increases.
# Code for Plot
# Plot it
plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")

# LOWESS example using the Cars dataset
data("cars")
head(cars)
str(cars) # we see 50 observation and 2 variables 
# now we create a plot, speed Vs distance
plot(speed ~ dist, data = cars)
# When we look at the plot, we see that there is a positive relationship between these two variables
# Read the documentation for the lowess function in RStudio
help("lowess")
# Now we will use the lowess() function
lowess(cars$speed ~ cars$dist)
# Now we will use the lowess() function along with the line() function 
# to draw the lines
lines(lowess(cars$speed ~ cars$dist, f=2/3), col="blue") 
# here the f value is the the smoother span, f= 2/3 = 0.666 
# the default value for smoother span is 0.666 in RStudio.

#This gives the proportion of points in the plot which influence the smooth at each value.
# Larger values give more smoothness.
# Change the "f" value and observe the shape of the line.
lines(lowess(cars$speed ~ cars$dist, f=0.75), col="gray")  # f = 0.75
lines(lowess(cars$speed ~ cars$dist, f=0.8), col="red")  # f = 0.8
lines(lowess(cars$speed ~ cars$dist, f=0.9), col="green")  # f = 0.9
lines(lowess(cars$speed ~ cars$dist, f=0.1), col= 5)  # f = 0.1
lines(lowess(cars$speed ~ cars$dist, f=0.01), col= 6)  # f = 0.01 
# Observe that, when we try to have a very lower values for "f", in this example, it will try to overfit points.

# Linear Discriminant Analysis Example using Iris dataset.
# In order to use the lda() function, you need to have the MASS library.
# Multiclass Classification 
library(MASS)
names(iris)
dim(iris) # check the dimensions of the iris dataset, you will see 150 rows and 5 columns
head(iris)

# Creating the training dataset using the Random sampling using the sample() function 
# we will allocate half of the dataset to train the model that we are planning to build.
# setting the seed value
set.seed(555)
Train <- sample(1:nrow(iris), nrow(iris)/2)
iris_Train <- iris[Train,] # Traning dataset
iris_Test <- iris[-Train,] # Testing dataset

# Read the lda() function documentation on RStudio
help(lda)
# now we will use the lda() function to fit the model
fit1 <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris_Train)

# We will use the predict() function to conduct the prediction based on the fit1 model we built
# with the Train dataset
predict1 <- predict(fit1, iris_Train)
predict1_class <- predict1$class
predict1_class

# generating the confusion matrix using the table() function 
table1 <- table(predict1_class, iris_Train$Species)
table1
# Calculating the Accuracy of the prediction
sum(diag(table1))/sum(table1)

# Test dataset
predict2 <- predict(fit1, iris_Test)
predict2_class <- predict2$class
# table
table2 <- table(predict2_class, iris_Test$Species)
table2
# accuracy
sum(diag(table2))/sum(table2)
