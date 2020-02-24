#Lab 2
#Kehan Wang
#661983342

#Lab2 Part1
rm(list=ls())
setwd("H:/RPI/Spring 2020/Data Analytics/Assignment 2")

EPI_data <- read.csv("2010EPI_data.csv",skip=1)
attach(EPI_data)
dim(EPI_data)
#Remove null values
EPI_data <- EPI_data[1:163,]

head(EPI_data)
tail(EPI_data)
summary(EPI_data)

#Lab2a Measures of Central Tendency
summary(EPI)
names(table(EPI))[which(table(EPI)==max(table(EPI)))]
# From the result we can see that mean of EPI is 58.37, median of EPI is 59.20, mode of EPI are 44.6 and 51.3.
summary(DALY)
names(table(EPI))[which(table(DALY)==max(table(DALY)))]
# From the result we can see that mean of DALY is 53.62, median of DALY is 60.35, mode of DALY is 62.

#Lab2a Generate the Histogram for EPI and DALY variables
hist(EPI)
hist(DALY)

#Lab2a Dplyr exercise
#Using sample_n() function in dplyr, get 5 random data points from EPI, DALY 
library(dplyr)
sample_n(EPI_data, 5)$EPI
sample_n(EPI_data, 5)$DALY

#Using sample_frac() function in dplyr, get 10% random data points from EPI, DALY 
sample_frac(EPI_data, 0.1)$EPI
sample_frac(EPI_data, 0.1)$DALY

#Use the arrange() and desc() functions to arrange values in the descending order in the EPI and DALY  and assign them to new variables: new_decs_EPI and new_decs_DALY
new_decs_EPI <- arrange(EPI_data, desc(EPI))$EPI
new_decs_DALY <- arrange(EPI_data, desc(DALY))$DALY

#Using the mutate() function, create new columns: double_EPI and double_DALY where multiplying the values in EPI and DALY by 2 
mutate(EPI_data, double_EPI = EPI*2)
mutate(EPI_data, double_DALY = DALY*2)

#Using the summarise() function along with the mean() function to find the mean for EPI and DALY
summarise(EPI_data, avg_EPI = mean(EPI, na.rm = TRUE))
summarise(EPI_data, avg_DALY = mean(DALY, na.rm = TRUE))

boxplot(ENVHEALTH,ECOSYSTEM)
qqplot(ENVHEALTH,ECOSYSTEM)

#Lab2b Regression Exercise
#I choose Europe Region
EPI_data_new <- subset(EPI_data, EPI_regions == "Europe")

#Becasue I limited EPI_regions, so I could remove it from the dataset.
#As for Cuntry and GEO_subregion, they have high correlation with EPI_regions, so I removed them as well.
#Similarily, code and ISO3V10 are useless, so removed.
EPI_data_new <- EPI_data_new[, 6:160]
EPI_data_new["EPI"]
EPI1[""]
#convert EPI1 dataset into numeric
EPI1 <- sapply(EPI_data_new,as.numeric)
#make correlation table
corr <- round(cor(EPI1), 2)
corr <- data.frame(corr)
corr$EPI
# It can be seen that the biggest positive coefficient of EPI is ECOSYSTEM, which is 0.92.
# To confirm, I make a regression of the first 20 variables, since it includes ECOSYSTEM as well as the variables has meaning from their name, such as BIODIVERSITY, Desert and etc.
EPI1 <- as.data.frame(EPI1)
EPI2 <- EPI1[, 1:20]
fit <- lm(EPI ~ ., data = EPI2)
summary(fit)
#From the model we can see that P value of ECOSYSTEM  is very small, about 0, so the effect of ECOSYSTEM on EPI is very significant. Also,it has the positive coefficient and the value is pretty considerable. Thus, the single most important factor in increasing the EPI in Europe is ECOSYSTEM.

#Linear and least-squares
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)

#Predict
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<- predict(lmENVH,NEW,interval="prediction")
cENV<- predict(lmENVH,NEW,interval="confidence")

#Repeat for
#AIR_E
corr$AIR_E
EPI3 <- EPI1[, 10:30]
fit <- lm(AIR_E ~ ., data = EPI3)
summary(fit)
#Similarly, the single most important factor in increasing the AIR_E in Europe is SO2_pt.

#CLIMATE
corr$CLIMATE
fit <- lm(CLIMATE ~ ., data = EPI3)
summary(fit)
#Similarly, the single most important factor in increasing the CLIMATE in Europe is ECOSYSTEM.

#Exercise 1: Regression
Reg <- read.csv("dataset_multipleRegression.csv")
head(Reg)
dim(Reg)

fit1 <- lm(ROLL ~ UNEM + HGRAD, data = Reg)
new1 <- data.frame(UNEM = 7.0, HGRAD = 90000)
ROLL1 <- predict(fit1, newdata = new1)
ROLL1

fit2 <- lm(ROLL ~ UNEM + HGRAD + INC, data = Reg)
new2 <- data.frame(UNEM = 7.0, HGRAD = 90000, INC = 25000)
ROLL2 <- predict(fit2, newdata = new2)
ROLL2

#Exercise 2: Classification
ab <- read.csv("abalone.csv")
head(ab)
dim(ab)
ab$Rings <- as.numeric(ab$Rings)
ab$Rings <- cut(ab$Rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
ab$Rings <- as.factor(ab$Rings)
ab$Sex <- NULL
ab[1:7] <- scale(ab[1:7])

set.seed(1)
ind <- sample(2, nrow(ab), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- ab[ind==1,]
KNNtest <- ab[ind==2,]
k = sqrt(nrow(KNNtrain))

library(class)
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$Rings, k = k)
#Result
KNNpred

table(KNNpred)
table(KNNtest[,8], KNNpred, dnn = list('Actual', 'Predict'))

#Exercise 3: Clustering
ir <- iris[, -5]
head(ir)

#Method1
k.max <- 1000
wss<- sapply(1:k.max,function(k){kmeans(ir,k)$tot.withinss})
#Method2
for (i in 1:1000) {
  set.seed(1)
  predicted <- kmeans(ir, i)
  error_rate[i] <- mean(iris[,5] != predicted)
}
#I cannot make k to 1000 because we only have 150 observations in dataset iris.

#So I limit k to 20
k.max <- 20
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
#From the plot I can infer that when k =3, within cluster sum of squares becomes vary small and does not change anymore, so I choose k =3.

#Then I try maximum iteration equals 1000
set.seed(1)
icluster <- kmeans(ir,3, iter.max = 1000)
table(iris[,5], icluster$cluster, dnn = list('Acutual','Predict') )
# In the table we can see that most of the observations have been clustered correctly. 
# The model predict 50 setosa and actual has 50 setosa and all of them are predicted accurately.
# The model predict 50 versicolor just as Acutual. However 2 of the versicolor have been put in the cluster with most of them are virginica 
# Similarly, 14 of the verginica have been put in cluster 1 which mostly has versicolor.
