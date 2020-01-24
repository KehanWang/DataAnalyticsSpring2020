# Creating a dataframe
# Example: RPI Weather dataframe,

days <- c('Mon','Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun') # days
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4) # Temperature in F' during the winter
snowed <- c('T','T','F','F','T','T','F') # Snowed on that day:T = TRUE, F= FALSE
help("data.frame")
RPI_Weather_Week <- data.frame(days, temp, snowed) # creating the dataframe using the data.frame

RPI_Weather_Week
head(RPI_Weather_Week) # head of the data frame, NOTE: it will snow only 6 rows, usually head() function shows the first 6 rows of the dataframe, hewe we have onlhy 7 rows in our dataframe.

str(RPI_Weather_Week) # we can take a look at the structure of the dataframe using the str() function

summary(RPI_Weather_Week) # summary of the dataframe using the summary function

RPI_Weather_Week[1,] # showing the 1st row and all columns
RPI_Weather_Week[,1] # showing the 1st column and all rows

RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week, SUBSET = snowed == TRUE)

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

# RPI_Weather_Week[descending_snowed,]
dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow

#creating dataframes
#creating an empty dataframe
empty.dataframe <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df

# Exercise - getting data in
rm(list=ls())
setwd("H:/RPI/Spring 2020/Data Analytics/group1")

GPW3 <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")
GPW3 <- read.csv(file.choose(), header = TRUE)
EPI_data <- read.csv("2010EPI_data.csv",skip=1)

plot(EPI_data$EPI)
plot(EPI_data$EPI_regions)

data()
help(data)

#Files
help("read.csv")
View(EPI_data)

attach(EPI_data)
fix(EPI_data)
EPI
tf <- is.na(EPI)
E <- EPI[!tf]

# Exercise 1: exploring the distribution
summary(EPI)
fivenum(EPI,na.rm=TRUE)
help(stem)
stem(EPI)
help(hist)
hist(EPI)
hist(EPI, seq(30., 95., 1.0),prob=TRUE)
help(lines)
lines(density(EPI,na.rm=TRUE,bw=1.))
help(rug)
rug(EPI)

# Exercise 1: fitting a distribution beyond histograms
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
?ecdf()
qqnorm(EPI); qqline(EPI)
x <- rt(250, df = 5)
qqplot(qt(ppoints(250), df = 5),x, xlab = "Q-Q plot for t dsn")
?qt()
qqline(x)

#Exercise 1
boxplot(EPI, DALY)
boxplot(DALY, WATER_H)
qqplot(EPI,DALY)
plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)

help("distributions")

# Exercise 2: filtering(populations)
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)

#GPW3
View(GPW3)
a <- is.na(GPW3)
b <- GPW3[!a]

# Exercise 1: exploring the distribution
summary(GPW3$NumUnits)
stem(GPW3$NumUnits)
hist(GPW3$NumUnits)
lines(density(GPW3$NumUnits,na.rm=TRUE,bw=1.))
rug(GPW3$NumUnits)

plot(ecdf(GPW3$NumUnits), do.points=FALSE, verticals=TRUE)
qqnorm(GPW3$NumUnits); qqline(GPW3$NumUnits)