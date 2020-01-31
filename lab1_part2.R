rm(list=ls())
setwd("H:/RPI/Spring 2020/Data Analytics/group1")
EPI_data <- read.csv("2010EPI_data.csv",skip=1)
attach(EPI_data)

#Exercise 1: fitting a distribution beyond histograms
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
plot(ecdf(EPI), do.points=TRUE,verticals = TRUE) 
help("qqnorm")
par(pty="s") 
qqnorm(EPI); qqline(EPI)
EPI
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x2, xlab = "Q-Q plot")
qqline(x)

qqplot(EPI,DALY)
boxplot(EPI_data$EPI,EPI_data$DALY)

#Linear

multivariate <- read.csv("multivariate.csv")
head(multivariate)
attach(multivariate)
help(lm)
mm<-lm(Homeowners~Immigrant)
summary(mm)
summary(mm)$coef 
mm
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients
?attributes
newImmigrantdata <- data.frame(Immigrant = c(0,  20))
predict(mm, newImmigrantdata)

mtcars
head(mtcars)
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars)
ggplot(mtcars, aes(x=wt, y=mpg))+geom_point()
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature,pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="blue")

qplot(pressure$temperature,pressure$pressure, geom="line")
qplot(temperature,pressure, data=pressure, geom="line")
ggplot(pressure, aes(x=temperature, y=pressure))+geom_line()+geom_point()
ggplot(pressure, aes(x=temperature, y=pressure))+geom_line()+geom_point()

# creating bar graphs
barplot(BOD$demand, names.arg=BOD$Time)
barplot(BOD$demand, BOD$Time)
table(mtcars$cyl)        
barplot(table(mtcars$cyl)) # generate a table of counts
qplot(mtcars$cyl) # cyl is continuous here
qplot(factor(mtcars$cyl)) # treat cyl as discrete
# bar grph of counts
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x=factor(cyl)))+geom_bar()

# create histogram
#view the distribution of one-dimension data with histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 5) # specify approximate number of bins with breaks
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 12)
hist(mtcars$mpg, breaks = seq(1,40,5))
?hist

qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x=mpg)) +geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg)) +geom_histogram(binwidth = 5)
ggplot(mtcars, aes(x=mpg)) +geom_histogram(binwidth = 6)

ggplot(mtcars, aes(x=mpg)) +geom_histogram(bins = 3)
ggplot(mtcars, aes(x=mpg)) +geom_histogram(bins = 5)
ggplot(mtcars, aes(x=mpg)) +geom_histogram(bins = 10)

?geom_histogram

#creating boxplot
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()

#Data Manipulation using Dplyr

# Dplyr 
install.packages('dplyr')
install.packages('nycflights13')

library(dplyr)
library(nycflights13)
head(flights)
summary(flights)

#filter
filter(flights,month == 10, day == 4, carrier =='AA')
head(filter(flights, month == 10, day == 4, carrier == 'AA'))
head(flights[flights$month == 10 & flights$day == 4 & flights$carrier == 'AA' , ]) # here I have to keep calling the dataframe name, and use the logical operators with '&' and combine them.

#slice
slice(flights, 1:15) 

#arrange
arrange(flights,year,month,day, arr_time)
head(arrange(flights,year,month,day,arr_time))
head(arrange(flights,year,month,day, desc(arr_time)))

#select
select(flights,carrier)
head(select(flights,carrier))
head(select(flights, carrier, arr_time))
head(select(flights, carrier, arr_time, day))
head(rename(flights, airline.carrier = carrier))

#distinct
distinct(select(flights, carrier))

#mutate/transmute
head(mutate(flights, MyNewColumn = arr_delay - dep_delay))
head(transmute(flights, MyNewColumn = arr_delay - dep_delay))

#summarise
summarise(flights, avg_air_time = mean(air_time, na.rm = TRUE)) 
summarise(flights, TotalFlightTime = sum(air_time, na.rm = TRUE)) 

#sample
sample_n(flights, 15) 
sample_n(flights, 71) 

sample_frac(flights,0.1) 
sample_frac(flights, 0.3) 
sample_n(flights, 30)
sample_frac(flights, 0.5)

df_mtcars <- mtcars
head(df_mtcars)

#nest
#method 1
filter(df_mtcars, mpg > 20)
sample_n(filter(df_mtcars, mpg > 20), 10)
arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
results_mpg <- arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
results_mpg

#method 2
install.packages('magrittr')
library(magrittr)

a <- filter(df_mtcars, mpg > 20)
b <- sample_n(a,10)
c <- arrange(b, desc(mpg))
c

#method 3
df_mtcars %>% filter(mpg > 20) %>%
  sample_n(10) %>%
  arrange(desc(mpg))