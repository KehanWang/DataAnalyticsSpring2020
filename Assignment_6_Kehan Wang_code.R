setwd("H:/RPI/Spring 2020/Data Analytics/Assignment 6")
library(readxl)
df <- read_excel("data.xlsx")
df <- as.data.frame(df)
dim(df)

# EDA
# null value
is.null(df)

# box plot
boxplot(df$GDP, col = "orange", border = "blue")

# line chart
library(ggplot2)
ggplot(df,aes(x=Date, y=GDP))+geom_point(colour = "orange")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.9))

# correlation table
cor(df[2:6])
# heat map
library(corrplot)
corrplot(cor(df[2:6]), method="shade")

# summary of response variable
summary(df$GDP)

# split GDP into different classes
df$GDP_class <- cut(df$GDP, br=c(-1,12061782,17753721,21726780), labels = c("low", 'medium', 'high'))
ggplot(df,aes(x=Date, y=Unemployment_rate, colour = GDP_class)) +geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.9))
ggplot(df,aes(x=Personal_income, y = Net_saving, fill = GDP_class)) +geom_boxplot()+theme_bw()

df <- df[, -c(1,8)]
# time series data
library(forecast)
df.ts <- ts(df,start = c(1999,1),end = c(2019,4),freq=4)
head(df.ts)
tsdisplay(df.ts)

# normal model
set.seed(1)
train.index <- sample(c(1:dim(df)[1]), dim(df)[1]*0.7)  
train.df <- df[train.index, ]
valid.df <- df[-train.index, ]

fit.lm <- lm(GDP~., data = train.df)
summary(fit.lm)

pred.lm <- predict(fit.lm, newdata = valid.df)
accuracy(pred.lm, valid.df$GDP)

# stepwise
fit.step <-step(fit.lm, direction = "backward")
summary(fit.step)

pred.step <- predict(fit.step, newdata = valid.df)
accuracy(pred.step, valid.df$GDP)

# regression tree
library(rpart)
library(rpart.plot)
set.seed(1)
cv.ct <- rpart(GDP ~ ., data = train.df, method = "anova", cp = 0.00001, minsplit = 5)
printcp(cv.ct)

pruned.ct <- prune(cv.ct, cp = 1.3396e-03)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'orange', 'blue'))
# variable importance
importance <- pruned.ct$variable.importance
importance <- as.data.frame(importance)
importance$variable <- row.names(importance)
row.names(importance) <- c(1:5)
ggplot(importance,aes(x=variable,y=importance,fill=variable))+
  geom_bar(stat="identity",width = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.9))
# prediction
pred.tree <- predict(pruned.ct, newdata = valid.df)
accuracy(pred.tree, valid.df$GDP)

# time series
# only GDP
library(tseries)
GDP <- df[, 1]
head(GDP)
GDP.ts <- ts(GDP,start = c(1999,1),end = c(2019,4),freq=4)
tsdisplay(GDP.ts)

# spilt data(70%+30%)
nValid <- 26
nTrain <- 58
GDP.train.ts <- window(GDP.ts, start = c(1999,1), end = c(1999,nTrain))
GDP.valid.ts <- window(GDP.ts,start = c(1999,nTrain+1),end=c(1999,nTrain+nValid))
tsdisplay(GDP.train.ts)

# Check stable
adf.test(GDP.train.ts)
# not stable

# difference
library(forecast)
ndiffs(GDP.train.ts)
ntrain<-diff(GDP.train.ts,1)
ndiffs(ntrain)
tsdisplay(ntrain)

# tslm
fit.tslm <- tslm(GDP.train.ts~trend+season)
summary(fit.tslm)

pred.tslm <- forecast(fit.tslm,h=nValid,level=0)
plot(pred.tslm,  ylab = "GDP",
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1999,2019.4), main = "tslm", flty = 2)
axis(1, at = seq(1999, 2019, 1), labels = format(seq(1999, 2019, 1)))
lines(pred.tslm$fitted, lwd = 2, col = "blue")  
lines(GDP.train.ts)
lines(GDP.valid.ts)

# fit model
# arima
auto.arima(GDP.train.ts)
fit.arima <- arima(GDP.train.ts,order=c(1,1,0))
tsdiag(fit.arima)

pred.arima <- forecast(fit.arima,h = 26, level=c(99.5))
plot(pred.arima)
lines(pred.arima$fitted,col="green")
lines(GDP.ts,col="red")
