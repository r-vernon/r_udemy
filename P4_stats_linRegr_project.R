# Bike sharing demand gaggle challenge
# - https://www.kaggle.com/c/bike-sharing-demand/data
# - lin. regr. likely not ideal due to seasonality but good practice
#
# Format:
# - datetime: hourly date + timestamp
# - season: 1=spring, 2=summer, 3=fall, 4=winter
# - holiday: is holiday or not
# - workingday: is day work day (not weekend or holiday)
# - weather: 1=best, 2=good, 3=bad, 4=worst
# - temp: temp in C
# - atemp: 'feels like' temp (C)
# - humidity: rel. humidity
# - windspeed: windspeed
# - casual: num-non registered rentals
# - registers: num registered rentals
# - count: total num rentals
 
# load in necessary stuff
library(tidyverse)
library(ggthemes)
library(corrplot) # plot a correlation matrix
library(caTools) # split data into training/test
library(patchwork) # subplots
library(viridis) # nice color schemes

# ------------------------------------------------------------------------------ 
# initial processing

# read in the data (T=datetime, f=factor, l=logical, d=double, i=integer)
bike <- read_csv('dataFiles/bikeshare.csv', col_types='Tfllfddddiii')

# check for nulls (there aren't any!)
if (any(is.na(bike))) {print('found nulls)')}

# check count makes sense (it does!)
if (any(bike$count != bike$casual + bike$registered)) {print('check count')}

# add hour column extracted from datatime
bike <- mutate(bike, hour=as.numeric(format(datetime, '%H')), .before=season)

# rename factors for clarity
# levels(bike$season) <- c('spr','sum','aut','win')

# ------------------------------------------------------------------------------ 
# initial exploration

# plot temp vs count
pl <- ggplot(bike, aes(x=temp, y=count)) + 
  geom_point(aes(color=temp), alpha=0.4) +
  scale_color_gradient(low='blue', high='red')
print(pl)

# repeat but with date vs count
# - shows some seasonality to the data!
# - also shows increasing trend over time
pl <- ggplot(bike, aes(x=datetime, y=count)) + 
  geom_point(aes(shape=season, color=temp), alpha=0.4)+
  scale_color_gradient(low='blue', high='red')
print(pl)

# explore seasonality with boxplot
# - note that increasing trend renders this rather useless!
pl <- ggplot(bike, aes(x=season, y=count)) + 
  geom_boxplot(aes(color=season))
print(pl)

# lets make a correlation matrix
corMat <- cor(bike[,sapply(bike,is.numeric)])
corSig <- cor.mtest(bike[,sapply(bike,is.numeric)], conf.level=0.95)
corrplot(corMat, p.mat=corSig$p, method = 'color', diag=F, insig='pch',
         col=rev(COL2('RdYlBu',200)), addgrid.col='black', 
         addCoef.col='black', tl.col='black')

# plot count vs hour (working vs nonworking day)
# - note that hour is at least bimodal, not good for lin. regr.
pl1 <- ggplot(subset(bike, workingday==T), aes(x=hour, y=count)) + 
  geom_point(aes(color=temp), alpha=0.4, position=position_jitter(w=1, h=0)) + 
  scale_color_viridis(option='viridis') + ggtitle('Working Days')
pl2 <- ggplot(subset(bike, workingday==F), aes(x=hour, y=count)) + 
  geom_point(aes(color=temp), alpha=0.4, position=position_jitter(w=1, h=0)) + 
  scale_color_viridis(option='viridis') + ggtitle('Non-Working Days')
print(pl1+pl2)

# ------------------------------------------------------------------------------ 
# build the model

# first attempt, just with temp (~15.6% var)
m1 <- lm(count ~ temp, data=bike)
summary(m1)

# predict count at 25deg
predict(m1, list(temp=25))

# build more advanced model (~34.6% var)
# using all preds except listed ones (casual etc)
m2 <- lm(count ~ . -casual -registered -datetime -atemp, data=bike)
summary(m2)

# playing around
m3 <- lm(count ~ temp + humidity + temp*humidity, data=bike)
summary(m3)
# temp and tempXhumidity both sig, ~25.5%
m3 <- lm(count ~ temp + humidity + temp*humidity + hour, data=bike)
summary(m3)
# adding hour in, now all vars are sig. with ~33.2% var (on par with m2!)

# however, Q-Q plot suggests deviation from normal dist.
# - too much variability at higher quantiles
# we know working days has a big impact on hour at least, add it in
m4 <- lm(count ~ temp + humidity + temp*humidity + hour + 
           workingday + hour*workingday, data=bike)
summary(m4)
# not noticeably better! (33.2% var)

# one last thought:
# - casual: much more likely to depend on weather
# - registered: more likely to depend on hour
# - predict separately and see how well combined they predict count
m3a <- lm(casual ~ temp + humidity + temp*humidity + hour, data=bike)
m3b <- lm(registered ~ temp + humidity + temp*humidity + hour, data=bike)
summary(m3a) # R^2 ~ 37.0% var
summary(m3b) # R^2 ~ 24.8% var - limiting factor is registered renters
m3_pred <- predict(m3a,bike) + predict(m3b,bike)
m3_resid <- bike$count - m3_pred
SSE <- sum(m3_resid^2)
SST <- var(bike$count) * (nrow(bike)-1)
R2 <- 1.0 - (SSE/SST)
# 33.1% var, not noticeably better than orig. m3!

# try adding in predictor for ratio of registered to casual renters
bike <- mutate(bike, rentRat=registered/count, .before=count)
m5 <- lm(count ~ temp + humidity + temp*humidity + hour + rentRat, data=bike)
summary(m5)
# it is a sig. predictor, but also something not known in advance, so not
# a useful metric ><

# one more thought, lets build a model per season!
m6a <- lm(count ~ temp + humidity + temp*humidity + hour, 
          data=subset(bike,season==1))
summary(m6a) # spring
m6b <- lm(count ~ temp + humidity + temp*humidity + hour, 
          data=subset(bike,season==2))
summary(m6b) # summer
m6c <- lm(count ~ temp + humidity + temp*humidity + hour, 
          data=subset(bike,season==3))
summary(m6c) # autumn
m6d <- lm(count ~ temp + humidity + temp*humidity + hour, 
          data=subset(bike,season==4))
summary(m6d) # winter
# m6a - 26.8% var, humidity not sig. but tempxhumidity is
# m6b - 35.8% var, humidity not sig. but tempxhumidity definitely is
# m6c - 35.9% var, humidity is sig.
# m6d - 30.8% var, humidity not sig. but tempxhumidity definitely is

# lets throw in season and seasonxhumidity interaction in
m7 <- lm(count ~ temp + humidity + temp*humidity + hour + season + season*humidity,
         data = bike)
summary(m7)
# now at 36% var so only marginal improvement but all preds (except humidity) sig.
# still huge nonlinearity
# m3 seems like the best we've got without resorting to more appropriate tools!


# okay one more test, hour as factor!
m3_test <- lm(count ~ temp + humidity + temp*humidity + factor(hour), data=bike)
summary(m3_test)
# 61.0% of var!
# was an idiot before, hour isn't linear so needed further refining ><
# not an ideal model but hints at where to go next...