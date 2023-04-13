
# load relevant packages
library(tidyverse)
library(MASS) # for data set
library(caTools) # split data
library(neuralnet) # for the nn

# load data (housing data from Boston, 70s ish)
# predicting median value (medv)
df <- Boston

# check for missing data (there isn't any)
any(is.na(df))

# preprocess data
dfMax <- apply(df, 2, max) # get maxes for all cols
dfMin <- apply(df, 2, min) # get mins for all cols
dfRng <- dfMax-dfMin # range
df_sc <- as.data.frame(scale(df, center=dfMin, scale=dfRng))
# subtracted min from every column, divided by range (so new range 0-1)

# split data
split <- sample.split(df_sc$medv, SplitRatio=0.7)
df_tr <- subset(df_sc, split==T) # train
df_te <- subset(df_sc, split==F) # test

# ------------------------------------------------------------------------------
# train the neural net

# create the formula
n <- names(df_tr)
f <- as.formula(paste('medv ~', paste(n[!n %in% 'medv'], collapse='+')))

# train
nn <- neuralnet(f, data=df_tr, hidden=c(5,3), linear.output=T)
plot(nn)

# test
nn_out <- compute(nn, df_te[1:13])
# get a list of neurons and their predictions

# rescale predictions (and do same for test data to allow direct comparison)
nn_pred <- (nn_out$net.result * dfRng['medv']) + dfMin['medv']
te_out <- (df_te$medv * dfRng['medv']) + dfMin['medv']

# calculate mean squared error
nn_MSE <- sum((te_out - nn_pred)^2)/nrow(df_te)

# plot
df_err <- data.frame(te_out, nn_pred)
pl <- ggplot(df_err, aes(x=te_out, y=nn_pred)) + geom_point() + stat_smooth()
print(pl)

# how well would linear regression do on the same data?
linm <- lm(f, data=df_tr)
linm_pred <- (predict(linm, df_te) * dfRng['medv']) + dfMin['medv']
linm_MSE <- sum((te_out - linm_pred)^2)/nrow(df_te)
df_err$linm_pred <- linm_pred
pl <- ggplot(df_err, aes(x=te_out, y=linm_pred)) + geom_point() + stat_smooth()
print(pl)
# did far worse!