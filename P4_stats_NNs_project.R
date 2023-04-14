
# import necessary stuff
library(tidyverse)
library(caTools) # split data
library(reshape2) # reshape df (melt)
library(neuralnet) # for the nn
library(randomForest)

# read in the data
df <- read.csv('./dataFiles/bank_note_data.csv')

# create a boxplot
df_long <- melt(df, id='Class')
df_long$Class <- as.factor(df_long$Class)
pl <- ggplot(df_long, aes(x=variable, y=value, color=Class)) + 
    geom_boxplot()
print(pl)
# variance and skew look most useful, curtosis/entropy not quite as much
# not on wildly different scales, although variance differs a fair bit
# likely don't need to normalise data

# split data
split <- sample.split(df$Class, SplitRatio=0.7)
df_tr <- subset(df, split==T) # train
df_te <- subset(df, split==F) # test

# ------------------------------------------------------------------------------
# train the neural net

# create the formula
# set the formula
f <- as.formula('Class ~ Image.Var + Image.Skew + Image.Curt + Entropy')

# train
nn <- neuralnet(f, data=df_tr, hidden=10, linear.output=F)

# test
nn_out <- compute(nn, df_te[1:4])
nn_pred <- round(nn_out$net.result)
table(df_te$Class, nn_pred)

# ------------------------------------------------------------------------------
# compare to random forest

# set class as factor
df_tr$Class <- as.factor(df_tr$Class)
df_te$Class <- as.factor(df_te$Class)

# get random forest predictions
rf <- randomForest(Class ~ ., data=df_tr)
rf_pred <- predict(rf, df_te, type='response')
table(df_te$Class, rf_pred)
