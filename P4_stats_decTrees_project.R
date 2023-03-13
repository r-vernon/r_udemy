
# load necessary packages
library(tidyverse)
library(ISLR2) # for dataset
library(caTools) # for crude train/test
library(rpart) # for basic decision tree
library(rpart.plot) # for plotting decision tree
library(randomForest) # for random forest

# grab the data
# Private     - Private or Public university (factor yes/no)
# Apps        - Number of applications received
# Accept      - Number of applications accepted
# Enroll      - Number of new students enrolled
# Top10perc   - Pct. new students from top 10% of H.S. class
# Top25perc   - Pct. new students from top 25% of H.S. class
# F.Undergrad - Number of fulltime undergraduates
# P.Undergrad - Number of parttime undergraduates
# Outstate    - Out-of-state tuition
# Room.Board  - Room and board costs
# Books       - Estimated book costs
# Personal    - Estimated personal spending
# PhD         - Pct. of faculty with Ph.D.’s
# Terminal    - Pct. of faculty with terminal degree
# S.F.Ratio   - Student/faculty ratio
# perc.alumni - Pct. alumni who donate
# Expend      - Instructional expenditure per student
# Grad.Rate   - Graduation rate
df <- College

# check for missing data
if (any(is.na(df))) {print('found nulls')}
# (no nulls!)

# relevel so 'yes' is first factor in Private
df$Private <- relevel(df$Private, "Yes")

# ------------------------------------------------------------------------------
# initial exploration

# plot graduation rate vs room+board costs
pl <- ggplot(df, aes(x=Room.Board, y=Grad.Rate)) + 
    geom_point(aes(color=Private))
print(pl)
# public seem to be cheaper with lower grad. rate

# histogram on num. full time undergrads
pl <- ggplot(df, aes(x=F.Undergrad)) + 
    geom_histogram(bins=50, position='dodge', color='black', aes(fill=Private))
print(pl)

# histogram on graduation rate
pl <- ggplot(df, aes(x=Grad.Rate)) + 
    geom_histogram(bins=50, position='dodge', color='black', aes(fill=Private))
print(pl)
# note, one college has graduation rate greater than 100!

# make sure graduation rate capped at 100
df$Grad.Rate <- ifelse(df$Grad.Rate > 100, 100, df$Grad.Rate)

# do that for other percent columns too!
pctCols <- c(5,6,13,14,16,18) # columns that are %
toCheck <- colSums((df[,pctCols] > 100 | df[,pctCols] < 0))
# only PhD to check, min is 8 but max is 103!
df$PhD <- ifelse(df$PhD > 100, 100, df$PhD)

# lets check for any more outliers! (Q1-1.5IQR, Q3+1.5IQR)
isOut <- function(x) {
    x_Q1Q3 <- quantile(x,probs=c(0.25,0.75))
    x_IQR <- diff(x_Q1Q3) * c(-1.0,1.0)
    x_Out <- x_Q1Q3 + (1.5*x_IQR)
    x <- (x < x_Out[1] | x > x_Out[2])
    return(x)
}
df_Out <- sapply(df[,-1],isOut)
# fair few number of outliers in number categories
# 12.4% outliers for fulltime undergrads - likely not normallly distributed!
# (although doesn't matter so much for decision trees)

# for now we'll ignore, but mental note!

# ------------------------------------------------------------------------------
# test a decision tree

# set seed for consistency
set.seed(101)

# create random split
split <- sample.split(df$Private, SplitRatio=0.7)
df_train <- df[split,]
df_test <- df[!split,]

# build and test a decision tree
tree <- rpart(Private ~ . , method='class', data=df_train)
tree_pred <- predict(tree, df_test, type='class')
table(df_test$Private, tree_pred, dnn=c('data','pred'))

# plot the tree
prp(tree)

# ------------------------------------------------------------------------------
# test a random forest

# build the model
rfModel <- randomForest(Private ~ ., data=df_train, importance=T)
rfModel_pred <- predict(rfModel, df_test, type='response')
table(df_test$Private, rfModel_pred, dnn=c('data','pred'))

# confusion set for training data
print(rfModel$confusion)

# feature importance
print(rfModel$importance)

# ------------------------------------------------------------------------------
# just out of interest, lets see if PCA improves things!

# perform pca
pc <- prcomp(df[,-1], center=T, scale=T)
summary(pc)

# get proportion variance explained
pc_pvar <- data.frame(Comp=seq(1,17), pVar=(pc$sdev^2) / sum(pc$sdev^2))
pc_pvar$cVar <- cumsum(pc_pvar$pVar)
pl <- ggplot(pc_pvar, aes(x=Comp,y=pVar)) + geom_line() + geom_point()
print(pl)
screeplot(pc, type='lines') # alternative

# we'll keep 6, about 80% variance explained
df_pc <- as.data.frame(pc$x[,1:6])
df_pc$Private <- df$Private

# lets try the cross-fold validation approach again, see if there's more luck!
n <- nrow(df)
nFolds <- 10
cfIdx <- sample(rep.int(1:nFolds, ceiling(n/nFolds)))[1:n] # sample shuffles
m1_F1 <- double(nFolds)
m2_F1 <- double(nFolds)
for (currFold in 1:nFolds) {
    
    currIdx <- (cfIdx!=currFold)
    trg <- df$Private[!currIdx]
    
    # build the models
    m1 <- randomForest(Private ~ ., data=df[currIdx,])
    m2 <- randomForest(Private ~ ., data=df_pc[currIdx,])
    
    # get the predictions
    p1 <- predict(m1, df[!currIdx,], type='response')
    p2 <- predict(m2, df_pc[!currIdx,], type='response')
    
    # calculate performance
    TP1 <- sum(trg=='Yes' & p1=='Yes')
    FP1 <- sum(trg=='No' & p1=='Yes')
    FN1 <- sum(trg=='Yes' & p1=='No')
    TP2 <- sum(trg=='Yes' & p2=='Yes')
    FP2 <- sum(trg=='No' & p2=='Yes')
    FN2 <- sum(trg=='Yes' & p2=='No')
    
    # Precision and recall
    Pr1 <- TP1 / (TP1 + FP1)
    Re1 <- TP1 / (TP1 + FN1)
    Pr2 <- TP2 / (TP2 + FP2)
    Re2 <- TP2 / (TP2 + FN2)
    
    # F1_score, weighted average of both
    m1_F1[currFold] <- 2 * (Pr1*Re1)/(Pr1+Re1)
    m2_F1[currFold] <- 2 * (Pr2*Re2)/(Pr2+Re2)
}

# m1 had higher F1 on average, lower SD, PCs may not help!