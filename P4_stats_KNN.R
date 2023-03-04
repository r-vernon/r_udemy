# NOTES FOR K Nearest Neighbours
# ------------------------------
#
# classifies based on nearest neighbours
# - pick a point
# - choose K nearest neighbours (distance metric can vary)
# - assign point to case majority in nearest neighbours
#
# v. costly with large datasets!

# ==============================================================================
# load in some data

# load necessary libraries
library(class) # for knn
library(tidyverse)
library(ISLR2) # for the Caravan data
library(caTools) # for test/train

# load data
df <- Caravan
n <- nrow(df)

# extract and save the output (purchased insurance or not)
boughtInsur <- df$Purchase
df <- select(df,-Purchase)

# check for NAs (there aren't any)
any(is.na(df))

# standardise the predictors so they don't bias predictions
df_z <- scale(df) # now mean 0, SD 1 

# split into training, test data
trIdx <- sample.split(boughtInsur, SplitRatio=0.7) # training index

# ------------------------------------------------------------------------------
# build initial model

# model(pass in training data, test data, training outputs)
m1Pred <- knn(df_z[trIdx,], df_z[!trIdx,], boughtInsur[trIdx], k=1)

# check accuracy
cm1 <- table(boughtInsur[!trIdx], m1Pred, dnn=c('testData','m1Pred'))
print(round(cm1/sum(!trIdx),2)) 

# ------------------------------------------------------------------------------
# refine model by choosing better version for 'k'
# going to use 5-fold cross-validation
# - doing it manually, could also use caret package
cfIdx <- sample(rep.int(1:5, ceiling(n/5)))[1:n] # sample shuffles
kRng <- seq(1,100,5)
allPr <- double(length(kRng))
allRe <- double(length(kRng))
allF1 <- double(length(kRng))
for (currFold in 1:5) {
    currIdx <- (cfIdx!=currFold)
    trnTrg <- boughtInsur[currIdx]  # making train + test targets as will make
    tstTrg <- boughtInsur[!currIdx] # code more readable, and they're small
    inc <- 1
    for (currK in kRng){
        currPred <- knn(df_z[currIdx,], df_z[!currIdx,], trnTrg, k=currK)
        # acc <- mean(tstTrg != currPred) # accuracy
        TP <- sum(tstTrg==T & currPred==T)
        FP <- sum(tstTrg==F & currPred==T)
        FN <- sum(tstTrg==T & currPred==F)
        Pr <- TP / (TP + FP) # precision, when pred T, how often correct
        Re <- TP / (TP + FN) # recall, when actually T, how often pred T
        F1 <- 2 * (Pr*Re)/(Pr+Re) # F1_score, weighted average of both
        allPr[[inc]] <- allPr[[inc]] + coalesce(Pr,0)
        allRe[[inc]] <- allRe[[inc]] + coalesce(Re,0)
        allF1[[inc]] <- allF1[[inc]] + coalesce(F1,0)
        inc <- inc + 1
    }
}
allPr <- allPr/5
allRe <- allRe/5
allF1 <- allF1/5

# ===============================================================
# NOTE:
# originally ran using accuracy as metric
# not great as biased data set (6% positive)
# switched to various other metrics (precession, recall, F1)
# none worked! (use this as a training exercise, not analysis...)
# ===============================================================

# throw it into a dataframe
err_df <- tibble(kRng, allPr, allRe, allF1)

# plot it
pl <- ggplot(err_df, aes(x=kRng, y=allPr)) + geom_point() + 
    geom_line(lty='dotted',color='red')
print(pl)

# k = 10 looks like the best option, build model and test!
m2Pred <- knn(df_z[trIdx,], df_z[!trIdx,], boughtInsur[trIdx], k=10)

# check accuracy
cm2 <- table(boughtInsur[!trIdx], m2Pred, dnn=c('testData','m2Pred'))
print(round(cm2/sum(!trIdx),2)) 
