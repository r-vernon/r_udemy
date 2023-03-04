
# import necessary stuff
library(class) # for KNN
library(tidyverse) 
library(ISLR2) # for data set
library(patchwork) # for subplots

set.seed(101)

# load in the data (using classic iris data set)
df <- iris
n <- nrow(df)

# extract output column (target)
trg <- factor(df$Species)
df <- select(df,-Species)

# convert trg to numeric
trg_orig <- trg
trg <- factor(as.integer(trg_orig))
nLvls <- nlevels(trg)

# standardise all the predictors (converts to matrix)
df_z <- scale(df)

# lets try the cross-fold validation approach again, see if there's more luck!
nFolds <- 5
cfIdx <- sample(rep.int(1:nFolds, ceiling(n/nFolds)))[1:n] # sample shuffles
kRng <- seq(1,20,1) # sqrt(n)=12.24 good guess for k so test that range
allAcc <- double(length(kRng))
allF1 <- double(length(kRng))
for (currFold in 1:nFolds) {
    inc <- 1
    currIdx <- (cfIdx!=currFold)
    trnTrg <- trg[currIdx]  # making train + test targets as will make
    tstTrg <- trg[!currIdx] # code more readable, and they're small
    for (currK in kRng){
        currPred <- knn(df_z[currIdx,], df_z[!currIdx,], trnTrg, k=currK)
        allAcc[[inc]] <- allAcc[[inc]] + sum(tstTrg==currPred)/length(tstTrg)
        Pr <- 0.0 # precision, when pred T, how often correct
        Re <- 0.0 # recall, when actually T, how often pred T
        for (currLvl in 1:nLvls) {
            TP <- sum(tstTrg==currLvl & currPred==currLvl)
            FP <- sum(tstTrg!=currLvl & currPred==currLvl)
            FN <- sum(tstTrg==currLvl & currPred!=currLvl)
            Pr <- Pr + (TP / (TP + FP))
            Re <- Re + (TP / (TP + FN))
        }
        Pr <- Pr/nLvls # calculate avg. Pr/Re over each level
        Re <- Re/nLvls 
        F1 <- 2 * (Pr*Re)/(Pr+Re) # F1_score, weighted average of both
        allF1[[inc]] <- allF1[[inc]] + F1
        inc <- inc + 1
    }
}
allF1 <- allF1/nFolds
allAcc <- allAcc/nFolds

# throw it into a dataframe
err_df <- tibble(kRng, allAcc, allF1)

# plot it
pl1 <- ggplot(err_df, aes(x=kRng, y=allAcc)) + geom_point() + 
    geom_line(lty='dotted',color='red') + ggtitle('Acc')
pl2 <- ggplot(err_df, aes(x=kRng, y=allF1)) + geom_point() + 
    geom_line(lty='dotted',color='red') + ggtitle('F1')
print(pl1+pl2)

# k = 5 looks like the reasonable option, build model and test!
trIdx <- sample.int(n, size=round(0.7*n)) # select random 70% of data
m1Pred <- knn(df_z[trIdx,], df_z[-trIdx,], trg[trIdx], k=5)

# check accuracy 
cm <- table(trg[-trIdx], m1Pred, dnn=c('testData','m1Pred'))
print(cm) 
