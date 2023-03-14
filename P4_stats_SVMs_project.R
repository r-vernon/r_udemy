
# load in necessary libraries
library(tidyverse)
library(e1071) # for SVMs
library(caTools) # train/test split

# load in the data
# credit.policy: 1 if cust. meets credit underwriting criteria of LendingClub
# purpose: purpose of loan ("credit_card", "debt_consolidation", "educational", 
#          "major_purchase", "small_business", "all_or")
# int.rate: interest rate of loan (e.g. 0.1), higher = more risky
# installment: monthly instalments owed by borrower if loan funded
# log.annual.inc: natural log of self-reported annual income of borrower
# dti: debt-to-income ratio of borrower (amount of debt / annual income)
# fico: FICO credit score of borrower
# days.with.cr.line: num days borrower has had credit line
# revol.bal: borrower's revolving balance (amnt unpaid at end of CC bill cycle)
# revol.util: borrower's revolving line utilization rate 
#             (amnt of credit line used relative to total credit available)
# inq.last.6mths: borrower's num inquiries by creditors in last 6 mths
# delinq.2yrs: num times borrower been 30+ days past due on payment in past 2yrs
# pub.rec: borrower's number of derogatory public records 
#          (bankruptcy filings, tax liens, or judgements)
# not.fully.paid: 1 if borrower not fully paid
df <- read_csv('./dataFiles/loan_data.csv', col_types='lfddddddddiiil')

# convert some columns to categorical (factors)
df$inq.last.6mths <- factor(df$inq.last.6mths)
df$delinq.2yrs <- factor(df$delinq.2yrs)
df$pub.rec <- factor(df$pub.rec)
df$not.fully.paid <- factor(df$not.fully.paid)
df$credit.policy <- factor(df$credit.policy)

# ------------------------------------------------------------------------------ 
# explore the data

# histogram (freq. polygons) of fico vs not.fully.paid
nBins <- nclass.FD(df$fico)
pl <- ggplot(df, aes(x=fico)) + 
    geom_freqpoly(bins=nBins, aes(color=not.fully.paid))
print(pl)

# check out loan purpose
pl <- ggplot(df, aes(x=purpose)) + 
    geom_bar(aes(fill=not.fully.paid), position='dodge') + 
    theme(axis.text.x = element_text(angle=90))
print(pl)

# how does interest rate compare to fico (credit) score
pl <- ggplot(df, aes(x=int.rate, y=fico, color=not.fully.paid)) + 
    geom_point(alpha=0.5)
print(pl)
# higher interest rate correlates with lower credit score, surprise...

# try splitting paid/not paid to separate graphs instead
pl <- ggplot(df, aes(x=int.rate, y=fico)) + 
    geom_point(alpha=0.5) + 
    facet_grid(. ~ not.fully.paid)
print(pl)
# looks like same relationship in both

# ------------------------------------------------------------------------------ 
# build a model

# set seed
set.seed(101)

# split the data
dfSplit <- sample.split(df$not.fully.paid, SplitRatio = 0.7)
dfTrn <- df[dfSplit,]
dfTst <- df[!dfSplit,]

# first build naive model
st <- Sys.time()
m1 <- svm(not.fully.paid ~ ., data=dfTrn, kernel='radial')
ed <- Sys.time()
print(ed-st) # 3s/model
m1Pred <- predict(m1, dfTst)
table(m1Pred, dfTst$not.fully.paid)
# didn't do great!

# tune the model
costRng <- exp(seq(log(0.1),log(1000),length.out=5))
gamRng <- exp(seq(log(0.0001),log(1),length.out=5))
st <- Sys.time()
# tuneResults <- tune(svm, train.x=not.fully.paid~., data=dfTrn, kernel='radial', 
#                     ranges=list(cost=costRng, gamma=gamRng))
ed <- Sys.time()
print(ed-st) # took 24 mins!

# after all that, best cost was 1e-1 and best gamma 1e-4
# aka first points in our sequences...
# redo with much smaller range if we're every so inclined...

# test params anyway...
m2 <- svm(not.fully.paid ~ ., data=dfTrn, kernel='radial', cost=1e-1, gamma=1e-4)
m2Pred <- predict(m2, dfTst)
table(m2Pred, dfTst$not.fully.paid)