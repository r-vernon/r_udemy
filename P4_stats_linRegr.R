# NOTES FOR LINEAR REGRESSION
# ---------------------------
#  
# Estimating Y ~ b0 + b1X1 + ... + bnXn + err.
# - typically do so by min. Residual Sum of Squares (RSS)
#
# note on multiple regression:
# - when we have multiple estimates (b1, b2, ..., bn) the size of (e.g.) b1
#   reflects the effect of x1 holding other variables (x2...xn) fixed
#  - this gets complicated when regressors are correlated!
#
# say we're taking repeated estimates of a mean from some population
# - variance of those means = variance(population)/n
#  - as n increases (more samples), a given mean will be closer to pop. mean
# - can sqrt(var) to get SE (Std. Err.): SE = SD/sqrt(n)
#  - (commonly used for confidence intervals: xbar +/- 1.96SEM)
#
# can calculate SE (squared) for b0, b1 with:
# - SE(B0)^2 = (SD^2) * [(1/n) + ((xbar^2) / sum((xi - xbar)^2))]
# - SE(B1)^2 = (SD^2) / sum((xi-xbar)^2)
#  - note that SD should be pop. SD, but usually est. with sample SD
#  - call this RSE (Residual Standard Error): RSE = sqrt(RSS/(n-p-1))
#  - (dof = n-2 for basic model as p = 1 (for b1))
# - can then use these SE estimates to estimate significance
#  - e.g. t-statistic t(b1) = (b1-0) / SE(b1)... with dof = n-2
#
# accuracy of the model...
# - already know RSS: Residual Sum Squares: sum((yi - f(xi))^2)
#  - that describes unexplained variance in the model
# - we know know RSE: sqrt(RSS/(n-2))
#  - that estimates error even if the model parameters were estimated perfectly
#    (the '+ err' term that reflects noise that cannot be captured)
#  - if RSE is small relative to (say) mean of data, decent fit!
# - can also calc. R^2: 1 - (RSS/TSS) (TSS - Total Sum Squares; all var^2)
#  - R^2 is prop. var. in Y that can be explained with model on X
# - can also calc. F-statistic as estimate for whole model
#  - F = ((TSS-RSS)/p) / (RSS/(n-p-1)) (p: nvars, n: nobs)
#  - null hypothesis: b1=b2=...=bn=0, F > 1 suggests good pred.
#
# for categorical predictors can dummy code
# - need (numLevels-1) dummy variables to code a category
# can also code interactions
# - Y = b0 + b1X1 + b2X2 + b3X1X2 + err. would model X1-X2 interaction
#  - if an interaction is sig., include components even if not sig.
# can add polynomial regressors (b0X1 + b1X1^2) to capture limited non-linearity
#
# assumptions:
# - data is linear! (plot resid vs x or pred(x), look for patterns)
# - errors should be uncorrelated (error ei should not predict error ei+1)
#  - plot e.g. errors over time to look for trends (if t-series data)
# - errors should have constant variance (heteroscedasticity)
#  - funnel shape in residual plot, may be able to transform Y to fix
# - check for outliers (plot studentized resids. - resids/SE, check > 3)
# - check for unduly influential data points using a 'leverage statistic'
# - collinearity (ugh)
#  - check corr. matrix (but may not detect multicollinearity)
#  - can calculate VIF (variance inflation factor), >5-10 = issues!
#   - fix by dropping preds or combinging them (via z-scores, PCA etc)

# ============================================================================== 
# load in some data for sample lin. regr.

# load in necessary stuff
library(tidyverse)
library(ggthemes)
library(corrplot) # plot a correlation matrix
library(corrgram) # create and plot a correlation matrix
library(patchwork) # allows subplots
library(caTools) # split data into training/test

# set a seed for consistency
set.seed(101)

# read in data (making sure chr cols are factors)
# - reading in student data, want to predict grades (G1, G2, G3)
df <- read.csv('dataFiles/student-mat.csv', sep=';', stringsAsFactors=T)

# check for nulls (there aren't any!)
if (any(is.na(df))) {print('found nulls)')}

# ------------------------------------------------------------------------------ 
# explore data

# grab numeric cols and correlate them
col_isNum <- sapply(df,is.numeric)
df_corr <- cor(df[,col_isNum])
corrplot(df_corr, method = 'color') # print color coded corr. mat.

# alternative, corrgram can just plot without above fuss
corrgram(df, order=F, lower.panel=panel.shade, upper.panel=panel.cor,
         text.panel=panel.txt, outer.labels=list(left=T,top=T))

# lets create a histogram to explore grades
hFmt <- geom_histogram(bins=20, alpha=0.5, fill='blue')
pl1 <- ggplot(df, aes(x=G1)) + hFmt
pl2 <- ggplot(df, aes(x=G2)) + hFmt
pl3 <- ggplot(df, aes(x=G3)) + hFmt
print(pl1 + pl2 + pl3) # using patchwork package for subplots
# some weird things:
# - G1: two notable absences in middle of histogram
# - G2: whole bunch of peeps who scored 0
# - G3: as G2 but also a lot who scored a particular score

# ------------------------------------------------------------------------------ 
# lets build the model! (predicting G3)

# split data into train/test
df_sample <- sample.split(df$G3, SplitRatio=0.7) # randomly segment
df_train <- subset(df, df_sample == T) # select training data
df_test <- subset(df, df_sample == F) # select test data

# build the model (using all vars with .)
model <- lm(G3 ~ ., data=df_train)
print(summary(model))

# create a residual plot (using pred vs resid)
modelOut <- data.frame(pred=fitted(model), resid=residuals(model))
pl <- ggplot(modelOut, aes(x=pred, y=resid)) + geom_point()
print(pl)
# something weird going on, clear line in plot...

# print histogram
pl <- ggplot(modelOut, aes(res)) + hFmt
print(pl)
# note that it's predicting -ve values!

# can also just plot model!
# - get pred vs. resid (created earlier)
# - Q-Q plot for normality (and it ain't normal!)
# - pred vs. standardized resids (outliers)
# - leverage vs. standardized resids (influential points)
plot(model)

# ------------------------------------------------------------------------------ 
# get predictions from model

# predict test data
G3_pred <- predict(model,df_test)

# calculate MSE, RMSE
MSE <- mean((df_test$G3 - G3_pred)^2)
RMSE <- sqrt(mse)

# calculate R^2
SSE <- sum((G3_pred - df_test$G3)^2)
SST <- sum((mean(df_test$G3) - df_test$G3)^2)
R2 <- 1.0 - (SSE/SST)
