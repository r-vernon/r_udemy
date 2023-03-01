# NOTES FOR LOGISTIC REGRESSION
# -----------------------------
#
# linear regression assumes Y is qualitative, logistic works for categorical Y
# - may be able to use lin. reg. for binary cats (0, 1) but not 3+ cats
# - however, it's outcome estimates may not map neatly to probabilities
# - logistic regression better!
#
# logistic function: p(X) = (e^(b0 + b1X)) / (1 + e^(b0+b1X))
# - can show: p(X)/(1 - p(X)) = e^(b0 + b1X)
# - take logs: log(p(X)/(1 - p(X))) = b0 + b1X
#  - left-hand side called 'logit', linear in X
#
# brief intro to 'maximum likelihood' (l)
# - aim to find b0 and b1 such that predicted probability p(x) is as close to
#   each individuals actual status as possible
#  - l(b0,b1) = prod[i in yi=1](p(xi)) * prod[i in yi=0](1-p(xi))
# - use likelihood instead of least squares for logistic regression
#
# as with lin. regr. can dummy code categorical variables
# can extend naturally to multiple logistic regression
# can even predict multiple categories using multinomial logistic regression
# - say we have K cases, ranging k = 1,2,...,K
# - let Kth class act as baseline (arbitrary choice), can show:
#  - log(P(Y=k|X=x) / P(Y=K|X=x)) = bk0 + bk1x1 + ... + bkxn (sim. line 10)
# - so when classifying cases A, B, C, need two models:
#  - model1: let A be baseline, predict B and C
#  - model2: let B be baseline, predict A and C
#  - coefs will vary (e.g. for C in both models) but preds should be same
#
# Alternative approaches using bayesian statistics
# - LET Ok be Overall (prior) probability that random x comes from kth class
#  - i.e. [6xA; 4xB] OA will be 60%, OB will be 40%
# - LET fk(x) = p(x|Y = k) (density function)
#  - i.e. if we know it's class k=A, what's the prob. of getting value X
# Bayes theorem: p(A|B) = (p(A)*p(B|A)) / p(B)
# - we want p(Y=k|X=x), prob of each class k given each value of x
# - p(A) = p(Y=k) = Ok
# - p(B|A) = fk(x) = p(x|Y = k)
# - p(B) = p(X=x) = sum[i=1:K](Oi * fi(x)) ... i.e. sum(p(Y-i) * p(x|Y=i))
#  - so P(Y=k|X=x) = (Ok * fk(x)) / sum[i=1:K](Oi * fi(x))
# - Ok is easy to estimate from sample (frequencies), fk(x) is hard!
# - how to estimate fk(x)...
#
# linear discriminant analysis (LDA)
# - assumes fk(x) is normally distributed (defined with mean + SD)
# - assumes variances equal (common) across classes
# - estimates Ok from frequencies in sample
# - estimates mean for each class and SD calculated across all classes
#  - math simplifies to: 
#     fk(x) = x*(xbar_k / SD^2) - (xbar_k^2 / 2*SD^2) + log(Ok)
#  - pick k where fk(x) is highest!
# - can extend with multivariate normal distribution (assume common cov. mat)
#
# quadratic discriminant analysis (QDA)
# - like above but assigns each class it's own variance/covariance estimate
#  - however that's a lot of parameters to estimate! 
#  - only use with lots of data, or where shared variance is clearly false
#
# naive bayes
# - make one assumption: within kth class, the p predictors are independent
#  - for LDA had to estimate relations between each pred (off-diag in covmat)
#  - these are normally v. hard to estimate without lots of data!
#  - assuming there is no relationship simplifies things and often works well!
# - mathematically: fk(x) = fk1(x1) * fk2(x2) * ... * fkp(xp) for p preds
# - to estimate fkp(xp) for each predictor p, can...
#  - assume normal distribution, like QDA but covmat off-diag = 0
#  - use a histogram or kernel density estimator 
#  - if categorical predictor, use proportions
#
# when to use each...
# - logistic + LDA assume linear decision boundaries
# - LDA assumes each class is normally distributed
#  - likely better than logistic when that holds, worse if not
# - QDA may outperform LDA with lots of data, but could overfit
#  - only use with lots of data, or when variances clearly differ across classes
# - naive going to be best when lots of parameters or small num. observations
#  - aka insufficient data to make fine-tuned fk(x) estimates

# ==============================================================================
# load in some data for logistic regression

# load in necessary stuff
library(tidyverse)
library(patchwork) # allows subplots
library(Amelia) # identify missing data
library(corrplot)
library(caTools)

set.seed(101)

# using titanic data set from kaggle: https://www.kaggle.com/c/titanic
# - PassengerID: ID of passenger
# - Survived: boolean surived (1) or not (0)
# - Pclass: passenger class
# - Name: name of passenger
# - Sex: male or female
# - Age: age of passenger
# - SibSp: number of siblings/spouses passengers had
# - Parch: number of parents/children passengers had
# - Ticket: ticket code
# - Fare: cost of ticket
# - Cabin: cabin ID
# - Embarked: location where embarked
df_train <- read_csv('dataFiles/titanic_train.csv', col_types='ilfcfdiicdcf')

# check for missing data
missmap(df_train, main='Missing Map', col=c('red','black'), legend=F)

# create categorical for SibSp and Parch (0, 1, 2+)
df_train <- mutate(df_train, SibSp2=factor(case_when(
  SibSp==0~'0', SibSp==1~'1', SibSp>1~'2+')), .after=SibSp)
df_train <- mutate(df_train, Parch2=factor(case_when(
  Parch==0~'0', Parch==1~'1', Parch>1~'2+')), .after=Parch)

# ------------------------------------------------------------------------------
# plot data

# survived vs gender (more males on females, but more females survived!)
pl <- ggplot(df_train, aes(Sex)) + geom_bar(aes(fill=Survived))
print(pl)

# survived vs class (survival fairly equal across classes)
pl <- ggplot(df_train, aes(Pclass)) + geom_bar(aes(fill=Survived))
print(pl)

# survival vs age histogram
pl <- ggplot(df_train, aes(Age)) + geom_histogram(bins=20, aes(fill=Survived))
print(pl)

# ------------------------------------------------------------------------------
# clean up data

# lots of NA in age... fix with model (using class + gender + sib/sp)
predAge <- lm(Age ~ factor(Pclass) + Sex + SibSp, 
              data=df_train, na.action=na.omit)
df_train <- mutate(df_train, 
                   Age2=coalesce(Age, predict(predAge,df_train)), .after=Age)

# ------------------------------------------------------------------------------
# build the model

# drop vars we don't want
df_train <- select(df_train, -PassengerId, -Name, -Age, -SibSp, -Parch, -Ticket, -Cabin)

# create model
logModel <- glm(Survived ~ ., family=binomial(link='logit'), data=df_train)
summary(logModel)

# test model by splitting data
split <- sample.split(df_train$Survived, SplitRatio=0.7)
df1 <- subset(df_train, split==T)
df2 <- subset(df_train, split==F)
m1 <- glm(Survived ~ ., family=binomial(link='logit'), data=df1)
m1_prob <- predict(m1, df2, type='response')
m1_results <- ifelse(m1_prob>0.5, 1, 0)
misClassErr <- mean(m1_results != df2$Survived)
print(1-misClassErr)
