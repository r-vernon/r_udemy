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
# ------------------------------------------------------------------------------
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