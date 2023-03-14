
# NOTES FOR SUPPORT VECTOR MACHINES (SVMs)
# ----------------------------------------
#
# For any p-dimensional space, can define a hyperplane:
# - b0 + b1X1 + ... + bpXp = 0
#  - with 2 parameters, b0+b1X1+b2X2=0, it defines a line, with 3 a flat plane
# - For any point in that space:
#  - if X satisfies the equation f(X) it lies on the hyperplane
#  - if f(X) < 0 or f(X) > 0, it lies on one side or the other
# - the hyperplane then, can be used as a binary classifier!
#  - can also use distance from the hyperplane as a measure of confidence
#
# most problems though, will not be perfectly linearly separable by a plane
# solve with support vector *classifier* (n obs. of p params.)
# - define some tolerance C such that the sum of all classification errors e
#   sum(ei for i=1:n) are <= C (and all ei >= 0)
#   - if C = 0, that means all points *must* be classified correctly
# - then maximise M in yi(b0 + b1xi1 + ... + bpxip) >= M(1-ei)
#  - if ei > 1, it's on wrong side of hyperplane
#  - yi is just 1/-1 to set side of hyperplane (above/below)
#  - betas (b1...bp) are further constrained to squared sum to 1
#    (that way equation yi(...) gives perp. distance of point xi to hyperplane)
#  - therefore M defines the margin between hyperplane and nearest points
#   - however by allowing M(1-ei), some points will be allowed to cross over
# - choose C with cross-validation
#  - as ei>1 means misclassification, at most C points can be incorrect
#
# - only points that either lie on the margin (M) of the hyperplane, or violate 
#   it affect it's position, i.e. it's only tuned by points near the decision
#   boundary
# - that makes it robust to outliers
#
# decision boundaries though, are not always linear
# address this with support vector *machines* (extension of the classifier)
# - details complicated, but rather than relying on linear weightings, can 
#   replace those weightings with some kernel (e.g. polynomial, radial etc)
# - that allows much more flexibility and non-linear fitting!
#  - a support vector classifier is a SVM with polynomial kernel of degree 1
#
# SVMs don't extend nicely to multiple classes (>2)
# - can (e.g.) run K SVMs for K classes, each comparing one class to all others
# - assign item to class where distance from hyperplane was maximal
#
# the actual name, support vector, comes from the vector points that lie on
# the margin of the hyperplane splitting the categories

# ============================================================================== 
# run through an example

# load necessary libraries
library(tidyverse)
library(ISLR2)
library(e1071) # for SVMs

# load data
df <- iris

# create the model
model <- svm(Species ~ ., data=df)
summary(model)

# test it's output
modelPred <- predict(model, df[,-5])
table(modelPred, df[,5])

# ------------------------------------------------------------------------------ 
# tune a model
# - cost: tolerance for errors, less likely to overfit at cost of mistakes
# - gamma: property of some kernel functions, large gamma = high bias/low 
#          variance models (less sensitive to data/imposes model) and vice versa

# use tune function
costRng <- exp(seq(log(1),log(5),length.out=15))
gamRng <- exp(seq(log(0.01),log(2),length.out=15))
tuneResults <- tune(svm, train.x=iris[,-5], train.y=iris[,5], kernel='radial', 
                    ranges=list(cost=costRng, gamma=gamRng))
summary(tuneResults)
tuneCost <- tuneResults$best.parameters$cost   # 3.16
tuneGamma <- tuneResults$best.parameters$gamma # 0.07

# use tuned params
tuneModel <- svm(Species ~ ., data=df, kernel='radial', 
                 cost=tuneCost, gamma=tuneGamma)
tuneModelPred <- predict(tuneModel, df[,-5])
table(tuneModelPred, df[,5])
# actually about the same performance on this small dataset :p