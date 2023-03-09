
# NOTES FOR TREE METHODS
# ----------------------
#  
# Tree methods segment predictor space into simple regions
# - segmentation splitting rules can be described with a tree structure
# - example methods include: bagging, random forests, boosting
# - they produce multiple trees that combined, yield a consensus prediction
# 
# For example, predicting academic salary based on years spent and # papers
# - early on years spent likely matters as more
# - later, # papers will start to have more impact
#  - can split regression (or other model) to account for this!
#
# Two main steps:
# - split predictor space into distinct (non-overlapping) regions
# - treat all values in a given region as a homogonous group, i.e. we make
#   the same prediction (the mean of response values, not individual ones)
#
# To split predictor space:
# - find predictor and corresponding cutpoint that minimises RSS
# - i.e. if the two regions are R1 and R2, minimise:
#  - sum((Y - mean(R1_predY))^2) + sum((Y - mean(R2_predY))^2)
# - repeat process, this time splitting one of the two regions R1, R2
# - continue until some stopping criteria reached
#
# This may lead to some overfitting, so can then prune the tree if necessary
# - e.g. can add cost parameter to additional nodes to penalise complexity
# - let Tn be number of terminal nodes of tree, a be cost fn. param.
#  - minimise: Sum[i=1:Tn](Sum[j=R1:Rn]((Y - mean(Rj_predY))^2) + aTn)
# - when a=0, returns original tree, as a increases prefer less complexity
# To summarise the algorithm:
# - build complex tree as described above
# - apply cost complexity pruning (using K-Fold X-Val to determine cost, a)
# - return sub-tree that best fits chosen complexity
#
# Can also apply the whole process to classification, not just regression
# - rather than assigning avg. value from region, assign most frequent class
# - use Gini index to calculate performance (look it up)
#
# Merits of trees over linear approaches:
# + easy to explain/intuitive (may mirror human decision making)
# + can be displayed graphically
# + can handle qualitative predictors without dummy variables
# - generally don't have same predictive accuracy as other approaches
# - not as robust, small change in data may cause large change in tree
# can address some disadvantages with bagging/random forests etc
#
# bagging
# - decision trees are high variance - sensitive to small changes in data
# - one way to reduce variance is average multiple values together
# - bagging uses bootstrapping to make many predictions, averaging them together
#  - don't prune the trees, so each variable, but low bias (few assumptions) 
#  - for classification, can take majority vote
#  - can estimate performance based on responses left out of the 'bag'
# - does reduce interpretability, but can estimate pred. importance
#
# random forests
# - improve bagging by decorrelating trees
# - when building trees, split on random selection of predictors, not all
#  - typically choose sqrt(nPred) as number of predictors to choose
# - otherwise, a strong predictor may dominate in bagging
#  - that would mean most trees would be similar, defeating the point!
# 
# boosting
# - rather than multiple, random trees, create sequential, incremental trees
# - algorithm:
#  - set f(x)=0 and residuals ri=yi for all i in training set
#  - for boost b=1:B...
#   - fit tree with d splits using data (X,r) giving prediction f_b(x)
#   - updated f(x) with shrunken version of new tree: f(x) = f(x) + a*f_b(x)
#   - update residuals: ri = ri - a*f_b(x) (which will be used for next tree)
#  - f(x) will be final boosted model: f(x) = sum[b=1:B](a*f_b(x))
# - by fitting tree to residuals rather than outcome Y, model slowly learns
# - can use small trees (based on d), and due to residuals will focus on areas 
#   where it previously underperformed
# - depends on three parameters:
#  - num. trees, B (use x-val to choose, too large can cause overfitting)
#  - shrinkage param. a (typically 0.01, 0.001), controls pace of learning
#  - num. splits, d (even d=1 often works well!)



# ============================================================================== 
# load in some data for 