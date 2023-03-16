
# NOTES FOR K-MEANS CLUSTERING
# ----------------------------
#
# Simple algorithm:
# - choose num. clusters K
# - randomly assign each item to cluster 1:K
# - compute centroid of each cluster
# - reassign each item to cluster with closest centroid
#  - repeat until clusters stabilise
# (this is the naive algorithm, much better implementations available!)
#
# Note that this will only find a local optimum
# - need to re-run with different starting allocations
#
# can choose K by plotting K against SSE (sum squared errors)
# - in this case SSE is distance from point to cluster centroid
# - as K increases, SSE will naturally diminish
# - however if at a certain point it decreases suddenly, suggests good solution
#
# Hierarchical clustering is an alternative that doesn't require specific K
# - builds a dendogram (like a tree) where similar items bunch together
# - longer the branches, the greater the separation between items
# algorithm fairly simple
# - group the two closest items on some distance metric (euclidian etc)
#  - that group is now considered a single item, joined on the dendogram
# - find the next closest pair and so on
# - multiple ways to compare an item to a group (linkage function)
#  - compare to average, to single item etc
# - similarly many different distance functions, can have profound impact!
#
# note that variable scaling should be considered carefully, as could have 
# large impact on the resultant clusters!
#
# clusters can also be sensitive to small perturbations in the data
# repeat clustering with subsets to determine cluster stability

# ============================================================================== 
# run through an example

library(tidyverse)
library(ISLR2)
library(cluster) # for exploring clusters

set.seed(101)

# use iris dataset
df <- iris

# split out the label column, as wouldn't normally have it
dfLabel <- df$Species
df$Species <- NULL

# plot the data
pl <- ggplot(df, aes(Petal.Length, Petal.Width, color=dfLabel)) + 
    geom_point()
print(pl)

# note that iris data is very correlated
# - lets see if we can get a look with pca
# - not scaling as in similar units already!
df_pc <- prcomp(df)
print(df_pc$sdev^2 / sum(df_pc$sdev^2))
# 92.5% var in first component! (5.3% in second)
# plot with first two components
pl <- ggplot(as.data.frame(df_pc$x), aes(x=PC1, y=PC2, color=dfLabel)) + 
    geom_point()
print(pl)
# actually looks like first component separates them nicely!

# lets test the SS choice for choosing K
totSSE <- double(10)
inc <- 1
for (K in seq(1,10)) {
    dfClust <- kmeans(df, centers=K, nstart=25)
    totSSE[inc] <- dfClust$tot.withinss
    inc <- inc +1
}
pl <- ggplot(data=NULL, aes(x=seq(1,10), y=totSSE)) + 
    geom_line() + geom_point()
print(pl)
# shockingly, shows 3 likely best solution...

# run the kmeans algorithm
dfClust <- kmeans(df, centers=3, nstart=25)
print(dfClust)

# compare to reality
table(dfClust$cluster,dfLabel)

# print the clusters
clusplot(df, dfClust$cluster, color=T, shade=T, labels=0, lines=0)

# out of interest, try it with scaled variables
dfScClust <- kmeans(scale(df), centers=3, nstart=25)
table(dfScClust$cluster,dfLabel)
clusplot(df, dfScClust$cluster, color=T, shade=T, labels=0, lines=0)
# actually seemed to do slightly worse!

# and lets see how it does with the principle components
dfPcClust <- kmeans(df_pc$x[,1:2], centers=3, nstart=25)
table(dfPcClust$cluster,dfLabel)
clusplot(df, dfPcClust$cluster, color=T, shade=T, labels=0, lines=0)
# about on par, ever so slightly worse...