
# load necessary packages
library(tidyverse)
library(corrplot)

# load in data files
df1 <- read_delim('./dataFiles/winequality-red.csv', delim=';')
df2 <- read_delim('./dataFiles/winequality-white.csv', delim=';')

# add labels and combine
df1$label <- factor('red')
df2$label <- factor('white')
df <- rbind(df1,df2)

# lets split off the label
dfLab <- df$label
df$label <- NULL

# check the means and SDs of the vars
dfM <- sapply(df,mean)
dfSD <- sapply(df,sd)
# very variable, will need to scale!

# lets test the SS choice for choosing K (although we'll be using K=2)
totSSE <- double(10)
inc <- 1
for (K in seq(1,10)) {
    dfClust <- kmeans(scale(df), centers=K, nstart=25)
    totSSE[inc] <- dfClust$tot.withinss
    inc <- inc +1
}
pl <- ggplot(data=NULL, aes(x=seq(1,10), y=totSSE)) + 
    geom_line() + geom_point()
print(pl)
# interestingly, 3 clusters looks more promising than 2...

# ------------------------------------------------------------------------------

# lets check correlations
dfCor <- cor(df)
corrplot(dfCor, method = 'color')

# exploratory pca
dfPC <- prcomp(df, scale=T) 
pcV <- dfPC$sdev^2
pl <- ggplot(data=NULL, aes(x=1:12, y=pcV/sum(pcV))) + 
    geom_line() + geom_point()
print(pl)
# no clear factor structure emerges!

# *maybe* 4 components, print loadings..
srtPC <- function (x) desc(abs(round(x,1)))
dfPCs <- as.data.frame(dfPC$rotation[,1:4])
dfPCs[abs(dfPCs) < 0.25] <- NA
dfPCs <- arrange(dfPCs,srtPC(PC1),srtPC(PC2),srtPC(PC3),srtPC(PC4)) # sort
dfPCs <- round(dfPCs,2)
dfPCs[is.na(dfPCs)] <- "-"
print(dfPCs)
# PC1 seems to be linked to sulfur (total sulfur dioxide)
# PC2 seems to be alcohol content/density (-ve)
# PC3 linked to acidity (fixed acidity, citric acid)
# PC4 a mishmash of the other 3, will ignore for now
# so sulfur, alcohol and acidity seem to be three key players!

# lets see if our wines vary on any of those dimensions
df_3PCs <- as.data.frame(dfPC$x[,1:3])

# histogram (freq. polygons) of PC1 - sulfury stuff
pl <- ggplot(df_3PCs, aes(PC1, after_stat(density), color=dfLab)) + 
    geom_freqpoly(bins=nclass.FD(df_3PCs$PC1)) + ggtitle('PC1')
print(pl)
# separates them incredibly well!

# histogram (freq. polygons) of PC2 - alcohol/density
pl <- ggplot(df_3PCs, aes(PC2, after_stat(density), color=dfLab)) + 
    geom_freqpoly(bins=nclass.FD(df_3PCs$PC2)) + ggtitle('PC2')
print(pl)
# overlap on this one pretty heavily

# histogram (freq. polygons) of PC3 - acidity
pl <- ggplot(df_3PCs, aes(PC3, after_stat(density), color=dfLab)) + 
    geom_freqpoly(bins=nclass.FD(df_3PCs$PC3)) + ggtitle('PC3')
print(pl)
# overlap here too - seems like sulfur is going to be the winner!

# ------------------------------------------------------------------------------
# lets run the model

dfClust <- kmeans(scale(df), centers=2, nstart=25)
table(dfClust$cluster,dfLab)
# does an incredibly good job!
# note that without scaling, it does pretty damn terribly...

# try it on the PCAs out of interest...
dfPCClust <- kmeans(df_3PCs, centers=2, nstart=25)
table(dfPCClust$cluster,dfLab)
# does slightly worse, extra data was helping it seems!

# out of interest, see how it allocates 3
df3Clust <- kmeans(scale(df), centers=3, nstart=25)
table(df3Clust$cluster,dfLab)
# red gets it's own cluster, white splits pretty neatly into two...
# maybe white + rose?