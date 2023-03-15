# softImpute package implements this efficiently!

# from what I understand
# - fill missing data with mean values, compute pca
# - use a number of those components to predict missing values
#  - not all as would overfit!
# - repeat until some accuracy threshold met based on known data

# load the data, scaling it (M=0, SD=1)
X <- data.matrix(scale(USArrests))

# ------------------------------------------------------------------------------
# remove some values

nomit <- 20 # num to omit
set.seed(15) # replicate book example
ina <- sample(seq(50), nomit) # rows to omit values from
inb <- sample(1:4, nomit, replace=T) # cols to replace values from
Xna <- X # matrix to contain missing vals
index.na <- cbind(ina, inb) # combined index of missing value locations
Xna[index.na] <- NA # removing the missing values

# ------------------------------------------------------------------------------
# create a function that will approximate a matrix using pca
# X is matrix to approximate
# M is number of components to use for approximation
# %*% is matrix multiplication
# drop=F just means dataframes/matrices aren't converted to vectors if 1 col.

fit.svd <- function(X, M=1) {
    svdob <- svd(X)
    with(svdob,
         u[, 1:M, drop=F] %*% (d[1:M] * t(v[, 1:M, drop=F]))
         )
}

# alternative using prcomp
fit.pc <- function(X, M=1) {
    pcob <- prcomp(X)
    
    # recreate data using M components
    with(pcob,
         t(t(x[, 1:M, drop=F] %*% t(rotation[, 1:M, drop=F])) + center)
         )
}

# ------------------------------------------------------------------------------
# create a version of X where missing vals replaced with col. means

Xhat <- Xna # copy mat. with missing vals.
xbar <- colMeans(Xna, na.rm=T) # get col. means ignoring NAs
Xhat[index.na] <- xbar[inb] # replace NAs with col. means

# ------------------------------------------------------------------------------
# setup steps to run the completion

thresh <- 1e-7 # threshold to stop
rel_err <- 1 # when rel_err below threshold, stop
iter <- 0

# find all missing elements
ismiss <- is.na(Xna) # logical same size as Xna

# mean squared error of non-missing elements of old/orig version of Xhat
mssold <- mean((scale(Xna, center=xbar, scale=F)[!ismiss])^2)

# mean of squared non-missing elements
mss0 <- mean(Xna[!ismiss]^2)

# ------------------------------------------------------------------------------
# run the algorithm

while (rel_err > thresh) {
    iter <- iter +1
    
    # just in case something broke...
    if (iter>20) {break}
    
    # approximate the matrix with PCA (1 component; M=1)
    Xapp <- fit.pc(Xhat, M=1)
    
    # update old missing values (currently col. means) with approximated ones
    Xhat[ismiss] <- Xapp[ismiss]
    
    # calculate relative error
    mss <- mean(((Xna - Xapp)[!ismiss])^2)
    rel_err <- (mssold - mss) / mss0
    
    # update mssold for next iter
    mssold <- mss
    
    # concatenate and print progress
    cat('Iter:', iter, 'MSS:', mss, 'Rel. Err:', rel_err, '\n')
}

# check algorithm performance against actual values
cor(Xapp[ismiss], X[ismiss])
