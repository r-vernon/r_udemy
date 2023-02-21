# quick guide to the basics

# create a vector and name it
v1 <- c(1,2,3,4)
names(v1) <- c('a','b','c','d')

# basic vector operations
sum_v1 <- sum(v1)
mean_v1 <- mean(v1)
prod_v1 <- prod(v1) # 1*2*3*4
odd_v1 <- v1 %% 2   # v1 modulus 2
one_v1 <- v1 == 1   # where v1 equals 1

# vector combinations
v2 <- c(5,6,7,8)
sum_v1v2 <- v1 + v2
mult_v1v2 <- v1 * v2

# indexing (starts at 1)
elem1_v1 <- v1[1]   # using index location of elem1
elem2_v1 <- v1['b'] # using name of elem2
slice_v1 <- v1[c(2,3)] # 2nd and 3rd elems
gtr2_v1 <- v1[v1 > 2]
rev_v1 <- v1[c(length(v1):1)]
