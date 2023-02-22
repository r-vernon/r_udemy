# create a matrix
m1 <- matrix(1:10)
m2 <- matrix(m1,nrow=2) # reshape with 2 rows (so 5 cols auto.)
m3 <- matrix(m1,nrow=2,byrow=T) # reshape along rows rather than (def.) cols
# - m1 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] (10x1)
# - m2 = [1,3,5,7,9; 2,4,6,8,10] (2x5)
# - m3 = [1,2,3,4,5; 6,7,8,9,10] (2x5)

# name the matrix rows and columns
rownames(m2) <- c('r1','r2')
colnames(m2) <- c('c1','c2','c3','c4','c5')

# matrix math/logicals works like vector math
# - m1*2, m1^2, 2/m1, m1>2 etc
# - m1*m2 will be *element-wise*
#  - for matrix multiplication, use m1 %*% m2 (with compat. matrices)
# - sum/mean etc (mean(m2)) work on entire matrix

# operations on rows/cols
# - colSums(m2) will sum columns (rowSums for rows)
# - got colMeans, rowMeans and others

# can add to matrices
c6 <- c(11,12) # new col. (col6) for m2
r3 <- c(11:15) # new row (row 3) for m3
m2 <- cbind(m2,c6) # col. bind
m3 <- rbind(m3,r3) # row bind
# note that row or column bound will take name of variable being added

# matrix indexing
m2_row1 <- m2[1,] # first row, all cols
m2_col1 <- m2[,1] # all rows, first col.
m2_elem <- m2[2,3] # element from row 2, col. 3
m2_cols <- m2[,c(1,3)] # all rows, col. 1 & 3