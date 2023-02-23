
# reference card:
# https://cran.r-project.org/doc/contrib/Short-refcard.pdf

# math functions
abs() # absolute val
sum() # summed vals
mean() # arith. mean
round() # rounds values - round(var,digits=prec)

#----------------------------------------

# date functions
curDate <- Sys.Date() # returns current YYYY-MM-DD as Date class

# assigning dates with non-standard formats
# %d - day as 01
# %m %b %B - month as 01, Jan, January
# %y %Y - year as 90, 1990
# - for more see help(strptime)
newDate <- as.Date('Nov-03-90',format='%b-%d-%y')

# convert times to timestamps
# - POSIXct - stores as time (seconds) since epoch (01/01/1970)
# - POSIXlt - stores time as list with day, month, etc
#  - former better for storage and computation
#  - latter better for extracting individual parts (e.g. month)
# - will assume missing info (e.g. current date) if not given
# - for formats see help(strptime)
tStamp <- as.POSIXct('11:02:03',format='%H:%M:%S')
tStamp <- as.POSIXlt('11:02:03',format='%H:%M:%S')

# more likely to use strptime than above
# - however, it converts to POSIXlt format
tStamp <- strptime('11:02:03',format='%H:%M:%S')

#----------------------------------------

# sequences
seq(stPt,endPt,by=inc)

# sorting
# - sorts alphabetically too, ignoring case
sort(var,decreasing = F/T)

# reverse object
rev(var)

# structure of an object
str(var)

# append to object (vectors/lists)
append(vars,newVars)

#----------------------------------------

# checking things
is.[...] # can check all sortsa shit
is.vector(), is.integer(), is.na() # etc

# converting to things
as.[...]
as.list(), as.matrix(), as.double() # etc

#----------------------------------------

# applying functions to elems in list/vector
# - returns list (list apply)
fcn_on_vec <- lapply(vec,fcn)

# want a vector/matrix back? use sapply (wrapper for lapply)
fcn_on_vec <- sapply(vec,fcn)

# using anonymous functions
sapply(vec,function(var){myCode})

# multiple arguments
fcn <- function(var1,var2,...,varN){myCode}
sapply(vec,fcn,var2=var2_val,...,varN=varN_val)

#----------------------------------------

# regex (grep - global reg. expr. print)
# - grep returns index
# - grepl returns logical
# - sub replaces first match
# - gsub replaces all matches
# - there's more, see help(grep)
vec <- c('a','b','c','c','cc','d')
grep('c',vec) # returns 3 4 5
grepl('c',vec) # returns F F T T T F
sub('c','C',vec) # returns "a"  "b"  "C"  "C"  "Cc" "d"
gsub('c','C',vec) # returns "a"  "b"  "C"  "C"  "CC" "d"
