# create a data frame (made up weather data)
days <- c('Mon','Tue','Wed','Thu','Fri')
temp <- c(22.2,21,23,24.3,25)
rain <- c(TRUE, TRUE, FALSE, FALSE, TRUE)
df <- data.frame(days,temp,rain)

# exploring data frame
head(df) # top 6 rows
head(df,1) # top row
tail(df) # bottom 6 rows
str(df) # structure of data frame
summary(df) # summary statistics for cols. of data frame

# indexing data frame
df_row1 <- df[1,] # row 1 all cols
df_rain <- df[,'rain'] # all rows from rain column
df_rain <- df$rain # alternative way 
df_rain_df <- df['rain'] # returns rain column but as data frame
df_elem <- df[[1,1]] # double brackets for single element(s)
# - typeof(df['temp']) returns *list* type
# - typeof(df[['temp']]) returns *double* type
df[-2,] # all but 2nd row

# subset of data frame
# NOT RECOMMENDED IN HELP - USE STD INDEXING ([])
df_rainyDays <- subset(df,subset = rain==T)

# sorting data frame
sortIdx <- order(df['temp'],decreasing=F)
df_tempOrder <- df[sortIdx,] # data frame sorted by temp

#---------------------------------------------------------------
# data frame operations

# create new data frame
c1 <- c(1:10)
c2 <- letters[1:10] # a, b, c...
df <- data.frame(myCol1=c1, myCol2=c2)

# number of rows/columns, names of columns
nrow(df)
ncol(df)
colnames(df)

# add elements
newRow <- data.frame(myCol1=11, myCol2=letters[11])
df <- rbind(df,newRow)
newCol <- 2*df$myCol1
df$mycol3 <- newCol
# can also use...
# - df[,'myCol3']

# oops typo, rename column
colnames(df)[3] <- 'myCol3'


