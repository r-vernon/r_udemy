# using tidyr and data.table (extension of data frames)
library(tidyr)
library(data.table)

# make dummy dataframe
df <- data.frame(Comp=rep(1:3,c(3,3,3)), 
                 Year=c(1998,1999,2000,1998,1999,2000,1998,1999,2000),
                 Qtr1=runif(9,0,100), Qtr2=runif(9,0,100), 
                 Qtr3=runif(9,0,100), Qtr4=runif(9,0,100))

# can reshape so Qtr1-Qtr4 all in one column (Quarter) not 4, and
# their values are in subsequent column (Revenue)
# - resulting df cols: Comp Year Quarter Revenue
# OLD: df_byQtr <- gather(df,key='Quarter',value='Revenue',Qtr1,Qtr2,Qtr3,Qtr4)
# NEW: more flexible version of gather:
df_byQtr <- pivot_longer(df, c(Qtr1,Qtr2,Qtr3,Qtr4), 
                         names_to='Quarter', values_to='Revenue')

# new dummy data frame
df <- data.frame(time=as.Date('2009-01-01')+0:9, X=rnorm(10,0,1),
                 Y=rnorm(10,0,1), Z=rnorm(10,0,1))
# gather it
df <- pivot_longer(df,c('X','Y','Z'),names_to='stock',values_to='price')
# undo the gathering (newer version of spread)
df <- pivot_wider(df,names_from='stock', values_from='price')

#--------------------------------------

# can separate columns based on position or delim (or regex)
# fcns: separate_wider_position, separate_wider_delim
# superceding old 'seperate' fcn

# new dummy data frame
df <- data.frame(newCol1=c(1,2,3,4), newCol2=c(NA,'a.x','b.y','c.z'))

# separate it by delimiter
df <- separate_wider_delim(df,newCol2,'.',names=c('newCol2','newCol3')
                           )
# rejoin 'em
df <- unite(df,'newCol2','newCol2','newCol3',sep='.')
