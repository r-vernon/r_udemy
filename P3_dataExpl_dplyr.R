# will be using dplyr and nycflights13
library(dplyr)
library(nycflights13)

# replaces stats filter package with filter for dataframes
# - readable filtering!
head(filter(flights,month==11,day==3,carrier=='AA'))
# alternative would be...
head(flights[flights$month==11 & flights$day==3 & flights$carrier=='AA',])

# has slice for selecting certain rows, select for cols
slice(flights,1:3) # rows 1-3
head(select(flights,carrier,arr_time))

# can sample random rows with slice_sample
slice_sample(flights,n=10) # 10 random rows
slice_sample(flights,prop=0.1) # random 10%

# can sort dataframes with arrange
head(arrange(flights,year,month,day,arr_time)) # all asc.
head(arrange(flights,year,month,day,desc(arr_time))) # with arr_time desc.

# can rename columns
# rename(flights,new_name=old_name)

# can get unique items from columns
distinct(select(flights,carrier)) # distinct carriers

# can create new columns based off existing ones
# - mutate adds column, transmute just returns it
head(mutate(flights,new_col = arr_delay-dep_delay))
head(transmute(flights,new_col = arr_delay-dep_delay))

# can get summary stats from dataframe
summarise(flights,avg_air_time=mean(air_time,na.rm=T))