library(ggplot2)
library(data.table)
library(ggthemes)

# nice little histogram
pl <- ggplot(mpg,aes(x=hwy))  +
  geom_histogram(bins=20,fill='red',alpha=0.5)
print(pl)

# bar chart with factor colours
pl <- ggplot(mpg,aes(x=manufacturer)) + 
  geom_bar(aes(fill=factor(cyl)), position=position_stack(reverse=T)) + 
  theme_minimal()
print(pl)

# scatterplot
pl <- ggplot(txhousing,aes(x=sales,y=volume)) + 
  geom_point(color='blue',alpha=0.3,na.rm=T) + 
  geom_smooth(color='red',na.rm=T)
print(pl)

#-----------------------------
# economist graph generation

# read in the data (using fread from data.tables, dropping index col.)
df <- fread('dataFiles/Economist_Assignment_Data.csv',drop=1)

# create the basic plot
pl <- ggplot(df, aes(x=CPI, y=HDI, color=Region))
  
# add geometry
pl <- pl + geom_point(shape=1, size=4)

# add a trendline
# - group=1: data to use for line
# - lm: linear model
# - se: no confidence interval
pl <- pl + geom_smooth(aes(group=1), method=lm, se=F, color='red', 
                       formula='y~log(x)')

# add data labels to select countries
pointsToLabel <- c('Russia', 'Venezuela', 'Iraq', 'Myanmar', 'Sudan',
                   'Afghanistan', 'Congo', 'Greece', 'Argentina', 'Brazil',
                   'India', 'Italy', 'China', 'South Africa', 'Spane',
                   'Botswana', 'Cape Verde', 'Bhutan', 'Rwanda', 'France',
                   'United States', 'Germany', 'Britain', 'Barbados', 'Norway', 
                   'Japan', 'New Zealand', 'Singapore')
pl <- pl + geom_text(aes(label=Country), color='gray20', check_overlap=T,
                       data=subset(df, Country %in% pointsToLabel))

# add a theme
pl <- pl + theme_economist_white()

# set scaling
xlabel <- 'Corruption Perception Index, 2011 (10=least corrupt)'
ylabel <- 'Human Development Index, 2011 (1=Best)'
pl <- pl + scale_x_continuous(name=xlabel, limits=c(1,10), breaks=1:10)
pl <- pl + scale_y_continuous(name=ylabel, limits=c(0.2,1.0), 
                              breaks=seq(0.2,1.0,0.1))

# add a title
pl <- pl + ggtitle('Corruption and Human development')

# print the plot
print(pl)