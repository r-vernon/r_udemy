library(ggplot2)
library(plotly)

# create basic ggplot (scatter using mtcars)
pl <- ggplot(mtcars,aes(mpg,wt)) + geom_point()

# convert to plotly graph
gpl <- ggplotly(pl)

# print the result
print(gpl)