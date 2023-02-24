library(ggplot2)
library(ggplot2movies)
library(ggthemes)

# ggplot2 layers:
# - data: data to plot
# - aesthetics: how should data be mapped to graph (x,y etc)
# - geometries: visual elements for data (graph type etc)
# - facets: think subplots
# - statistics: overlays that show sum. stats. of data
# - coordinates: space for data plot
# - themes: non-data styling

#-----------------------------
# histograms

# data and (aes)thetics
pl <- ggplot(movies,aes(x=rating))

# add geometry
pl <- pl + geom_histogram(binwidth=0.2,color='black',
                          fill='dodgerblue3',alpha=0.5)
# add some labels
pl <- pl + xlab('Movie Rating') + ylab('Count')

# add a title
pl <- pl + ggtitle('Movie Rating Histogram')

# show the result
print(pl)

#-----------------------------
# scatterplots

# data and aesthetics
pl1 <- ggplot(mtcars,aes(x=wt,y=mpg))
pl2 <- pl1

# add geometry
pl1 <- pl1 + geom_point(size=5,aes(shape=factor(cyl),
                                 color=factor(cyl)))

# alternative to show gradient shading
pl2 <- pl2 + geom_point(aes(colour=hp),size=5) +
  scale_colour_gradient(high='red',low='blue')

# show the results
print(pl1)
print(pl2)

#-----------------------------
# barplots

# data and aesthetics
pl <- ggplot(mpg,aes(x=class))

# add geometry
pl <- pl + geom_bar(aes(fill=drv))

# show the results
print(pl)

#-----------------------------
# boxplots

# data and aesthetics
pl <- ggplot(mtcars,aes(x=factor(cyl),y=mpg))

# add geometry
pl <- pl + geom_boxplot(aes(fill=factor(cyl)))

# show the results
print(pl)

#-----------------------------
# 2D plotting

# data and aesthetics
pl <- ggplot(movies,aes(x=year,y=rating))

# add geometry (3 versions)
pl1 <- pl + geom_bin2d(binwidth=c(3,1)) 
pl2 <- pl + geom_hex() 
pl3 <- pl + geom_density2d() 

# add colour mapping to pl1 and pl2
pl1 <- pl1 + scale_fill_gradient(high='red',low='yellow')
pl2 <- pl2 + scale_fill_viridis_c(option='plasma') 

# show the results
print(pl1)
print(pl2)
print(pl3)

#-----------------------------
# coords 

# data, aesthetics, and geom
pl <- ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()

# add coordinate space or set fixed ratio
pl1 <- pl + coord_cartesian(xlim=c(1,4),ylim=c(15,30))
pl2 <- pl + coord_fixed(ratio=1/4)

# show the results
print(pl1)
print(pl2)

#-----------------------------
# faceting (subplots) 

# data, aesthetics, and geom
pl <- ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()

# set up faceting
# - syntax: y_facet ~ x_facet
pl1 <- pl + facet_grid(. ~ cyl)
pl2 <- pl + facet_grid(drv ~ .)

# show the results
print(pl1)
print(pl2)

#-----------------------------
# themes

# data, aesthetics and geom
pl <- ggplot(mtcars,aes(x=wt,y=mpg)) + geom_point()

# add a theme (more themes available from ggthemes)
pl1 <- pl  + theme_minimal()
pl2 <- pl  + theme_economist()
#theme_set(theme_minimal()) # would set theme for all plots

# show the results
print(pl1)
print(pl2)