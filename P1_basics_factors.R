# non-numerical matrices (factors)

# create a vector of cats ('c') and dogs ('d') with ID
animal <- c('d','c','d','c','c')
id <- c(1,2,3,4,5)
factAnimal <- factor(animal)
# works out that there's two (nominal) levels (c, d)

# ordinal factors (ordered data)
temps <- c('cold','med','hot','hot','hot','cold','med')
factTemps <- factor(temps,ordered=T,levels=c('cold','med','hot'))
# now levels listed and have ordering (cold < med < hot)