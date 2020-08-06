#mymovies
mymovies <- read.csv(file.choose())
View(mymovies)
library(arules)
library(arulesViz)
class(mymovies)
movies_rules <- apriori(mymovies,parameter = list(support=0.002,confidence=0.05,minlen=3))
movies_rules
plot(movies_rules,method = "scatterplot")
plot(movies_rules,method = "grouped",jitter = 0)
plot(movies_rules,method = "graph",jitter = 0)
plot(movies_rules,method = "mosaic",jitter = 0)
#it seems like most of them has watched lord of ring movies along with green vile and gladiators
