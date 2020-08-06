#Groceries
library(arules)
Groceries <- read.transactions(file.choose(),format = "basket")
inspect(Groceries[1:10])
class(Groceries)
#itemFrequencyplot
itemFrequencyPlot(Groceries,topN=20)
Groceries_rules<-apriori(Groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))

library(arulesViz)
plot(Groceries_rules,method="scatterplot")
plot(Groceries_rules,method = "grouped")
plot(Groceries_rules,method = "graph")
plot(Groceries_rules,method = "mosaic")
