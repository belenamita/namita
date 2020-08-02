#Forest fires
forest <- read.csv("//Users//smitshah//Desktop//Assignments//Neural network//forestfires.csv")
View(forest)
hist(forest$area)
library(dplyr)

#area has many zeroes in data.converting to log(area+1)
forest <- mutate(forest,y = log(area+1))
hist(forest$y)
summary(forest)

#normalize the data
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

forest$temp <- normalise(forest$temp)
forest$rain <- normalise(forest$rain)
forest$RH <- normalise(forest$RH)
forest$wind <- normalise(forest$wind)

#Classification
sum(forest$area < 5)
sum(forest$area >=5)
forest$size <- NULL
forest$size <- factor(ifelse(forest$area < 5,1,0),labels = c("small","large"))

#Splitting data
library(neuralnet)
library(nnet)
#Data is unevenly distributed
set.seed(123)
fr <- sample(2,nrow(forest),replace= TRUE,prob = c(0.7,0.3))
train <- forest[fr==1,]
test <- forest[fr==2,]
View(train)
View(test)

#Building model
forest_model <- neuralnet(size~temp+RH+ wind +rain, data = train)
str(forest_model)
plot(forest_model,rep = "best")
summary(forest_model)

#SSE sum of squared errors
#Evaluating model performance
#compute function to generate output for model prepared
set.seed(12323)
model_results <- compute(forest_model,test[])
preduct_profit <- model_results$net.result