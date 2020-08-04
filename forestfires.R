#forestfires
forest <- read.csv(file.choose())
View(forest)
str(forest)

hist(forest$area)
forest <- mutate(forest,y = log(area+1))
hist(forest$y)

#considering input variables as rain,temperature,humidity,wind speed

normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

forest$temp <- normalise(forest$temp)
forest$rain <- normalise(forest$rain)
forest$RH <- normalise(forest$RH)
forest$wind <- normalise(forest$wind)

#Classification
sum(forest$area < 5)
sum(forest$area >5)

forest$size <- NULL
forest$size <- factor(ifelse(forest$area < 5, 1 , 0),labels=c("small","large"))
View(forest)

#Data partition
train <- forest[1:400,]
test <- forest[401:517,]

#Building model
#vanilladot
model_vanilla <- ksvm(size~temp+RH+wind+rain,data=train,kernel="vanilladot")
model_vanilla
pred_vanilla <- predict(model_vanilla,newdata=test)
mean(pred_vanilla==test$size)#0.6837607

#rbfdot
model_rbf <- ksvm(size~temp+RH+wind+rain,data=train,kernel="rbfdot")
model_rbf
pred_rbf <- predict(model_rbf,newdata=test)
mean(pred_rbf==test$size)# 0.6666667

# kernal = besseldot
model_besseldot<-ksvm(size~temp+RH+wind+rain,data=train,kernel = "besseldot")
model_besseldot
pred_bessel<-predict(model_besseldot,newdata=test)
mean(pred_bessel==test$size)#0.6837607

# kernel = polydot

model_poly<-ksvm(size~temp+RH+wind+rain,data=train,kernel = "polydot")
model_poly
pred_poly<-predict(model_poly,newdata = test)
mean(pred_poly==test$size) #0.6837607

#rbfdot method gives less training error 

