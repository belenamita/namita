#Credit card data
credit <- read.csv(file.choose())
str(credit)
class(credit)
summary(credit)
attach(credit)

colnames(credit)
sum(is.na(credit))
View(credit$card)
credit$card <- ifelse(credit$card=="yes",0,1)
as.numeric_from_fractor <- function(x) if(is.factor(x)) as.integer(as.factor(x)) else x
credit[] <- lapply(credit,as.numeric_from_fractor)
str(credit)
head(credit)

#Linear regression
model1 <- lm(card~.,data = credit)
model1
summary(model1)
#Split data
install.packages("caTools")
library(caTools)
split <- sample.split(credit$card,SplitRatio = 0.9)
train <- subset(credit,split == TRUE)
test <- subset(credit,split == FALSE)
nrow(train)
nrow(test)

#Using  glm model
model2 <- glm(card~.,family = "binomial",data = train,maxit=50)
summary(model2)
pred <- predict(model2,type = "response")
pred
summary(pred)
