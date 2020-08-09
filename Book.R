#Best book recommendation

book <- read.csv(file.choose())
View(book)
class(book)

install.packages("Matrix")
library("recommenderlab")
library(caTools)

str(book)
table(book$Book.Title)
summary(book)
book$User.ID <- as.factor(book$User.ID)
book$Book.Rating <- as.factor(book$Book.Rating)
View(book)


#rating distribution
hist(book$Book.Rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
book_data_matrix <- as(book, 'realRatingMatrix')
dim(book_data_matrix@data)

#similarity matrix
#Users
sim <- similarity(book_data_matrix[1:10,],method = "cosine",which = "users ")
image(as.matrix(sim),main = "User Similarity")
#Books
sim2 <- similarity(book_data_matrix[,1:10],method = "cosine",which ="items")
image(as.matrix(sim2),main="Item Similatity")

#Popularity based
book_recomm_model1 <- Recommender(book_data_matrix,method="POPULAR")

#PREDICTION OF TWO USERS
  recommend_items1 <- predict(book_recomm_model1,book_data_matrix[413:414],n=5)
as(recommend_items1,"list")  
## Popularity model recommends the same movies for all users , we need to improve our model
#using # # Collaborative Filtering
#User Based Collaborative Filtering
book_recomm_model2 <- Recommender(book_data_matrix,method="UBCF")

#Prediction of two users
recommend_items2 <- predict(book_recomm_model2,book_data_matrix[413:414],n=5)
as(recommend_items2,"list")



