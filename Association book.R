#association Rule
book <- read.csv("//Users//smitshah//Desktop//Assignments//Association rule//book (2).csv")
str(book)
book[] <- lapply(book,as.character)
View(book)
#Use of custom function
paste_fun <- function(i){
  return(paste(as.character(i),collapse = ""))
}
book["newcol"] <- apply(book,1,paste_fun)
View(book["newcol"])
install.packages("tm")
library(tm)
x <- Corpus(VectorSource(book$`newcol`))
x <- tm_map(x,stripWhitespace)
#Creating TDM matrix
dtm1 <- t(TermDocumentMatrix(x))
#Converting to dataframe
dtm1df <- data.frame(as.matrix(dtm1))
View(dtm1df)
#association rule
library(arules)
library(arulesViz)
#frequency plot
Windows()
barplot(sapply(dtm1df,sum),col=1:10)
#Applying apriori rules
rules <- apriori(as.matrix(dtm1df),parameter = list(support=0.002,confidence=0.5,minlen=2))
inspect(rules)
plot(rules)

