#Assignment:
#TWO:
#  1) Extract reviews of any product from ecommerce website like snapdeal and amazon


library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/Bosch-Fully-Automatic-Loading-Washing-WAK24168IN/product-reviews/B00OT9CS5S/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
amazon_reviews <- NULL
for (i in 1:20){ #repeating the process 20 times
  murl <- read_html(as.character(paste(aurl,i,sep="=")))#review datas are in html, so using html and converting to character.
  
  rev <- murl %>%
    html_nodes(".review-text") %>% #purpose of using this is to connect. #MAGRITTR is a pipe connecter
    #using nodes i want to read .revie-text type data only
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"Bosch.txt")#from extracted reviews we are reading as apple.text data

install.packages("xlsx")
library("xlsx")

setwd("C://Excelr Data//Assignments//Text Mining")
getwd()



