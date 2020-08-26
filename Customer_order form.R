#Customer order form
customer <- read.csv("//Users//smitshah//Desktop//Assignments//Hypothesis Testing//Costomer+OrderForm.csv")
stack_data <- stack(customer)
View(customer)
class(customer)
str(customer)
customer$Phillippines <- ifelse(customer$Phillippines=="Defective",1,0)
customer$Indonesia <- ifelse(customer$Indonesia=="Defective",1,0)
customer$India <- ifelse(customer$India=="Defective",1,0)
customer$Malta <- ifelse(customer$Malta=="Defective",1,0)
table(stack_data$values,stack_data$ind)
chisq.test(stack_data$values,stack_data$ind)
#P value >0.05  accept null hypothesis
#All proportions are equal
# % proportions of defective are almost equal within 4 samples