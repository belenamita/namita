#Cutlet csv
cutlet <- read.csv(file.choose())
View(cutlet)
attach(cutlet)
#2 sample t test#
#alpha=0.05
colnames(cutlet)
#Normality test
shapiro.test(Unit.A)
#P- value = 0.32 >0.05 as P value in high it follows normal distribution
shapiro.test(Unit.B)
#P-value = 0.52 as P value is grreater than 0.05 it follows normal distribution
#Variance test
var.test(Unit.A,Unit.B)
#P-value = 0.3136 >0.05  means null hypothesis=Equal variance
#2 sample t test#
t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct= TRUE)
#null hypothesi means equal means
#alternate hypothesis means unequal means
#P-values = 0.4723 >0.05 means accept null hypothesis
#Equal means

?t.test
t.test(Unit.A,Unit.B,alternative = "greater",var.equal = T)
#P-value > 0.2361>0.05 means
#alternatives="greater than means true difference is greater than 0"
#Null hypothesis <- (UnitA-Unit B)< 0
#Alternative hypothesis <- (Unit A - Unit B) > 0
#As p value is greater accept alternative hypothesis
