#Fantaloons
fantaloons <- read.csv(file.choose())
View(fantaloons)
attach(fantaloons)
table1 <- table(Weekdays,Weekend)
table1
?prop.test
prop.test(x=c(66,47),n=c(167,120),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
#P value= 0.95 > 0.05 i.e proportion of male and female is equal proportion

#Accept null hyporthesis


prop.test(x=c(66,47),n=c(167,120),alternative = "greater")
#P value =0.5 >0.05 i.e accept null hypothesis

#There is equla proporion of male and femaleon weekend and weekdays
