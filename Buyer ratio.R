#Buyer's ratio
buyer <- read.csv(file.choose())
View(buyer)
str(buyer)
#Chi-square test
#hypothesis
#ho= Proportion of male and female are equal
#ha= Proportion of male and female are not equal
attach(buyer)
chisq.test(buyer$Observed.Values,buyer$East,buyer$West,buyer$West,buyer$North,buyer$South)

#P value> 0.05 means reject alternative hypothesis
#proportion of male and female are almost equal