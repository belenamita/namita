#TAT
tat <- read.csv(file.choose())
View(tat)
attach(tat)
#alpha=0.05
colnames(tat)
Stacked_Data <- stack(tat)
View(Stacked_Data)
attach(Stacked_Data)
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value < 0.05 reject  null hypothesis 
# All Proportions all unequal 



