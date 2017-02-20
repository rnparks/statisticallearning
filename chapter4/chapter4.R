library(ISLR)
names(Smarket)
View(Smarket)
cor(Smarket[,-9])
plot(Smarket$Volume)


# GLM models include logistic regression