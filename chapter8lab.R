# Lab: Decision Trees
library(tree)
library(ISLR)
library(score)
library(MASS)
library(randomForest)
library(party)
library(gbm)
attach(Carseats)

View(Carseats)
High <- ifelse(Sales <= 8, "No", "Yes") 
Carseats <- data.frame(Carseats, High)

# Fit Decision Tree
tree.carseats <- tree(High ~ . -Sales, Carseats)
summary(tree.carseats)
plot(tree.carseats)

# the pretty toggle tells R to print full names
text(tree.carseats, pretty = 0)
tree.carseats

# Dividing the data into training and testing
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High ~ . -Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test)

# Now we are going to use cross validation to prune the tree
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

# Now Appling the Pruning function to My tree
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)

# Fiting Regression Trees
View(Boston)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~., Boston, subset = train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty = 0)

# Cross validate optimal tree
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty=0)

# Use the unpruned tree to make predictions on prices
# We use the unpruned tree bc that what was selected by cross validation
yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat - boston.test)^2)


# Random Forrest on Boston Data
set.seed(1)
bag.boston <- randomForest(medv ~ ., data=Boston, subset=train, mtry=13, importance = T)
bag.boston
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)

# Change the number of trees
set.seed(1)
rf.boston <- randomForest(medv ~ ., data=Boston, subset=train, ntree=1000, importance = T)
rf.boston
yhat.bag <- predict(rf.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)
plot(rf.boston, type="simple")

importance(rf.boston)
varImpPlot(rf.boston)

### Boosting
set.seed(1)
boost.boston <- gbm(medv ~., data=Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston, i ="rm")
plot(boost.boston, i="lstat")

yhat.boost <- predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost - boston.test)^2)

# playing with lambda valaue
boost.boston <- gbm(medv ~., data=Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2)
yhat.boost <- predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost - boston.test)^2)

