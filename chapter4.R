library(ISLR)
names(Smarket)
View(Smarket)
cor(Smarket[,-9])
plot(Smarket$Volume)


# GLM models include logistic regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)
contrasts(Smarket$Direction)
factor(Smarket$Direction)

A = matrix(c(1,2,1,3,8,1,0,4,1), nrow=3, byrow=TRUE)
b = c(2,12,2)
solve(A, b)
solve(A) %*% b


# 3D Plot of Half of a Torus
par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 1))
R <- 3
r <- 2
x <- seq(0, 2*pi,length.out=50)
y <- seq(0, pi,length.out=50)
M <- mesh(x, y)

alpha <- M$x
beta <- M$y


surf3D(x = (R + r*cos(alpha)) * cos(beta),
       y = (R + r*cos(alpha)) * sin(beta),
       z = r * sin(alpha),
       colkey=FALSE,
       bty="b2",
       main="Half of a Torus"
       
       
       