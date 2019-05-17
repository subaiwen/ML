# hw9
# 1.
b.1 <- function(x){x}
b.2 <- function(x){(x-1)^2*(1*(x>=1))}
beta.0 = beta.1 = 1; beta.2 = -2
curve(beta.0 + beta.1*b.1(x) + beta.2*b.2(x), xlim = c(-2,2), ylab = "y")

# 2.
b.1 <- function(x){ 1*(0<=x & x<=2) - (x-2)*(1<=x & x<=2) }
b.2 <- function(x){ (x-3)*(3<=x & x<=4) + 1*(4<x & x<=5) }
beta.0 = beta.1 = 1; beta.2 = 3
curve(beta.0 + beta.1*b.1(x) + beta.2*b.2(x), xlim = c(-2,8), ylab = "y")

# 3.
## a.
library(ISLR)
library(boot)
attach(Auto)
MSE.hat <- rep(NA,10) # testing MSE
for(i in 1:10){
  ply <- glm( acceleration ~ poly(horsepower,i), data = Auto)
  MSE.hat[i] <- cv.glm( Auto, ply)$delta[2] # adjusted MSE penalize # of predictors
}
plot(MSE.hat, xlab = "degree")
lines(MSE.hat)
which.min(MSE.hat)
# plot model
ggplot(Auto, aes(y=acceleration, x=horsepower)) +
  geom_point(alpha = .5) +
  stat_smooth(method = "lm", formula = y ~ poly(x,which.min(MSE.hat)), se = FALSE)
# MSE
set.seed(666)
n <- nrow(Auto)
z <- sample(n,n/2)
ply.train <- glm( acceleration ~ poly(horsepower,i), data = Auto[z,])
mean((acceleration[-z] - predict(ply.train, newx=horsepower[-z]))^2) # test MSE

## b.
library(splines)
plot(horsepower, acceleration)
spline <- lm( acceleration ~ bs(horsepower, knots = c(120,160,180)), data = Auto)
# plot model
ggplot(Auto, aes(y=acceleration, x=horsepower)) +
  geom_point(alpha = .5) +
  stat_smooth(method = "lm", formula = y~ bs(x, knots = c(120,160,180)), se = FALSE)
# MSE
spline.train <- lm( acceleration ~ bs(horsepower, knots = c(120,160,180)), data = Auto[z,])
mean((acceleration[-z] - predict(spline.train, newx=horsepower[-z]))^2) # test MSE

## c.
MSE.hat <- rep(NA,100)
for (k in 1:100){
  d.f <- 2 + k/25
  ss = smooth.spline(horsepower, acceleration, df = d.f) # spline.smooth(x,y)
  MSE.hat[k] <- ss$cv.crit
}
2 + which.min(MSE.hat)/25 # best df
# plot model
ggplot(Auto, aes(y=acceleration, x=horsepower)) +
  geom_point(alpha = .5) +
  ggformula::geom_spline(df = 2 + which.min(MSE.hat)/25, col = "blue")
# MSE
smooth.train <- smooth.spline(horsepower[z], acceleration[z], df = 2 + which.min(MSE.hat)/25)
mean((acceleration[-z] - predict(smooth.train, x=horsepower[-z])$y)^2) # test MSE

# 4.
y <- acceleration
x <- horsepower
## a.
g.1 <- function(x){0}
plot(y~x, ylim = c(0, max(y)))
abline(a= 0, b=0, col = "red")
## b.
plot(y~x, ylim = c(0, max(y)))
abline(a= mean(y), b=0, col = "red")
## c.
plot(y~x, ylim = c(0, max(y)))
lines(smooth.spline(x, y, df = 2), col = "red")
## d.
plot(y~x, ylim = c(0, max(y)))
lines(smooth.spline(x, y, df = 3), col = "red")
## e.
plot(y~x, ylim = c(0, max(y)))
lines(smooth.spline(x,y), col = "red")

