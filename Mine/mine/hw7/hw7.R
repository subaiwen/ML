# hw7
# 1.(3.14)
## (a)
set.seed (1)
x1 = runif (100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
# (b)
cor(x1,x2)
plot(x1,x2)
# (c)
reg <- lm(y ~ x1 + x2)
summary(reg)
## (d)
reg.x1 <- lm(y ~ x1)
summary(reg.x1)
## (e)
reg.x2 <- lm(y ~ x2)
summary(reg.x2)
## (g)
x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)
reg <- lm(y ~ x1 + x2)
reg.x1 <- lm(y ~ x1)
reg.x2 <- lm(y ~ x2)
summary(reg)
summary(reg.x1)
summary(reg.x2)
### plot
par(mfrow = c(2,2))
plot(x1,x2)
points(x = 0.1, y = 0.8, col = 2)
plot(x1,y)
points(x = 0.1, y = 6, col = 2)
plot(x2,y)
points(x = 0.8, y = 6, col = 2)
### outlier
par(mfrow = c(1,3))
t.reg <- rstudent(reg)
t1.reg <- rstudent(reg.x1)
t2.reg <- rstudent(reg.x2)
plot(t.reg); plot(t1.reg); plot(t2.reg)
### influential
par(mfrow = c(1,3))
plot(influence(reg)$hat)
plot(influence(reg.x1)$hat)
plot(influence(reg.x2)$hat)
## (e)
car::vif(reg)

# 3.
## (a)
X <- 1
par(mfrow = c(2,2))
Y <- 3
lambda <- 1
curve((Y - beta)^2 + lambda*beta^2, -5, 5, col =1,
      xname = "beta", xlab="beta", main="Y=3, lambda=1") # ridge
curve((Y - beta)^2 + lambda*abs(beta), -5, 5, col =2,
      xname = "beta", xlab="beta", add = TRUE) # lasso
Y <- 3
lambda <- 3
curve((Y - beta)^2 + lambda*beta^2, -5, 5, col =1,
      xname = "beta", xlab="beta", main="Y=3, lambda=3") # ridge
curve((Y - beta)^2 + lambda*abs(beta), -5, 5, col =2,
      xname = "beta", xlab="beta", add = TRUE) # lasso
Y <- 1
lambda <- 3
curve((Y - beta)^2 + lambda*beta^2, -5, 5, col =1,
      xname = "beta", xlab="beta", main="Y=1, lambda=3") # ridge
curve((Y - beta)^2 + lambda*abs(beta), -5, 5, col =2,
      xname = "beta", xlab="beta", main="lasso", add = TRUE) # lasso
Y <- 2
lambda <- 2
curve((Y - beta)^2 + lambda*beta^2, -5, 5, col =1,
      xname = "beta", xlab="beta", main="Y=2, lambda=2") # ridge
curve((Y - beta)^2 + lambda*abs(beta), -5, 5, col =2,
      xname = "beta", xlab="beta", main="lasso", add = TRUE) # lasso
## (b)
ridge.beta <- function(l){return( Y/(1+l) )}
lasso.beta <- function(l){ return( (Y-l/2)*(Y > l/2) + (Y+l/2)*(Y < -l/2)) + 0*(abs(Y) < l/2)}
# plot
par(mfrow = c(1,2))
Y <- 1
curve( ridge.beta, 0, 20, col=1, lwd=3, ylim = c(-0.001,1), xlab="lambda", main="Y=1" )
curve( lasso.beta, 0, 20, col=2, lwd=3, add=TRUE)
Y <- 3
curve( ridge.beta, 0, 20, col=1, lwd=3, ylim = c(-0.001,1), xlab="lambda", main="Y=3" )
curve( lasso.beta, 0, 20, col=2, lwd=3, add=TRUE)

# 4.
## (a)
set.seed(666)
x <- rnorm(100)
epsi <- rnorm(100)
## (b)
b.0 <- b.1 <- b.2 <- b.3 <- 2
y <- b.0 + b.1*x + b.2^2*x + b.3^3*x + epsi
## (c)
df <- data.frame(x,y)
null <- lm(y~1, data =df)
full <- lm(y ~ x + I(x^2)+ I(x^3) + I(x^4)+ I(x^5)+
             I(x^6)+ I(x^7)+ I(x^8)+ I(x^9)+ I(x^10), data = df)
step( null,scope=list(lower=null, upper=full), direction="forward")
## (d)
x.mat <- model.matrix(full)
lasso <- glmnet::cv.glmnet(x.mat,y,alpha = 1, lambda = seq(0,10,0.01))
lasso$lambda.min
plot(lasso)
predict( lasso, lasso$lambda.min, type="coefficients")
## (e)
b.7 <- 2
y <- b.0 + b.7^7*x + epsi
bestsub <- leaps::regsubsets(y ~ x + I(x^2)+ I(x^3) + I(x^4)+ I(x^5)+ I(x^6)
                             + I(x^7)+ I(x^8)+ I(x^9)+ I(x^10), data = df)
par(mfrow = c(1,3))
plot(summary(bestsub)$adjr2, main = "adjr2")
lines(summary(bestsub)$adjr2)
plot(summary(bestsub)$cp, main = "cp")
lines(summary(bestsub)$cp)
plot(summary(bestsub)$bic, main = "bic")
lines(summary(bestsub)$bic)
which.max(summary(bestsub)$adjr2)
which.min(summary(bestsub)$cp)
which.min(summary(bestsub)$bic)
summary(lm(y ~ x + I(x^2)+ I(x^3) + I(x^4)+ I(x^5)+ I(x^6),data =df))
summary(lm(y ~ x + I(x^2),data =df))
# y ~ x^2
reg <- lm(y ~ x + I(x^2),data =df)
x.mat <- model.matrix(reg)
lasso <- glmnet::cv.glmnet(x.mat,y,alpha = 1, lambda = seq(0,10,0.01))
predict( lasso, lasso$lambda.min, type="coefficients")
