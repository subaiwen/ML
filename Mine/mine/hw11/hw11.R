# hw11
lapply(c("tidyverse","ISLR"),library, character.only=TRUE)
# 1.
curve(1+3*x, -5, 5)




# 2.
## (a)
circleFun <- function(center = c(-1,2),diameter = 4, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

dat <- circleFun(c(-1,2),4,npoints = 100)
#geom_path will do open circles, geom_polygon will do filled circles
ggplot(dat,aes(x,y)) + geom_path() + xlim(-4,2) + ylim(-1, 5)

## (b)
ggplot(dat,aes(x,y)) + geom_path() + xlim(-4,2) + ylim(-1, 5)

## (c)
dat.new <- data.frame('x' = c(0, -1, 2, 3),
                      'y' = c(0, 1, 2, 8))
ggplot(dat,aes(x,y)) + geom_path() + geom_point(data = dat.new[c(1,3,4),], col = 'blue') + geom_point(data = dat.new[2,], col = 'red')

## (d)

# 3.
## (a)
dots <- data.frame('x1' = c(3,2,4,1,2,4,4),
                   'x2' = c(4,2,4,4,1,3,1),
                   'y' = c(rep('Red',4),rep('Blue',3)))
(plot.a <- dots %>% ggplot(aes(x = x1, y = x2)) +
  geom_point(data = dots[1:4,], col = 'red') +
  geom_point(data = dots[5:7,], col = 'blue') +
  xlim(0,5) + ylim(0,5))
## (b)
plot.a +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_abline(intercept = -1, slope = 1, lty = 2) +
  geom_abline(intercept = -0.5, slope = 1, lty = 1)

# library(e1071)
# s <- svm(y~., data= dots, kernel = 'linear', cost = 0)
# summary(s)
# # get parameters of hiperplane
# x <- dots[,1:2] %>% as.matrix()
# beta = drop(t(s$coefs)%*%x[s$index,])
# beta0 = s$rho
# # in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
# plot.a +
#   geom_abline(intercept = beta0 / beta[2], slope = -beta[1] / beta[2])

## (c)

## (d)

## (e)

## (f)

## (g)
plot.a +
  geom_abline(intercept = -0.3, slope = 0.9, lty = 1)
## (h)
plot.a +
  geom_point(data= data.frame(x1=4,x2=2), col = 'red')


# 4.
library(e1071)
## (a)
Auto$mileage <- rep(1,nrow(Auto))
Auto$mileage[Auto$mpg < median(Auto$mpg)] <- 0
Auto$mileage <- as.factor(Auto$mileage)
## (b)
set.seed(444)
s.b <- tune(svm, mileage ~ weight + year, data = Auto,
                 ranges = list(cost = 10^seq(-3,3,1), kernel = 'linear'))
summary(s.b)
s.b.2 <- tune(svm, mileage ~ weight + year, data = Auto,
              ranges = list(cost = seq(0.01,0.2,0.005), kernel = 'linear'))
summary(s.b.2)
## (c)
set.seed(444)
s.c <- tune(svm, mileage ~ weight + year, data = Auto, ranges = list(
  cost = seq(0.01,0.2,0.005), kernel = c('linear','radial','polynomial')))
summary(s.c)
## (d)
svm.b <- svm(mileage ~ weight + year, data = Auto, kernel = 'linear', cost = 0.02)
svm.c <- svm(mileage ~ weight + year, data = Auto, kernel = 'linear', cost = 0.025)
plot(svm.b, data=Auto, weight~year)
plot(svm.c, data=Auto, weight~year)

