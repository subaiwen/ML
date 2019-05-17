# hw8
# 1.
invisible(lapply(c("tidyverse","ISLR","leaps", "glmnet", "boot", "pls"),library, character.only=TRUE))
attach(College)
set.seed(666)
n <- nrow(College)
z <- sample(n,n/2)
train <- College[z,]
test <- College[-z,]
## (a) LSE
reg.fit <- regsubsets(Apps ~ ., data = College) # leaps
summary(reg.fit)
which.max(summary(reg.fit)$adjr2) # Adjusted R2
which.min(summary(reg.fit)$cp)  # Mallows Cp
which.min(summary(reg.fit)$bic)  # BIC = Bayesian information criterion
    # 8 predictors
summary(reg.fit)$which[8,] # predictors
reg <- lm(Apps ~ Private + Accept + Enroll + Top10perc + Outstate + Room.Board + PhD + Expend, data = College)
summary(reg)
    # Validation set approach
ols.reg <- glm(Apps ~ Private + Accept + Enroll + Top10perc + Outstate + Room.Board + PhD + Expend, data = train)
y.hat <- predict(ols.reg, test)
(err.lm <- mean((test$Apps - y.hat)^2))  # test error
# cv.glm( College, ols.reg , K=2)$delta[2] # MSE # library(boot)

## (b) ridge
reg <- lm(Apps ~Private + Accept + Enroll + Top10perc + Outstate + Room.Board + PhD + Expend, data = train)
X <- model.matrix(reg)
Y <- Apps[z]
cv.ridge <- cv.glmnet(X,Y,alpha=0, nfolds = 10, lambda = seq(0,40,0.01)) # library(glmnet)
(lambda <- cv.ridge$lambda.min) # cv least lambda
X.test <- model.matrix(Apps ~Private + Accept + Enroll + Top10perc + Outstate + Room.Board + PhD + Expend, data = test)
y.hat.ridge <- predict(cv.ridge, s=lambda, newx=X.test) # s: Value(s) of the penalty parameter lambda
(err.ridge <- mean((test$Apps - y.hat.ridge)^2))  # test error
# ridge <- glmnet(X,Y,alpha=0,lambda=cv.ridge$lambda.min)
# ridge$cvm # cv mean square error

## (c) lasso
cv.lasso <- cv.glmnet(X,Y,alpha=1,nfolds = 10, lambda = seq(0,40,0.01)) # library(glmnet)
(lambda <-cv.lasso$lambda.min) # cv least lambda
y.hat.lasso <- predict(cv.lasso, s=lambda, newx=X.test)
(err.lasso <- mean((test$Apps - y.hat.lasso)^2))  # test error
# lasso <- glmnet(X,Y,alpha=1,lambda=cv.ridge$lambda.min, lambda = seq(0,1,0.0001))
# lasso$cvm # cv mean square error

## (d) principal component
cv.prin <- pcr(Apps ~Private + Accept + Enroll + Top10perc + Outstate + Room.Board + PhD + Expend,
                data = train, scale = TRUE, validation = "CV") # library(pls)
validationplot(cv.prin, val.type="MSEP")
MSEP(cv.prin) # MSE
y.hat.pcr <- predict(cv.prin, test, ncomp=8)
(err.pcr <- mean((test$Apps - y.hat.pcr)^2))  # test error

## (e)
cv.plsr <- plsr(Apps ~Private + Accept + Enroll + Top10perc + Outstate + Room.Board + PhD + Expend,
               data = train, scale = TRUE, validation = "CV") # library(pls)
validationplot(cv.plsr, val.type="MSEP")
MSEP(cv.plsr) # MSE
y.hat.plsr <- predict(cv.plsr, test, ncomp=8)
(err.plsr <- mean((test$Apps - y.hat.plsr)^2))  # test error

## compare
cbind(err.lm,err.ridge,err.lasso,err.pcr,err.plsr)
