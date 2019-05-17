# hw10
invisible(lapply(c("knitr","kableExtra","tidyverse"),library, character.only=TRUE))
# -----------------------------------------------------------------------------------
# 1.


# -----------------------------------------------------------------------------------
# 2.
p <- c(0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7, 0.75)
  # majority vote approach
summary(p>0.5)
  # average probability
mean(p)

# -----------------------------------------------------------------------------------
# 3.
library(ISLR)
attach(OJ)
## a.
set.seed(123)
n <- nrow(OJ)
train <- sample(n, 800)
## b.
library(tree)
class.tree <- tree(Purchase ~ ., data = OJ[train,])
summary(class.tree)
## c.
class.tree
## d.
plot(class.tree)
text(class.tree)
## e.
prediction <- predict(class.tree, newdata = OJ[-train,], type = "class")
table(prediction,OJ$Purchase[-train])
mean(prediction != OJ$Purchase[-train]) # test error rate
## f.
cv <- cv.tree(class.tree, FUN = prune.misclass )
## g.
plot(cv)
## h.
cv$size[which.min(cv$dev)]
## i.
tree.pruned <- prune.misclass( class.tree, best= cv$size[which.min(cv$dev)] )
## j.
train.pred <- predict(class.tree, newdata = OJ[train,], type = "class")
mean(train.pred != OJ$Purchase[train]) # train error rate
train.pred.pruned <- predict(tree.pruned, newdata = OJ[train,], type = "class")
mean(train.pred.pruned != OJ$Purchase[train]) # train error rate, pruned
## k.
prediction.pruned <- predict(tree.pruned, newdata = OJ[-train,], type = "class")
mean(prediction.pruned != OJ$Purchase[-train])
