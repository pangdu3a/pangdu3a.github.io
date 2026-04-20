# Helper packages
library(dplyr) # for data wrangling
library(ggplot2) # for awesome plotting
# Modeling packages
library(rpart) # direct engine for decision tree application
library(caret) # meta engine for decision tree application
# Model interpretability packages
library(rpart.plot) # for plotting decision trees
library(vip) # for feature importance
library(pdp) # for feature effects

set.seed(2441139)
library(tree)
library(ISLR)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
High=as.factor(ifelse(Sales<=8,"No","Yes"))
Carseats$High <- High
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
plot(tree.carseats)
text(tree.carseats,pretty=1, cex = 0.65)
tree.carseats

set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0, cex = 0.7)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

prune.carseats=prune.misclass(tree.carseats,best=21)
plot(prune.carseats)
text(prune.carseats,pretty=0, cex = 0.7)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0, cex = 0.7)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

set.seed(12345)
rpart.seats <- rpart(High~.-Sales, data = Carseats)
rpart.plot(rpart.seats)
rpart.seats
set.seed(12345)
plotcp(rpart.seats)

set.seed(12345)
carseats.dt<- rpart(
  formula = High~.-Sales,
  data = Carseats[train,],
  method = "anova",
  control = list(cp = 0, xval = 10) ## xval is number of CV
)
plotcp(carseats.dt)
abline(v = 4, lty = "dashed")
summary(carseats.dt)
