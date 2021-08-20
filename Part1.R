library("leaps")
library("tidyverse")
library("caret")
library("data.table")
library("car")
cement <- read.table("https://learn-us-east-1-prod-fleet01-xythos.content.blackboardcdn.com/blackboard.learn.xythos.prod/585304babb2ec/5640968?X-Blackboard-Expiration=1606089600000&X-Blackboard-Signature=dYi2RSugK0419rlE7GBEg29BFDOYinD3RiOUXz9x4Ew%3D&X-Blackboard-Client-Id=100773&response-cache-control=private%2C%20max-age%3D21600&response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27cement.txt&response-content-type=text%2Fplain&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20201122T180000Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAYDKQORRYTKBSBE4S%2F20201122%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=a4300e35d2cf53786d29a61e92629b57be3c1062f7bf77023c1651eb1237c90c", header = TRUE, sep = "")
cementlm<-lm(y~., data = cement)
summary(cementlm)
cementlmx1<-lm(y~x1 , data = cement)
cementlmx2<-lm(y~x2 , data = cement)
cementlmx3<-lm(y~x3 , data = cement)
cementlmx4<-lm(y~x4 , data = cement)
summary(cementlmx1)
summary(cementlmx2)
summary(cementlmx3)
summary(cementlmx4)

cementlmx4x1<-lm(y~ x4 + x1, data = cement)
cementlmx4x2<-lm(y~ x4 + x2, data = cement)
cementlmx4x3<-lm(y~ x4 + x3, data = cement)
summary(cementlmx4x1)
summary(cementlmx4x2)
summary(cementlmx4x3)

cementlmx4x1x2<-lm(y~ x4 + x1 + x2, data = cement)
cementlmx4x1x3<-lm(y~ x4 + x1 + x3, data = cement)
summary(cementlmx4x1x2)
summary(cementlmx4x1x3)

cementlmx1x2x3<-lm(y~  x1 + x2+ x3, data = cement)
cementlmx1x2x4<-lm(y~ x1 + x2 + x4, data = cement)
summary(cementlmx1x2x3)
summary(cementlmx1x2x4)

cementlmx1x2<-lm(y~  x1 + x2, data = cement)
summary(cementlmx1x2)

submodels <- regsubsets(y~., data = cement, nvmax = 4)
summary(submodels)

subsetsummary <- summary(submodels)
data.frame(
  Adj.R2 = which.max(subsetsummary$adjr2),
  CP = which.min(subsetsummary$cp)
)
data.frame(
  Adj.R2 = (subsetsummary$adjr2),
  CP = (subsetsummary$cp)
)

vif(cementlmx1x2)
vif(cementlmx1x2x4)

plot(cementlmx1x2)



