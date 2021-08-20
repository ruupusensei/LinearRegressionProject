library("leaps")
library("tidyverse")
library("caret")
library("data.table")
library("car")
cdi <- read.table("https://learn-us-east-1-prod-fleet01-xythos.content.blackboardcdn.com/blackboard.learn.xythos.prod/585304babb2ec/5640967?X-Blackboard-Expiration=1606089600000&X-Blackboard-Signature=1orxYazw4PQLsZ9qN6mFNtVNn3JchsYOdcu7STXuVuQ%3D&X-Blackboard-Client-Id=100773&response-cache-control=private%2C%20max-age%3D21600&response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27CDI.txt&response-content-type=text%2Fplain&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20201122T180000Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAYDKQORRYTKBSBE4S%2F20201122%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=7bc84bc90b32c48dc5ad422fb433178e730f4d974eb957d0ee9d592287a99717", header = FALSE, sep = "")
cdilm<-lm(V10~ V6 + V8 + V9 + V13 + V14 + V15, data = cdi)
summary(cdilm)
vif(cdilm)
cdilm_tI_ext<-rstudent(cdilm)
cdilm_ri_inter<-rstandard(cdilm)
summary(cdilm_tI_ext)
plot(cdilm)
plot(cdilm_tI_ext)
hatvalues(cdilm)
dffits(cdilm)[2]
dffits(cdilm)[6]
dffits(cdilm)[8]
dffits(cdilm)[48]
dffits(cdilm)[128]
dffits(cdilm)[206]
dffits(cdilm)[404]
dfbetas(cdilm)[2]
dfbetas(cdilm)[6]
dfbetas(cdilm)[8]
dfbetas(cdilm)[48]
dfbetas(cdilm)[128]
dfbetas(cdilm)[206]
dfbetas(cdilm)[404]
cooks.distance(cdilm)[2]
cooks.distance(cdilm)[6]
cooks.distance(cdilm)[8]
cooks.distance(cdilm)[48]
cooks.distance(cdilm)[128]
cooks.distance(cdilm)[206]
cooks.distance(cdilm)[404]

