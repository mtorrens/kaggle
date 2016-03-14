# XGBOOST WITH BINARY OUTCOME 
setwd("~/kaggle")
load("data/news_popularity_training_extended.RData")
data3 = np.train[,-c(1,2)]
# data3 = np.train[,-c(1,2, 13,32,31,34,35)] # 52.54!

library(xgboost)
library(caTools)
set.seed(3000)
spl = sample.split(data3, SplitRatio = 0.7)
Train2 = subset(data3, spl==TRUE)
Test2 = subset(data3, spl==FALSE)

Train2 = rbind(Train2[Train2$popularity == 1,], Train2[Train2$popularity == 2,])
Train2[Train2$popularity == 1,60] = rep(0, length(Train2[Train2$popularity == 1,60]))
Train2[Train2$popularity == 2,60] = rep(1, length(Train2[Train2$popularity == 2,60]))

# Train2$popularity = as.factor(Train2$popularity)
# Test2$popularity = as.factor(Test2$popularity)

h <- sample(nrow(Train2),200)
dval <- xgb.DMatrix(data=data.matrix(Train2[h,-60]), label=Train2$popularity[h])
dtrain <- xgb.DMatrix(data=data.matrix(Train2[-h,-60]), label=Train2$popularity[-h], missing=NaN)
watchlist <- list(val=dval, train=dtrain)


param <- list(objective = "binary:logistic",
              # objective = "multi:softmax",
              # num_class = 3,
              booster = "gbtree",
              eta = 0.01,
              max_depth = 9,
              subsample = 0.6,
              colsample_bytree = 0.8
)

clf <- xgb.train( params = param,
                  data = dtrain,
                  nrounds = 500,
                  verbose = 1,
                  early.sop.round = 700,
                  watchlist = watchlist,
                  maximize = FALSE
)
pred1 <- predict(clf, data.matrix(Test2[, -60]), missing=NaN)
sum(diag(table(Test2$popularity, round(pred1))/nrow(Test2)*100))