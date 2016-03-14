data <- read.csv("news_popularity_training.csv")
sample <- read.csv("news_popularity_sample.csv")

check <- read.csv("submission_basic_RF.csv")

datatest <- read.csv("news_popularity_test.csv")


############################################################################
# EXPLORATORY ANALYSIS
############################################################################

grep("B", colnames(df))

# proportion of each target category
round(table(data$popularity)/nrow(data)*100,1)  

# check timedelta
par(mar=c(5.1,4.1,4.1,2.1))
hist(data$timedelta)

# avg. percentage of each category by day
pop_by_day <- data[,c(1,33:40,62)]


table(pop_by_day$day, pop_by_day$popularity)

############################################################################
# PREDICTION - ATTEMPT 1
############################################################################

data2 = data[,-c(1,2)]
data2$popularity = as.factor(data$popularity)

library(caTools)
set.seed(3000)
spl = sample.split(data2, SplitRatio = 0.6)
Train = subset(data2, spl==TRUE)
Test = subset(data2, spl==FALSE)

Train$popularity = as.factor(Train$popularity)
Test$popularity = as.factor(Test$popularity)

library(randomForest)

model_forest1 = randomForest(popularity ~ ., data = Train, ntree=100)
PredictForest = predict(model_forest1, newdata = Test)
table(Test$popularity, PredictForest)/nrow(Test)*100
sum(diag(table(Test$popularity, PredictForest)/nrow(Test)*100))
#tr(table(Test$popularity, PredictForest)/nrow(Test)*100)


############################################################################
# PREDICTION - SUCCESSFUL ATTEMPT 
############################################################################

data3 = data[,-c(1,2)]

library(caTools)
set.seed(3000)
spl = sample.split(data3, SplitRatio = 0.7)
Train2 = subset(data3, spl==TRUE)
Test2 = subset(data3, spl==FALSE)


Train2$popularity = as.factor(Train2$popularity)
Test2$popularity = as.factor(Test2$popularity)

model_forest2 = randomForest(popularity ~ ., data = Train2, ntree=1000, nodesize=25)
PredictForest = predict(model_forest2, newdata = Test2)
table(Test2$popularity, PredictForest)/nrow(Test2)*100
sum(diag(table(Test2$popularity, PredictForest)/nrow(Test2)*100))


datas = data3

datas = rbind(datas[data3$popularity == 1,], datas[data3$popularity == 2,])
#               Train2[Train2$popularity == 3,])
datas$popularity = as.factor(datas$popularity)

model_forest2 = randomForest(popularity ~ ., data = datas, ntree=1000, nodesize=25)
PredictForest = predict(model_forest2, newdata = datatest[,-c(1,2)])

sub1 = as.data.frame(cbind(datatest[,1], PredictForest))
colnames(sub1) = c("id", "popularity")
head(sub1)

importance = as.data.frame(model_forest2$importance)
importance$variable = row.names(importance)
importance = importance[ order(-importance$MeanDecreaseGini), ]

importance$variable = row.names(importance)
 
write.csv(sub1, file = 'submission_basic_RF.csv', row.names = FALSE)
# 52.33 in sample with ntree 1000 nodesize 25

############################################################################
# OTHER ATTEMPTS WITH FEATURE TRANSFORMATION (MIQUEL)
############################################################################

selvar <- names(model_forest2$importance[model_forest2$importance < 100,])

data3 = data[,-c(1,2)]

data3 = data3[, c(as.vector(which(colnames(data3) %in% selvar)), 60)]

library(caTools)
set.seed(3000)
spl = sample.split(data3, SplitRatio = 0.7)
Train2 = subset(data3, spl==TRUE)
Test2 = subset(data3, spl==FALSE)

# Train2 = rbind(Train2[Train2$popularity == 1,], Train2[Train2$popularity == 2,],
#              Train2[Train2$popularity == 3,], Train2[Train2$popularity == 4,],
#               Train2[Train2$popularity == 5,])

Train2$popularity = as.factor(Train2$popularity)
Test2$popularity = as.factor(Test2$popularity)

model_forest3 = randomForest(popularity ~ ., data = Train2, ntree=200)
PredictForest = predict(model_forest3, newdata = Test2)
table(Test2$popularity, PredictForest)/nrow(Test2)*100

# CLASSIFICATION TREE

library(rpart)

data4 = data3

set.seed(3000)
spl = sample.split(data4, SplitRatio = 0.7)
Train3 = subset(data4, spl==TRUE)
Test3 = subset(data4, spl==FALSE)

Train3$popularity = as.factor(Train3$popularity)
Test3$popularity = as.factor(Test3$popularity)

tree = rpart(popularity ~. , data=Train3, method="class")
predictTree = predict(tree, newdata=Test3, type= "class")
table(Test3$popularity, predictTree)/nrow(Test3)*100

sum(diag(table(Test3$popularity, predictTree)/nrow(Test3)*100))

plot(tree)
m = as.matrix(predictTree)
t = apply(m, 1, max)


# LOGISTIC REGRESSION

data5 = data4

set.seed(3000)
spl = sample.split(data5, SplitRatio = 0.7)
Train3 = subset(data5, spl==TRUE)
Test3 = subset(data5, spl==FALSE)

Train3$popularity = as.factor(Train3$popularity)
Test3$popularity = as.factor(Test3$popularity)

Train3 = rbind(Train3[Train3$popularity == 1,], Train3[Train3$popularity == 2,])

model_glm = glm(popularity ~. +timedelta:kw_max_max
                  , data=Train3, family=binomial(link='logit'))
predictGlm = predict(model_glm, newdata=Test3, type= "response")
table(Test3$popularity, predictGlm > 0.5)/nrow(Test3)*100

sum(diag(table(Test3$popularity, predictGlm)/nrow(Test3)*100))


###############################################################
# RANDOM FOREST MIQUEL
##############################################################


library(caTools)
set.seed(3000)
spl = sample.split(dataset_miquel, SplitRatio = 0.8)
Train_miquel = subset(dataset_miquel, spl==TRUE)
Test_miquel = subset(dataset_miquel, spl==FALSE)

Train_miquel$popularity = as.factor(Train_miquel$popularity)

model_forest_miquel = randomForest(popularity ~ ., data = Train_miquel, ntree=1000, nodesize=25)
PredictForest = predict(model_forest_miquel, newdata = Test_miquel)

sum(diag(table(Test_miquel$popularity, PredictForest)/nrow(Test_miquel)*100))

###################################
# SEASONALITY
###################################



dates = strsplit(as.character(data[,2]), "/")
day = rep(NA, length(dates))
month = rep(NA, length(dates))
year = rep(NA, length(dates))

for (i in 1:length(dates)) {
  
day[i] = dates[[i]][6]
month[i] = dates[[i]][5]
year[i] = dates[[i]][4]

}

data_dates = cbind(data3, day, month, year)


library(caTools)
set.seed(3000)
spl = sample.split(data_dates, SplitRatio = 0.7)
TrainDate = subset(data_dates, spl==TRUE)
TestDate = subset(data_dates, spl==FALSE)


TrainDate$popularity = as.factor(TrainDate$popularity)
TestDate$popularity = as.factor(TestDate$popularity)

model_forestDate = randomForest(popularity ~ ., data = TrainDate, ntree=1000, nodesize=25)
PredictForest = predict(model_forestDate, newdata = TestDate)

sum(diag(table(TestDate$popularity, PredictForest)/nrow(TestDate)*100))

