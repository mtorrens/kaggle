#####################################################################
# RANDOM FOREST EXPANDED WITH FEATURE TRANSFORMATION 
#####################################################################
library(caTools)
library(randomForest)

# load the data
load("data/news_popularity_training_extended.RData")
data3 = np.train[,-c(1,2)]

# set seed and run cross-validation 
    set.seed(3000)
    spl = sample.split(data3, SplitRatio = 0.7)
    Train2 = subset(data3, spl==TRUE)
    Test2 = subset(data3, spl==FALSE)
    
    Train2 = rbind(Train2[Train2$popularity == 1,], Train2[Train2$popularity == 2,])
    
    Train2$popularity = as.factor(Train2$popularity)
    Test2$popularity = as.factor(Test2$popularity)
    
    model_forest2 = randomForest(popularity ~ ., data = Train2, ntree=1000, nodesize=25)
    PredictForest = predict(model_forest2, newdata = Test2)
    table(Test2$popularity, PredictForest)/nrow(Test2)*100
    sum(diag(table(Test2$popularity, PredictForest)/nrow(Test2)*100))

# set seed and run submitted model
    set.seed(3000)
    
    # training the model
    datas = data3
    datas = rbind(datas[data3$popularity == 1,], datas[data3$popularity == 2,])
    datas$popularity = as.factor(datas$popularity)
    
    model_forest2 = randomForest(popularity ~ ., data = datas, ntree=1000, nodesize=25)
    
    # loading the testing set
    load("data/news_popularity_test_extended.RData")
    datatest = np.test[,-c(1,2)]
    
    # predict test set and generate submitted csv file 
    PredictForest = predict(model_forest2, newdata = datatest)
    sub1 = as.data.frame(cbind(np.test[,1], PredictForest))
    colnames(sub1) = c("id", "popularity")
    write.csv(sub1, file = 'submission_expanded_RF.csv', row.names = FALSE)