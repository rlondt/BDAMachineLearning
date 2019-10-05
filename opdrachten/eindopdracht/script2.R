library(caret)

#paralell
library(doSNOW)
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# Modelling
# 
#laad_data


df_train <- readRDS("df_train.rds")
df_test  <- readRDS("df_test.rds")

##############################################################
# Seed tbv reproduceerbaarheid
seed <- 20180313

##############################################################
# Beoordelen op basis van accuracy
metric <- c("Accuracy")

##############################################################
# Sampling structuur

set.seed(seed)
control <- trainControl(method="repeatedcv"
                        , number=5
                        , repeats=2
                        , classProbs=TRUE 
                        #, summaryFunction = twoClassSummary
                        , verboseIter = TRUE
)



##############################################################
# model C5.0
set.seed(seed)

## tuninggrid
#c50.grid <- expand.grid(trials = seq(1, 36, length.out = 6)  
#                        , model = c("tree", "rules")
#                        , winnow = c(TRUE, FALSE)
#)
#
##trainen
#c50.fit  <- train(target~.
#                  , data=df_train
#                  , method="C5.0"
#                  , metric=metric
#                  , trControl=control
#                  , tuneGrid = c50.grid
#                  #, tuneLength = 1 
#                  )
#saveRDS(c50.fit, "./c50_fit.rds")
#
#plot(c50.fit)

set.seed(seed)
c50.grid2 <- expand.grid(trials = seq(30, 60, length.out = 6)  
                         , model = c("rules")
                         , winnow = c(FALSE)
)

#trainen
c50.fit2  <- train(target~.
                   , data=df_train
                   , method="C5.0"
                   , metric=metric
                   , trControl=control
                   , tuneGrid = c50.grid2
                   #, tuneLength = 1 
)
saveRDS(c50.fit2, "./c50_fit2.rds")

plot(c50.fit2)
set.seed(seed)

c50.grid3 <- expand.grid(trials = seq(60, 100, length.out = 11)  
                         , model = c("rules")
                         , winnow = c(FALSE)
)

#trainen
c50.fit3  <- train(target~.
                   , data=df_train
                   , method="C5.0"
                   , metric=metric
                   , trControl=control
                   , tuneGrid = c50.grid3
                   #, tuneLength = 1 
)
saveRDS(c50.fit3, "./c50_fit3.rds")

plot(c50.fit3)

c50.fit3

c50.predict    <- predict(c50.fit3, newdata = df_test)
saveRDS(c50.predict, "./c50_predict.rds")
c50.predict2    <- predict(c50.fit2, newdata = df_test)

confusionMatrix(svm.predict, df_test$target)
confusionMatrix(c50.predict2, df_test$target)






#gedaan ##############################################################
#gedaan # model verbeteren door random forest
#gedaan # m.b.v. package randomForest
#gedaan 
#gedaan # Random Forest
#gedaan set.seed(seed)
#gedaan 
#gedaan rf.grid <- expand.grid(mtry = seq(1,11))
#gedaan 
#gedaan rf.fit <- train(target~.
#gedaan                 , data=df_train
#gedaan                 , method="rf"
#gedaan                 , metric=metric
#gedaan                 , trControl=control
#gedaan                 , tuneGrid = rf.grid
#gedaan                 #, tuneLength = 1
#gedaan )
#gedaan 
#gedaan saveRDS(rf.fit, "./rf_fit.rds")
#gedaan 
#gedaan rf.predict    <- predict(rf.fit, newdata = df_test)
#gedaan saveRDS(rf.predict, "./rf_predict.rds")
#gedaan 
#gedaan 
#gedaan 
##############################################################
## model met logistic regression
#
##
## regression dus afwijkingen van gemiddelden en eest standaardiseren
## in preProcess
#gedaan set.seed(seed)
#gedaan 
#gedaan lr.grid <- expand.grid(.cost = 1,
#gedaan                        .loss = c("L1", "L2_dual", "L2_primal"),
#gedaan                        .epsilon = seq(0.001, 0.01, length.out = 5))
#gedaan 
#gedaan lr.fit <- train(target~.
#gedaan                 , data=df_train
#gedaan                 , method="regLogistic"
#gedaan                 , metric=metric
#gedaan                 , trControl=control
#gedaan                 , tuneGrid = lr.grid
#gedaan                 , preProcess = c("center", "scale")
#gedaan )
#gedaan 
#gedaan saveRDS(lr.fit, "./lr_fit.rds")
#gedaan 
#gedaan plot(lr.fit)
#gedaan 

set.seed(seed)

lr.grid2 <- expand.grid(.cost = 1,
                        .loss = c("L1"),
                        .epsilon = seq(0.006, 0.04, length.out = 10))

lr.fit2 <- train(target~.
                 , data=df_train
                 , method="regLogistic"
                 , metric=metric
                 , trControl=control
                 , tuneGrid = lr.grid2
                 , preProcess = c("center", "scale")
)

plot(lr.fit2)

saveRDS(lr.fit2, "./lr_fit2.rds")


set.seed(seed)

lr.grid3 <- expand.grid(.cost = 1,
                        .loss = c("L1"),
                        .epsilon = seq(0.02, 1.24, length.out = 30))

lr.fit3 <- train(target~.
                 , data=df_train
                 , method="regLogistic"
                 , metric=metric
                 , trControl=control
                 , tuneGrid = lr.grid3
                 , preProcess = c("center", "scale")
)

plot(lr.fit3)


lr.grid4 <- expand.grid(.cost = seq(0.5, 10, length.out = 10),
                        .loss = c("L1"),
                        .epsilon = seq(0.7, 1.25, length.out = 10) # tolerance
                        )

lr.fit4 <- train(target~.
                 , data=df_train
                 , method="regLogistic"
                 , metric="Accuracy"
                 , trControl=control
                 , tuneGrid = lr.grid4
                 , preProcess = c("center", "scale")
)

plot(lr.fit4)

lr.fit4

saveRDS(lr.fit4, "./lr_fit4.rds")





lr.predict    <- predict(lr.fit4, newdata = df_test)

saveRDS(lr.predict, "./lr_predict.rds")

#############################################################
# model met knn
#
set.seed(seed)

knn.grid <- expand.grid(k = seq(10,400,length.out = 50)
)
knn.fit <- train(target~.
                 , data=df_train
                 , method="knn"
                 , metric=metric
                 , trControl=control
                 , tuneGrid = knn.grid
                 , preProcess = c("center", "scale")
                 #, tunelength = 1
)

saveRDS(knn.fit, "./knn_fit.rds")

knn.fit

knn.predict    <- predict(knn.fit, newdata = df_test)
saveRDS(knn.predict, "./knn_predict.rds")



#############################################################
# model met svm
#
set.seed(seed)

# Sampling structuur aanpassen ivm capaciteit

control <- trainControl(method="repeatedcv"
                        , number=5
                        , repeats=2
                        , classProbs=TRUE 
                        , summaryFunction = twoClassSummary
                        , verboseIter = TRUE
)

svm.grid <- expand.grid(cost=seq(0.001,.01,length.out = 10)
)

svm.fit <- train(target~.
                 , data=df_train
                 , method="svmLinear2"
                 , metric=metric
                 , trControl=control
                 , tuneGrid = svm.grid
                 #, tuneLength = 1
                 , preProcess = c("center", "scale")
)

plot(svm.fit)

saveRDS(svm.fit, "./svm_fit.rds")

svm.predict    <- predict(svm.fit, newdata = df_test)

saveRDS(svm.predict, "./svm_predict.rds")



#############################################################
# summarize results
#boosting_results <- resamples(list(c5.0=fit_c50_2, gbm=fit_gbm, rf=rf.fit))
#summary(boosting_results)
#dotplot(boosting_results)


confusionMatrix(knn.predict, df_test$target)





############################################################
# Plotting ROC

install.packages("precrec")

library(PRROC)
library(precrec)
library(MASS)
library(caret)

plot.roc(df_train$target ,predict(svm.fit, df_train, type="prob")[,1],main="ROC curves, black=train, red=test")
plot.roc(df_test$target ,predict(svm.fit, df_test, type="prob")[,1],main="ROC curves, black=train, red=test")

plot.roc(testdata$Approved_,predict(model1, testdata, type="prob")[,1],col="red",add=TRUE)





par(mfrow=c(2,1))
sscurves <- evalmod(scores = predict(lr.fit2, df_train, type="prob")[,2], labels = df_train$target)
plot(sscurves)
sscurves <- evalmod(scores = predict(lr.fit2, df_test, type="prob")[,2], labels = df_test$target)
plot(sscurves)









