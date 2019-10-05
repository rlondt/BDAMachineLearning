# Modelling
# 

library(caret)

# paralelle verwerking mogelijk maken
library(doSNOW)
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

##############################################################
#laad_data
df_train <- readRDS("df_train.rds")
df_test  <- readRDS("df_test.rds")

##############################################################
# Seed tbv reproduceerbaarheid
seed <- 20180313

##############################################################
# Beoordelen op basis van accuracy
metric <- "Accuracy"

##############################################################
# Sampling structuur

set.seed(seed)
control <- trainControl(method="repeatedcv"
                        , number=5
                        , repeats=2
                        , classProbs=TRUE 
                        , verboseIter = TRUE
)


## DONE## ##############################################################
## DONE## # model C5.0
## DONE## set.seed(seed)
## DONE## 
## DONE## # tuninggrid
## DONE## c50.grid <- expand.grid(trials = seq(1, 36, length.out = 6)  
## DONE##                         , model = c("tree", "rules")
## DONE##                         , winnow = c(TRUE, FALSE)
## DONE## )
## DONE## 
## DONE## #trainen
## DONE## c50.fit  <- train(target~.
## DONE##                   , data=df_train
## DONE##                   , method="C5.0"
## DONE##                   , metric=metric
## DONE##                   , trControl=control
## DONE##                   , tuneGrid = c50.grid
## DONE##                   #, tuneLength = 1 
## DONE##                   )
## DONE## saveRDS(c50.fit, "./c50_fit.rds")
## DONE## 
## DONE## plot(c50.fit)
## DONE## 
## DONE## set.seed(seed)
## DONE## c50.grid2 <- expand.grid(trials = seq(30, 100, length.out = 20)  
## DONE##                          , model = c("tree")
## DONE##                          , winnow = c(FALSE)
## DONE## )
## DONE## 
## DONE## #trainen
## DONE## c50.fit2  <- train(target~.
## DONE##                    , data=df_train
## DONE##                    , method="C5.0"
## DONE##                    , metric=metric
## DONE##                    , trControl=control
## DONE##                    , tuneGrid = c50.grid2
## DONE##                    #, tuneLength = 1 
## DONE## )
## DONE## saveRDS(c50.fit2, "./c50_fit2.rds")
## DONE## 
## DONE## plot(c50.fit2)
## DONE## 
## DONE## 
## DONE## ##############################################################
## DONE## # Bagging door random forest
## DONE## # Random Forest
## DONE## set.seed(seed)
## DONE## 
## DONE## rf.grid <- expand.grid(mtry = seq(1,15))
## DONE## 
## DONE## rf.fit <- train(target~.
## DONE##                 , data=df_train
## DONE##                 , method="rf"
## DONE##                 , metric=metric
## DONE##                 , trControl=control
## DONE##                 , tuneGrid = rf.grid
## DONE## )
## DONE## 
## DONE## saveRDS(rf.fit, "./rf_fit.rds")
## DONE## 
## DONE## rf.predict    <- predict(rf.fit, newdata = df_test)
## DONE## saveRDS(rf.predict, "./rf_predict.rds")
## DONE## 
## DONE###############################################################
## DONE### model met knn
## DONE###
## DONE##set.seed(seed)
## DONE##
## DONE##knn.grid <- expand.grid(k = seq(10,400,length.out = 50)
## DONE##)
## DONE##
## DONE##knn.fit <- train(target~.
## DONE##                 , data=df_train
## DONE##                 , method="knn"
## DONE##                 , metric=metric
## DONE##                 , trControl=control
## DONE##                 , tuneGrid = knn.grid
## DONE##                 , preProcess = c("center", "scale")
## DONE##                 #, tunelength = 1
## DONE##)
## DONE##
## DONE##saveRDS(knn.fit, "./knn_fit.rds")
## DONE##
## DONE##knn.predict    <- predict(knn.fit, newdata = df_test)
## DONE##saveRDS(knn.predict, "./knn_predict.rds")
## DONE##
## DONE##
## DONE##
## DONE##
#############################################################
# model met svm
#
set.seed(seed)

svm.grid <- expand.grid(cost=c(seq(0.0001,.001,length.out = 5),seq(0.001,.005,length.out = 5))
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

saveRDS(svm.fit, "./svm_fit.rds")

svm.predict    <- predict(svm.fit, newdata = df_test)
saveRDS(svm.predict, "./svm_predict.rds")

plot(svm.fit)
svm.fit

## DONE################################################################
## DONE#### model met logistic regression
## DONE###
## DONE### regression dus afwijkingen van gemiddelden en eest standaardiseren
## DONE### in preProcess
## DONE##set.seed(seed)
## DONE##
## DONE##lr.grid <- expand.grid(.cost = c(0.5, 1 ,2 ,5 ,10),
## DONE##                       .loss = c("L1", "L2_dual", "L2_primal"),
## DONE##                       .epsilon = seq(0.001, 0.01, length.out = 5))
## DONE##
## DONE##lr.fit <- train(target~.
## DONE##                , data=df_train
## DONE##                , method="regLogistic"
## DONE##                , metric=metric
## DONE##                , trControl=control
## DONE##                , tuneGrid = lr.grid
## DONE##                , preProcess = c("center", "scale")
## DONE##)
## DONE##
## DONE##saveRDS(lr.fit, "./lr_fit.rds")
## DONE##
## DONE##plot(lr.fit)
## DONE##

set.seed(seed)

lr.grid2 <- expand.grid(.cost = seq(5,25, length.out = 10),
                        .loss = c("L2_dual"),
                        .epsilon = seq(0.004, 0.04, length.out = 10))

lr.fit2 <- train(target~.
                 , data=df_train
                 , method="regLogistic"
                 , metric=metric
                 , trControl=control
                 , tuneGrid = lr.grid2
                 , preProcess = c("center", "scale")
)

saveRDS(lr.fit2, "./lr_fit2.rds")






