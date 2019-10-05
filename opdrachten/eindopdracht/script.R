#decision tree; CART methode (rpart package)

#voorbeeld: wbcd dataset
#install.packages("caretEnsemble")
#install.packages("doSNOW")


library(mlbench)
library(caret)
library(caretEnsemble)
library(tidyverse)
library(C50)
library(rpart)
library(rpart.plot)
library(caret)
#library(randomForest)
library(doSNOW)
cl <- makeCluster(8, type = "SOCK")
registerDoSNOW(cl)


##############################################################
#
# Voorbereiden bestand
#

# inlezen bestand
df_read <- read.csv("train.csv")

# eerst kolom is een ID variabele; deze kan verwijderd worden
df <- df_read[, -1]

df$target <- as.character(df$target)
df$target[df$target == "1"] <- "yes"
df$target[df$target == "0"] <- "no"
df$target <- as.factor(df$target)

seed <- 20180313

##############################################################
# Algemene funcie voor normaliseren
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
df_normalized <- df
df_normalized[ , 2:201] <- sapply(df_normalized[ , 2:201], normalize)



##############################################################
# splitsen in training data en test data
# zorgen voor in de testdataset voor dezelfde verdeling diverse categorieen doelvariabele

set.seed(seed)
test <- createDataPartition(df$target, p = 0.02, list = FALSE)
df_test <- df[test, ]
#df_test_normalized <- df_normalized[test, ]



# in de trainingdata meer gelijke verdeling krijgen ivm base rate.

df_train <- df[-test,]
df_train_yes <- df[df$target== "yes",]
train_yes <- createDataPartition(df_train_yes$target, p = 0.125/2, list = FALSE)
df_train_no  <- df[df$target== "no",]
train_no <- createDataPartition(df_train_no$target, p = 0.014/2, list = FALSE)
df_train <- rbind(df_train_yes[train_yes,], df_train_no[train_no,])

#df_train_normalized <- df_normalized[-test,]
#df_train_yes <- df_normalized[df$target== "yes",]
#train_yes <- createDataPartition(df_train_yes$target, p = 0.06, list = FALSE)
#df_train_no  <- df_normalized[df$target== "no",]
#train_no <- createDataPartition(df_train_no$target, p = 0.007, list = FALSE)
#df_train_normalized <- rbind(df_train_yes[train_yes,], df_train_no[train_no,])



saveRDS(df_train, "df_train.rds")
saveRDS(df_test, "df_test.rds")
#saveRDS(df_train_normalized, "df_train_normalized.rds")
#saveRDS(df_test_normalized, "df_test_normalized.rds")
#df_train <- readRDS( "df_train.rds")

##############################################################
# Beslisboom maken en beoordelen C5.0

model <- C5.0(target ~ ., data = df_train)

saveRDS(model, "./model_C50_1.rds")
#model <- readRDS("./model_C50_1.rds")

predict <- predict(model, df_test, type = 'class')
cm <- confusionMatrix(predict, df_test$target)
cm$table
saveRDS(cm, "./model_c50_1_cm.rds")
cm_c50_1 <- readRDS("./model_c50_1_cm.rds")



##############################################################
# model verbeteren door grid search
# m.b.v. package C50
# door te kiezen trials = 10 (defaultwaarde is 1)

set.seed(seed)
control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs=TRUE , summaryFunction = twoClassSummary)
metric <- c("Accuracy")


c50Grid <- expand.grid(trials = c(1,10,20)   #c(1:9, (1:10)*10)
                       , model = c("tree", "rules")
                       , winnow = c(TRUE, FALSE)
                       )

#fit_c50  <- train(target~., data=df_train, method="C5.0", metric=metric, trControl=control, tuneGrid = c50Grid)
fit_c50  <- train(target~., data=df_train, method="C5.0", trControl=control, tuneGrid = c50Grid)
saveRDS(fit_c50, "./fit_c50.rds")


# C5.0
#set.seed(seed)
#fit_c50_2  <- train(target~., data=df_train, method="C5.0", metric=metric, trControl=control)
#saveRDS(fit_c50_2, "./fit_c50_2.rds")
#
#predict    <- predict(fit_c50_2, newdata = df_test)
#cm_c50_2   <- confusionMatrix(predict, df_test$target)
#saveRDS(cm_c50_2, "./cm_c50_2.rds")
#cm_c50_2$table

# Stochastic Gradient Boosting
#set.seed(seed)
#fit_gbm <- train(target~., data=df_train, method="gbm", metric=metric, trControl=control, verbose=FALSE)
#saveRDS(fit_gbm, "./fit_gbm.rds")
#
#predict    <- predict(fit_gbm, newdata = df_test)
#cm_gbm   <- confusionMatrix(predict, df_test$target)
#saveRDS(cm_gbm, "./cm_gbm.rds")


# summarize results
#boosting_results <- resamples(list(c5.0=fit_c50_2, gbm=fit_gbm))
#summary(boosting_results)
#dotplot(boosting_results)


##############################################################
# model verbeteren door random forest
# m.b.v. package randomForest

# Random Forest
set.seed(seed)

rf_grid <- expand.grid(mtry = c(2, 3, 4, 5))

fit_rf <- train(target~., data=df_train, method="rf", metric=metric, trControl=control, tuneGrid = rf_grid)
saveRDS(fit_rf, "./fit_rf.rds")

predict    <- predict(fit_rf, newdata = df_test)
cm_rf   <- confusionMatrix(predict, df_test$target)
saveRDS(cm_rf, "./cm_rf.rds")
#cm_rf <- readRDS("./model_rf_cm.rds")
cm_rf$table

# summarize results
boosting_results <- resamples(list(c5.0=fit_c50_2, gbm=fit_gbm, rf=fit_rf))
summary(boosting_results)
dotplot(boosting_results)



#############################################################
# model met logistic regression

#
# regression dus afwijkingen van gemiddelden en eest standaardiseren
# 
set.seed(seed)

lr_grid <- expand.grid(.cost = 1,
                       .loss = c("L1", "L2_dual", "L2_primal"),
                       .epsilon = seq(0.001, 0.01, length.out = 5))

fit_lr <- train(target~.
                , data=df_train
                , method="regLogistic"
                , metric=metric
                , trControl=control
                , tuneGrid = lr_grid
                , preProcess = c("center", "scale"))
saveRDS(fit_rf, "./fit_lr.rds")








# Ensembles
# create submodels
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')


fit_lr <- caretList(target~., data=df_train, trControl=control, methodList=algorithmList)
saveRDS(fit_lr, './fit_lr.rds')
fit_lr
results <- resamples(fit_lr)
summary(results)
dotplot(results)


expand.grid()



install.packages(pkgs = "caret", 
                 dependencies = c("Depends", "Imports"))





