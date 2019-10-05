library(caret)
library(precrec)

##############################################################
# laad data
df_train <- readRDS("df_train.rds")
df_test  <- readRDS("df_test.rds")
# laad modellen
c50.fit <- readRDS("./c50_fit2.rds")
rf.fit  <- readRDS("./rf_fit.rds")
lr.fit  <- readRDS("./lr_fit2.rds")
knn.fit <- readRDS("./knn_fit.rds")
svm.fit <- readRDS("./svm_fit.rds")




##############################################################
# Evaluatie C5.0

c50.predict <- predict(c50.fit, df_test)
c50.cm <- confusionMatrix(c50.predict, df_test$target, positive = 'yes',  mode = "prec_recall")
c50.cm

##############################################################
# Evaluatie Random Forest
rf.predict <- predict(rf.fit, df_test)
rf.cm <- confusionMatrix(rf.predict, df_test$target, positive = 'yes',  mode = "prec_recall")
rf.cm



##############################################################
# Evaluatie Logistische Regressie
lr.predict <- predict(lr.fit, df_test)
lr.cm <- confusionMatrix(lr.predict, df_test$target, positive = 'yes',  mode = "prec_recall")
lr.cm


##############################################################
# Evaluatie K Nearest Neighbours
knn.predict <- predict(knn.fit, df_test)
knn.cm <- confusionMatrix(knn.predict, df_test$target, positive = 'yes',  mode = "prec_recall")
knn.cm


##############################################################
# Evaluatie Support Vector Machine
svm.predict <- predict(svm.fit, df_test)
svm.cm <- confusionMatrix(svm.predict, df_test$target, positive = 'yes',  mode = "prec_recall")
svm.cm


