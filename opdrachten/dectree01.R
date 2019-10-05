#########################################
#MBA-BDA 2017-2018
#OPGAVE 5-5
#########################################
#install.packages("tree")
#########################################
library(tidyverse)
library(rpart)
library(C50)
library(tree)
library(caret)
library(ISLR)
#########################################

df <- Default
summary(df)
head(df)
tail(df)

##############################################################
set.seed(20180314)

train <- createDataPartition(df$default, p = .90, list = FALSE)
df_train <- df[train,]
df_test <- df[-train, ]

##############################################################
#MODEL1 DECISION TREE met rpart
##############################################################

model_01 <- rpart(default ~ ., data = df_train)
model_01
summary(model_01)

rpart.plot(model_01)
rpart.plot(model_01, type = 2, extra = 106)
rpart.plot(model_01, type = 2, extra = 1)

#prestatie op training data
predict_01_tr <- predict(model_01, type = "class")
cm_01_tr <- confusionMatrix(predict_01_tr, df_train$default)
cm_01_tr$table

predict_01 <- predict(model_01, df_test, type = "class")
cm_01 <- confusionMatrix(predict_01, df_test$default)
cm_01$table

##############################################################
#MODEL1 DECISION TREE met C5.0
##############################################################

model_01c50 <- C5.0(default ~ ., data = df_train)
plot(model_01c50)

predict_01_c50 <- predict(model_01c50, df_test, type = "class")
cm_01_c50 <- confusionMatrix(predict_01_c50, df_test$default)
cm_01_c50$table

##############################################################
#MODEL1 DECISION TREE met tree::tree
##############################################################

model_01_tree <- tree(default ~ ., data = df_train)
plot(model_01_tree)
text(model_01_tree)

predict_01_tree <- predict(model_01_tree, df_test, type = "class")
cm_01_tree<- confusionMatrix(predict_01_tree, df_test$default)
cm_01_tree$table

##############################################################