#decision tree; CART methode (rpart package)

#voorbeeld: wbcd dataset

library(tidyverse)
library(C50)
library(rpart)
library(rpart.plot)
library(caret) #confusion matrix

##############################################################
#inlezen bestand
df_read <- read.csv("Datafiles/wbcd.csv")
#eerst kolom is een ID variabele; deze kan verwijderd worden
df <- df_read[, -1]
df$diagnosis <- factor(df$diagnosis, levels = c("M", "B"))

##############################################################
#splitsen in training data en test data
#zorgen voor zelfde verdeling diverse categorieen doelvariabele
#in training en test data

set.seed(20180313)
train <- createDataPartition(df$diagnosis, p = 0.75, list = FALSE)
df_train <- df[train, ]
df_test <- df[-train, ]

##############################################################
#model maken en beoordelen

model <- C5.0(diagnosis ~ ., data = df_train)

predict <- predict(model, df_test, type = 'class')
cm <- confusionMatrix(predict, df_test$diagnosis)
cm$table
##############################################################

#OPGAVE: vul aan met code zodat de FPrate en de FNrate worden berekend
#OPGAVE: schrijf code zodat bovenstaande procedure 50 keer herhaald wordt
#en bereken de gemiddelde waarden van de FPrate en de FNrate.






##############################################################
#model verbeteren door boosting
#m.b.v. package C50
#door te kiezen trials = 10 (defaultwaarde is 1)
set.seed(20180313)

train <- createDataPartition(df$diagnosis, p = 0.75, list = FALSE)

df_train <- df[train, ]
df_test <- df[-train, ]

model_10 <- C5.0(diagnosis ~ ., data = df_train, trials = 10)
model_10
summary(model_10)

predict <- predict(model_10, df_test, type = 'class')
cm <- confusionMatrix(predict, df_test$diagnosis)
cm$table

#procedure 50 keer herhalen

FPs <- NULL; FNs <- NULL
for (i in 1:50) {
  train <- createDataPartition(df$diagnosis, p = 0.75, list = FALSE)
  df_train <- df[train, ]
  df_test <- df[-train, ]
  model <- C5.0(diagnosis ~ ., data = df_train, trials = 20)
  predict <- predict(model, df_test, type = 'class')
  cm <- confusionMatrix(predict, df_test$diagnosis)
  FPrate <- 1 - cm$byClass['Specificity']
  FNrate <- 1 - cm$byClass['Sensitivity']
  FPs <- c(FPs, FPrate)
  FNs <- c(FNs, FNrate)
}
(mean(FPs))
(mean(FNs))


##############################################################
#model verbeteren m.b.v. cost-matrix (loss-functie)
#Lantz p. 147

#definieer de cost-matrix
#opbouw: hoofddiagonaal: 0
#positie 2,1: 'penalty' voor een False Neagative
#positis 1,2: 'penalty' voor een False Positive

penal <- matrix(c(0, 4, 1, 0), nrow = 2)

#model maken met extra argument
#costs = penal
set.seed(20180313)

train <- createDataPartition(df$diagnosis, p = 0.75, list = FALSE)
df_train <- df[train, ]
df_test <- df[-train, ]

model_cost <- C5.0(diagnosis ~ ., df_train,
                   costs = penal)
model_cost
summary(model_cost)

predict <- predict(model_cost, df_test, type = 'class')
cm <- confusionMatrix(predict, df_test$diagnosis)
cm$table

##############################################################
