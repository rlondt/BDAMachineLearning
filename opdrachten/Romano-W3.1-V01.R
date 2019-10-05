#knn ISLR example

packages <- c("tidyverse", "ISLR", "class", "caret", "pROC")
# Installeer packages
for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, repos = 'http://cran.us.r-project.org')
  }
}
# Laad the packages
for (p in packages){
  suppressPackageStartupMessages(
    library(p, quietly = TRUE, character.only = TRUE ) 
  )
}


# Install the following packages: tidyverse , ISLR , class , caret
# Load the following libraries: tidyverse , ISLR , class , caret
# The dataset is in ISLR package
# caret is for the confusionMatrix

df <- Smarket

# Description data set
?Smarket

# explore the structure of df
structure(df)
# explore the summary of df
summary(df)
# explore the table of year feature
df %>%
  select(Year)%>%
  group_by(Year)%>%
  summarise("aantal", n())

#test set 2001 - 2004
#training set 2005

# Define the normalize function based on Min-Max rescaling
?normalize

# Apply normalization to df features 2 to 6
df_n <- df
df_n[ , 2:6] <- sapply(df_n[ , 2:6], normalize)
summary(df_n)

# Put years 2001-2004 to the training set
# Put the rest into the test set
train <- df_n$Year < 2005
df_n_train <- df_n[train, 2:6]
df_n_test <- df_n[!train, 2:6]

# Put labels of years 2001-2004 to the training labels set
# Put the rest into test labels set
df_n_train_labels <- df_n[train, 9]
df_n_test_labels <- df_n[!train, 9]

# Apply the KNN model with k=10
df_n_test_pred <- knn(
  train = df_n_train,
  test = df_n_test,
  cl = df_n_train_labels,
  k = 10)

# Explore the model performance
cm <- confusionMatrix(data = df_n_test_pred,
                      reference = df_n_test_labels)
cm

# Improve the model by changing K

df_model <- df_n_train
df_model$label <- df_n_train_labels


## Training and Control
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) 
knnFit <- train(label ~ ., data = df_model, method = "knn", trControl = ctrl, tuneLength = 40)
#Output of kNN fit
knnFit
plot(knnFit)

knnPredict <- predict(knnFit,newdata = df_n_test )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, df_n_test_labels )
mean(knnPredict==df_n_test_labels )

## Optimum K=35



## With classProbs and summaryFunction
ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit2 <- train(label ~ ., data = df_model, method = "knn", trControl = ctrl, tuneLength = 40)

#Output of kNN fit
knnFit2
plot(knnFit2, print.thres = 0.5, type="S")

knnPredict2 <- predict(knnFit2,newdata = df_n_test )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict2, df_n_test_labels )
mean(knnPredict2==df_n_test_labels )

## Optimum K=31

