library(caret)
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
# splitsen in training data en test data
# zorgen voor in de testdataset voor dezelfde verdeling diverse categorieen doelvariabele

set.seed(seed)
test <- createDataPartition(df$target, p = 0.02, list = FALSE)
df_test <- df[test, ]

# in de trainingdata meer gelijke verdeling krijgen ivm base rate.

df_train <- df[-test,]
df_train_yes <- df[df$target== "yes",]
train_yes <- createDataPartition(df_train_yes$target, p = 0.125/2, list = FALSE)
df_train_no  <- df[df$target== "no",]
train_no <- createDataPartition(df_train_no$target, p = 0.014/2, list = FALSE)
df_train <- rbind(df_train_yes[train_yes,], df_train_no[train_no,])

saveRDS(df_train, "df_train.rds")
saveRDS(df_test, "df_test.rds")





