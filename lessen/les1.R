


#install.packages("RWeka")
install.packages("MASS")
install.packages("gmodels")


install.packages("lattice")
install.packages("Cubist")
install.packages("Matrix")
install.packages("survival")
install.packages("rpart")
install.packages("partykit")
install.packages("C50")
install.packages("wordcloud")
install.packages("nlme")
install.packages("foreign")
install.packages("psych")
install.packages("rpart.plot")
install.packages("neuralnet")
install.packages("arules")
install.packages("class")
install.packages("nnet")
install.packages("ipred")
install.packages("recipes")
install.packages("caret")
install.packages("lmtest")
install.packages("vcd")
install.packages("irr")
install.packages("KernSmooth")
install.packages("ROCR")



install.packages("predict")



library(tidyverse)
library(RWeka)
library(gmodels)

# Andrew Ng -> guru machine learning
# KDNuggets.com


# Voorspellen overlijden op basis van nieuwe bronnen, dekkingsgraad, korten 
# steekproef bepalen op basis van resultaten uit het verleden
# campagnes/doelgroepen donor-registratie
# steekproef voor attesta de vita, controle terugkoppelen aan systeem zodat gerichtere controles kunnen 
#  plaatsvinden


df <- read_delim('datafiles/usedcars.csv', delim = ',')

typeof(df)




head(df)

distinct(df)

str(df)


range(df$price)


usedcars <- df#read.csv("usedcars.csv", stringsAsFactors = FALSE)

# get structure of used car data
str(usedcars)

## Exploring numeric variables -----

# summarize numeric variables
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])

# calculate the mean income
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))

# the median income
median(c(36000, 44000, 56000))

# the min/max of used car prices
range(usedcars$price)

# the difference of the range
diff(range(usedcars$price))

# IQR for used car prices
IQR(usedcars$price)

# use quantile to calculate five-number summary
quantile(usedcars$price)

# the 99th percentile
quantile(usedcars$price, probs = c(0.01, 0.99))

# quintiles
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# boxplot of used car prices and mileage
boxplot(usedcars$price, main="Boxplot of Used Car Prices",
        ylab="Price ($)")

boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage",
        ylab="Odometer (mi.)")

# histograms of used car prices and mileage
hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")

hist(usedcars$mileage, main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")

# variance and standard deviation of the used car data
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

## Exploring numeric variables -----

# one-way tables for the used car data
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

# compute table proportions
model_table <- table(usedcars$model)
prop.table(model_table)

# round the data
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

## Exploring relationships between variables -----

# scatterplot of price vs. mileage
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")

# new variable indicating conservative colors
usedcars$conservative <-
  usedcars$color %in% c("Black", "Gray", "Silver", "White")

# checking our variable
table(usedcars$conservative)

# Crosstab of conservative by model
CrossTable(x = usedcars$model, y = usedcars$conservative)
