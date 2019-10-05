#Voorbeeld lineaire regressie in classificatiesetting
#Dataset: wmbd

library(tidyverse)
library(GGally)
library(caret)

df_read <- read_csv("wbcd.csv")
df <- df_read[ , -1] #verwijderen unieke case ID

#diagnosis omzetten in factor variabele
#keuze: M's: positives; B's: negatives
df$diagnosis <- factor(df$diagnosis, levels = c("M", "B"),
                       labels = c("Malicious", "Benign"))
summary(df$diagnosis)

#pairwise scatterplots voor eerste 11 variabelen
ggpairs(df[ , 1:11])
#eerste rij en eerste kolom geven idee
#welke variabele(n) goed onderscheid maken tussen "M" en "B"

#vanwege samenhang met Diagnosis, kies perimeter_mean als X-variabele
ggplot(df, aes(x = perimeter_mean, y = diagnosis)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#lineair regressiemodel
#doelvariabele omzetten in 0/1 variabele; 1 = Malicious
df$diagnosis_n <- as.integer(df$diagnosis == "Malicious")

ggplot(df, aes(x = perimeter_mean, y = diagnosis_n)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

linmod1 <- lm(diagnosis_n ~ perimeter_mean, data = df)
summary(linmod1)

#model kan gebruikt worden om voorspellingen te doen
#kies drempel en als modelwaarde boven drempel
#predcitie = 1 (M)

drempel <- 0.5
pred1 <- predict(linmod1)

predict_diagn <- ifelse(pred1 > drempel, "Malicious", "Benign")

cm1 <- confusionMatrix(predict_diagn, df$diagnosis, positive = "Malicious")
cm1
