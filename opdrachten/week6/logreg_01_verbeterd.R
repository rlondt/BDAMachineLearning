#Voorbeeld logistische regressie
#Dataset: wmbd
#model genereren met caret::train() functie

#install.packages("GGally")

library(tidyverse)
library(caret)
library(GGally) #vanwege ggpairs() plot

df_read <- read_csv("wbcd.csv")
df <- df_read[ , -1] #verwijderen unieke case ID

#diagnosis omzetten in factor variabele
#keuze: M's: positives; B's: negatives
df$diagnosis <- factor(df$diagnosis, levels = c("M", "B"),
                          labels = c("Malignant", "Benign"))
summary(df$diagnosis)

#pairwise scatterplots voor eerste 11 variabelen
ggpairs(df[ , 1:11])
#eerste rij en eerste kolom geven idee
#welke variabele(n) goed onderscheid maken tussen "M" en "B"

#vanwege samenhang met Diagnosis, kies perimeter_mean als X-variabele
ggplot(df, aes(x = perimeter_mean, y = diagnosis)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(df, aes(x=perimeter_mean)) +
  geom_histogram() +
  facet_wrap(~diagnosis, ncol = 1)

#logistisch regressiemodel m.b.v. train() functie uit caret package
logreg.01 <- train(diagnosis ~ perimeter_mean, data = df,
                   method = 'glm', family = 'binomial')
summary(logreg.01)

#model gebruiken om predicties te maken
#functie predict(); argument type = 'prob' 
#geeft dataframe met twee kolommen
#resp. P(diagnosis = M | X) en P(diagnosis = B| X)
#hierin verschilt het train() model
#van het model dat met de glm() functie is gegenereerd
#dit voorkomt verwarring over wat 'Y=1' betekent

probs.01 <- predict(logreg.01, type = 'prob')

df$diagnosis.num <- as.numeric(df$diagnosis == "Malignant")
ggplot(df, aes(x = perimeter_mean, y = diagnosis.num, col = diagnosis)) +
  geom_point(size = .5) +
  geom_line(aes(y = probs.01$Malignant), col = "blue")

#keuze drempel voor classificatie
#bijv. drempel = 0.50
#indien P(diagnosis = Malignant|X) >= 0.50, classificeren als Malignant

drempel.01 <- .50
#classificeren
pred.01 <- ifelse(probs.01$Malignant >= drempel.01, "Malignant", "Benign")
pred.01 <- factor(pred.01, levels = c("Malignant", "Benign"))
#overzicht werkelijk naast predicties
View(data.frame(df$diagnosis, pred.01))
View(table(df$diagnosis, pred.01))

#confusionmatrix
cm.01 <- confusionMatrix(pred.01, df$diagnosis,
                         positive = "Malignant")
cm.01$table
cm.01

#indien test wordt gebruikt om de POSITIVES te detecteren
#is de FNrate nog te hoog
#door andere drempel te gebruiken
#kan de FNrate omlaag gebracht worden

#code om voor verschillende drempelwaarden
#de TPrate en de FPrate te bepalen
#en te plotten in een ROC

POS <- sum(df$diagnosis == "Malignant") #aantal POSITIVES
NEG <- sum(df$diagnosis == "Benign") #aantal NEGATIVES

plotdata <- data.frame(matrix(nrow = 0, ncol = 4))

for(drempel in seq(0, 1, .1)) {
  pred <- ifelse(probs.01$Malignant >= drempel, "Malignant", "Benign") %>%
    factor(levels = c("Malignant", "Benign"))
  cm <- confusionMatrix(pred, df$diagnosis)$table
  print(cm)
  TPrate <- cm[1, 1]/POS #met cm de confusion matrix
  FPrate <- cm[1, 2]/NEG
  plotdata <- rbind(plotdata, c(drempel, FPrate, TPrate))
}
names(plotdata) <- c("DREMPEL", "FPrate", "TPrate")

#plot ROC functie
ggplot(plotdata, aes(x = FPrate, y = TPrate)) +
  geom_path() + 
  #geom_path hier beter dan geom_line, omdat de punten
  #in de volgorde in het dataframe verbonden moeten worden
  #geeft alleen verschil als er twee punten zijn met
  #zelfde FPrate of zelfde TPrate
  geom_point() +
  geom_text(aes(label = DREMPEL), size = 3, hjust = -0.25)


#een tweede model met een tweede variabele erbij
#die samenhangt met diagnosis en niet met perimeter_mean

round(cor(df[,2:32]), 2)
ggpairs(df[ , c(1, 4, 12:21)])
ggpairs(df[ , c(1, 4, 22:32)])

#keuze: compactness_worst

logreg.02 <- train(diagnosis~perimeter_mean+compactness_worst,
                 data = df, method = "glm", family = "binomial")
summary(logreg.02)

probs.02 <- predict(logreg.02, type = 'prob')
drempel.02 <- 0.50
pred.02 <- ifelse(probs.02$Malignant >= drempel.02, "Malignant", "Benign") %>%
  factor(., levels = c("Malignant", "Benign"))
cm.02 <- confusionMatrix(pred.02, df$diagnosis, positive = "Malignant")
cm.02$table
cm.02

#ROC curve plotten
plotdata <- data.frame(matrix(nrow = 0, ncol = 4))

for(drempel in seq(0, 1, .1)) {
  pred <- ifelse(probs.02$Malignant >= drempel, "Malignant", "Benign") %>%
    factor(levels = c("Malignant", "Benign"))
  cm <- confusionMatrix(pred, df$diagnosis)$table
  TPrate <- cm[1, 1]/POS #met cm de confusion matrix
  FPrate <- cm[1, 2]/NEG
  plotdata <- rbind(plotdata, c(drempel, FPrate, TPrate))
}
names(plotdata) <- c("DREMPEL", "FPrate", "TPrate")

#plot ROC functie
ggplot(plotdata, aes(x = FPrate, y = TPrate)) +
  geom_path() +
  geom_point() +
  geom_text(aes(label = DREMPEL), size = 3, hjust = -0.25)


