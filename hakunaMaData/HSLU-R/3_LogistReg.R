# Pfad angeben in dem die Daten liegen
setwd(choose.dir())

# Manuell Windows
#setwd("C:/Users/Davide Di Ronza/Google Drive/Dokumente DR/Hakuna Madata/HSLU")
# Manuell Mac
#setwd("/Users/lisa/Documents/Programs/HSLU-R")

# Daten aus der CSV einlesen
daten <- read.csv("logreg_daten.csv")

# Kreuztabelle der Variablen (Häufigkeiten)
with(daten, ftable(gender, hipster, works_startup, y))

# Kreuztabelle als bedingte relative Häufigkeiten in Prozent
100 * prop.table(with(daten, ftable(gender, hipster, works_startup, y)), 1)

# Logistische Regression
# mit allen möglichen Haupteffekten/Interaktionen der 3 Prädiktoren
modF <- glm(y ~ gender * hipster * works_startup, daten, family = binomial)

# Zusammenfassung des vollen Modells (modF)
summary(modF)

modS <- modF
# Löschung aller möglichen Modellterme inkl. Likelihood-Ratio-Tests
drop1(modS, test = "LRT")

# nur 1 Term entfernbar; da dieser p >= .05 wird er gelöscht
# mit update() kann man ein bestehendes Modell aktualisieren
#   . ~ . - gender:hipster:works_startup
#   bedeutet, dass wir hier, ausgehend vom Modell in modS,
#   beim rechten Teil der Gleichung den Term mit der
#   Dreifachinteraktion entfernen
modS <- update(modS, . ~ . - gender:hipster:works_startup)

# Nun können wir theoretisch 3 Terme entfernen.
# Wir nehmen den mit dem größten p-Wert: hipster:works_startup, p = .8641
drop1(modS, test = "LRT")

modS <- update(modS, . ~ . - hipster:works_startup)
drop1(modS, test = "LRT")

modS <- update(modS, . ~ . - gender:hipster) # p = .20770
drop1(modS, test = "LRT")

modS <- update(modS, . ~ . - gender:works_startup) # p = .1521  
drop1(modS, test = "LRT")

modS <- update(modS, . ~ . - gender) # p = .6858  
drop1(modS, test = "LRT")

# wir können keine Terme mehr entfernen ohne dass sich die Modellpassung
# signifikant verschlechtert

# alternativ könnte man auch automatisierte Funktionen zur Modellselektion,
# wie step() verwenden, wobei diese z.B. Informationskriterien (AIC, BIC) 
# verwenden um zu bestimmen welche Terme gelöscht werden.
# Die Lösungen können sich also unterscheiden (ist hier der Fall)
step(modF)
# in dieser Lösung bleiben die Terme "gender" sowie die Interaktion
#   "gender:works_startup" im Modell

# Zusammenfassung unseres finalen Modells:
summary(modS)



# Basierend auf dem reduzierten Modell können wir nun Vorhersagen machen.

# Da in unserem Modell nur 2 dichotome Variablen sind reduziert sich die
# Anzahl möglicher Szenarien auf 4
# Wenn wir die Variablen "hipster" und "works_startup" nehmen und auf die
# "einzigartigen" (unique) reduzieren erhalten wir diese Matrix für die Vorhersage:
pred_mat <- unique(daten[,c("hipster", "works_startup")])
pred_mat

# Mit dem Modell und den Daten in pred_mat machen wir mit predict() vorhersagen
# Gibt man bei der Funktion keinen Typ an, so erhalt man Logit-transformierte
#   Wahrscheinlichkeiten: log(p/(1-p))
#   Diese speichern wir in eine neue Variable "pred_link" im Datensatz
pred_mat$pred_link <- predict(modS, newdata = pred_mat)
# Da wir an Wahrscheinlichkeiten interessiert sind, fordern wir diese mit
#   type = "response"   an und speichern sie in der Variable "pred_prob"
pred_mat$pred_prob <- predict(modS, newdata = pred_mat, type = "response")

# Hier sehen wir nun die Variablen, die wir zur Vorhersage verwendet haben
#   inklusive der Wahrscheinlichkeiten (logit-transformiert und normal)
pred_mat

# Diese decken sich mit den bedingten relativen Wahrscheinlichkeiten aus den Daten
100 * prop.table(with(daten, ftable(hipster, works_startup, y)), 1)



# hinzufügen der Wahrscheinlichkeiten zu den Daten und export als CSV:
daten$wahrs <- predict(modS, type = "response")
write.csv(daten, file = "daten_out.csv")
