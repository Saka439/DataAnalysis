#Je charge les donnees a l'aide de mon matricule 2184805
mondata <- charger(2184805)

#Pour visualiser l'ensemble des donnees
mondata

#Phase 1) a-
#Histogramme de Tukey
hist(mondata$IR,
     main = "Histogramme de l'indice de Rugosité",
     xlab = "Indice de Rugosité",
     ylab = "Fréquence",
     col = "skyblue",
     border = "red",
     breaks = seq(0, 30, by = 1))



#Diagramme de Tukey
boxplot(mondata$IR, main = "Boxplot de l'indice de Rugosité",
        xlab = "Indice de Rugosité", col = "purple", horizontal = TRUE)


# Normal Probability Plot (droite de Henry)
qqnorm(mondata$IR, main = "Droite de Henry pour IR", col = "blue")
qqline(mondata$IR, col = "red")

# Test de normalité de Shapiro-Wilk
shapiro.test(mondata$IR)

moyenne <- mean(mondata$IR)
mediane <- median(mondata$IR)
variance <- var(mondata$IR)
ecart_type <- sd(mondata$IR)
coefficient_variation <- ecart_type / variance

cat("moyenne =", moyenne,
    ", médiane =", mediane,
    ", variance = ", variance,
    ", écart-type =", ecart_type,
    ", coefficient de variation = ", coefficient_variation)

summary(mondata$IR)

n <- length(mondata$IR)
moyenne <- mean(mondata$IR)
ecart_type <- sd(mondata$IR)
se <- ecart_type / sqrt(n)  # Erreur standard
alpha <- 0.05
t_critique <- qt(1 - alpha / 2, df = n - 1)
ic_lower <- moyenne - t_critique * se
ic_upper <- moyenne + t_critique * se
c(ic_lower, ic_upper)

#Phase 1) b-
# Deux histogrammes juxtaposés avec Indice de rugosité
# variant en fonction du type de materiau 0 ou 1
print(table(mondata$M))
str(mondata)
par(mfrow = c(1, 2))


# Histogramme pour le matériau 0
hist(mondata$IR[mondata$M == 0],
     main = "Histogramme IR - Matériau 0",
     xlab = "Indice de Rugosité",
     col = "orange",
     breaks = 20)

# Histogramme pour le matériau 1
hist(mondata$IR[mondata$M == 1],
     main = "Histogramme IR - Matériau 1",
     xlab = "Indice de Rugosité",
     col = "lightgreen",
     breaks = 20)

# Diagramme de Tukey pour le matériau 0
boxplot(mondata$IR[mondata$M == 0],
        main = "Boxplot IR - Matériau 0",
        ylab = "Indice de Rugosité",
        col = "orange",
        horizontal = TRUE)



# Diagramme de Tukey pour le matériau 1
boxplot(mondata$IR[mondata$M == 1],
        main = "Boxplot IR - Matériau 1",
        ylab = "Indice de Rugosité",
        col = "lightgreen",
        horizontal = TRUE)

par(mfrow = c(1, 1))



# Tableau de statistiques descriptives par groupe:
# moyenne, quartiles, écart type, intervalles de confiance pour la moyenne
calculer_stats <- function(x) {
  c(
    Moyenne = mean(x),
    Médiane = median(x),
    Q1 = quantile(x, 0.25),
    Q3 = quantile(x, 0.75),
    Écart_type = sd(x),
    Minimum = min(x),
    Maximum = max(x),
    IC_inf = t.test(x)$conf.int[1],
    IC_sup = t.test(x)$conf.int[2]
  )
}
# pour matériau 0
stats_m0 <- calculer_stats(mondata$IR[mondata$M == 0])

# pour matériau 1
stats_m1 <- calculer_stats(mondata$IR[mondata$M == 1])

# tableau récapitulatif
tableau_stats <- data.frame(
  Matériau_0 = round(stats_m0, 2),
  Matériau_1 = round(stats_m1, 2)
)
print(tableau_stats)


# Test d'hypothèse sur l'égalité des moyennes pour les deux groupes
test_t <- t.test(IR ~ M, data = mondata)
cat("\nTest t pour l'égalité des moyennes:\n")
print(test_t)

# Vérification de la normalité (pour compléter l'analyse)
shapiro_m0 <- shapiro.test(mondata$IR[mondata$M == 0])
shapiro_m1 <- shapiro.test(mondata$IR[mondata$M == 1])

cat("\nTest de normalité Shapiro-Wilk:\n")
cat("Matériau 0 (p-value):", shapiro_m0$p.value, "\n")
cat("Matériau 1 (p-value):", shapiro_m1$p.value, "\n")

par(mfrow = c(1, 1))

#Phase 2
