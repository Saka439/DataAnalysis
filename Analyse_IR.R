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

#Phase 2: Effectuons l'ajustement,
#testons la signification du modèle
# et effectuons une analyse
# des residus , donnons un intervalle de confiance
# pour chacun des params bêta0 et bêta1:

# Fonction pour analyser un modèle linéaire standard
analyser_modele <- function(modele, nom_modele) {
  # 1. Ajustement du modèle et coefficients
  cat("\n\n====", nom_modele, "====\n")
  print(summary(modele))
  # 2. ANOVA
  cat("\nTableau ANOVA:\n")
  print(anova(modele))
  # 3. Intervalles de confiance des paramètres
  cat("\nIntervalles de confiance à 95%:\n")
  print(confint(modele))
  # 4. Analyse des résidus
  par(mfrow = c(2, 2))
  plot(modele, which = c(1, 2, 5))
  title(paste("Diagnostics pour", nom_modele), line = -1, outer = TRUE)
  par(mfrow = c(1, 1))
  # Test de normalité des résidus
  shapiro_res <- shapiro.test(residuals(modele))
  cat("\nTest de normalité des résidus (Shapiro-Wilk): p-value =", 
      shapiro_res$p.value, "\n")
}


# Modèle 1: Linéaire en V
modele1 <- lm(IR ~ V, data = mondata)
analyser_modele(modele1, "Modèle 1: Y = β0 + β1*V + ε")

# Modèle 2: Puissance en V (Y = β0*V^β1*e^ε)
# Transformation logarithmique pour linéarisation
mondata$log_IR <- log(mondata$IR)
mondata$log_V <- log(mondata$V)
modele2 <- lm(log_IR ~ log_V, data = mondata)
analyser_modele(modele2, "Modèle 2: ln(Y) = ln(β0) + β1*ln(V) + ε")

# Modèle 3: Exponentiel en V (Y = β0*e^(β1*V + ε))
# Transformation logarithmique afin de linéariser
modele3 <- lm(log_IR ~ V, data = mondata)
analyser_modele(modele3, "Modèle 3: ln(Y) = ln(β0) + β1*V + ε")

# Modèle 4: Linéaire en T
modele4 <- lm(IR ~ T, data = mondata)
analyser_modele(modele4, "Modèle 4: Y = β0 + β1*T + ε")

# Modèle 5: Puissance en T (Y = β0*T^β1*e^ε)
# Transformation logarithmique afin de linéariser
mondata$log_T <- log(mondata$T)
modele5 <- lm(log_IR ~ log_T, data = mondata)
analyser_modele(modele5, "Modèle 5: ln(Y) = ln(β0) + β1*ln(T) + ε")

# Modèle 6: Exponentiel en T (Y = β0*e^(β1*T + ε))
modele6 <- lm(log_IR ~ T, data = mondata)
analyser_modele(modele6, "Modèle 6: ln(Y) = ln(β0) + β1*T + ε")

# Nettoyage des variables temporaires
mondata$log_IR <- NULL
mondata$log_V <- NULL
mondata$log_T <- NULL

# Comparaison des AIC (plus petit = meilleur)
AIC(modele1, modele2, modele3, modele4, modele5, modele6)
