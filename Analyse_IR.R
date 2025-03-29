#Je charge les donnees a l'aide de mon matricule 2184805
mondata <- charger(2184805)

#Histogramme de Tukey
hist(mondata$IR, main="Histogramme de l'indice de Rugosité", xlab="IR", col = "skyblue",border = "black",xlim = c(0, max(mondata$IR)))

#Diagramme de Tukey
boxplot(mondata$IR, main="Boxplot de l'indice de Rugosité", xlab="IR" ,col = "purple", horizontal = TRUE)



# Normal Probability Plot (droite de Henry)
qqnorm(mondata$IR,main="Droite de Henry pour IR")
qqline(mondata$IR, col="red")

# Test de normalité de Shapiro-Wilk
shapiro.test(mondata$IR)


summary(mondata$IR)  # Moyenne, quartiles, min, max
sd(mondata$IR)        # Écart-type
mean(mondata$IR)      # Moyenne
var(mondata$IR)       # Variance


n <- length(mondata$IR)
moyenne <- mean(mondata$IR)
ecart_type <- sd(mondata$IR)
se <- ecart_type / sqrt(n)  # Erreur standard
alpha <- 0.05
t_critique <- qt(1 - alpha/2, df=n-1)
IC_lower <- moyenne - t_critique * se
IC_upper <- moyenne + t_critique * se
c(IC_lower, IC_upper)

