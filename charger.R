charger <- function(matricule) {
  set.seed(matricule)
  mondata <- read.csv2("DevoirD_H25.csv")[sample(275, 210), ] # nolint
}