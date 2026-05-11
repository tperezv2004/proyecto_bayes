set.seed(7489)
library(tidyverse)
library(lubridate)
library(skimr)

ruta_datos <- "Data/datos_trabajo.csv"

df_trabajo <- read_csv(ruta_datos)
# --------------------------------------------------------
# --------------------------------------------------------
#             Graficos preliminaries
# --------------------------------------------------------
# --------------------------------------------------------


# Grafico 1: Distribucion del puntaje DOTS

hist(
  df_trabajo$Dots,
  breaks = 30,
  main = paste("Distribucion del puntaje DOTS en atletas de USA"),
  xlab = "Puntaje DOTS",
  ylab = "Frecuencia"
)


# Grafico 2: Puntaje DOTS segun sexo

boxplot(
  Dots ~ Sex,
  data = df_trabajo,
  main = paste("Puntaje DOTS segun sexo en USA"),
  xlab = "Sexo",
  ylab = "Puntaje DOTS"
)


# Grafico 3: Puntaje DOTS segun equipamiento

boxplot(
  Dots ~ Equipment,
  data = df_trabajo,
  main = paste("Puntaje DOTS segun equipamiento en USA"),
  xlab = "Equipamiento",
  ylab = "Puntaje DOTS"
)


# Grafico 4: Relacion entre peso corporal y DOTS

plot(
  df_trabajo$BodyweightKg,
  df_trabajo$Dots,
  main = paste("Relacion entre peso corporal y DOTS en USA"),
  xlab = "Peso corporal (kg)",
  ylab = "Puntaje DOTS"
)


# Grafico 5: Relacion entre edad y DOTS

plot(
  df_trabajo$Age,
  df_trabajo$Dots,
  main = paste("Relacion entre edad y DOTS en USA"),
  xlab = "Edad",
  ylab = "Puntaje DOTS"
)


# Grafico 6: Peso muerto segun equipamiento

boxplot(
  Best3DeadliftKg ~ Equipment,
  data = df_trabajo,
  main = paste("Peso muerto segun equipamiento en USA"),
  xlab = "Equipamiento",
  ylab = "Mejor peso muerto registrado (kg)"
)
