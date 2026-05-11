set.seed(7489)
library(tidyverse)
library(lubridate)
library(skimr)
library(ggplot2)
library(tidyr)


ruta_datos <- "Data/reporte_USA.csv"

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



# Grafico 6: Peso muerto segun equipamiento

boxplot(
  Best3DeadliftKg ~ Equipment,
  data = df_trabajo,
  main = paste("Peso muerto segun equipamiento en USA"),
  xlab = "Equipamiento",
  ylab = "Mejor peso muerto registrado (kg)"
)

#########################

#distribución de atletas por categoría de equipamiento (Equipment)


ggplot(df_trabajo, aes(x = Wilks, y = TotalKg, color = Sex)) + 
  geom_point() +
  labs(x = "Puntos Wilks", y = "Total levantado (kg)", color = "Género")

ggplot(df_trabajo, aes(x = Federation)) + 
  geom_bar() +
  labs(x = "Federación", y = "Número de atletas") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# relación entre la clase de edad (AgeClass) y 
# el peso máximo promedio levantado para cada tipo de levantamiento
df_trabajo %>%
  group_by(AgeClass) %>%
  summarise(AvgSquat = mean(Best3SquatKg, na.rm = TRUE),
            AvgBench = mean(Best3BenchKg, na.rm = TRUE),
            AvgDeadlift = mean(Best3DeadliftKg, na.rm = TRUE)) %>%
  pivot_longer(cols = c("AvgSquat", "AvgBench", "AvgDeadlift"),
               names_to = "Lift",
               values_to = "Weight") %>%
  ggplot(aes(x = AgeClass, y = Weight, color = Lift, group = Lift)) + 
  geom_line() +
  geom_point() +
  labs(x = "Clase de edad", y = "Peso levantado promedio (kg)", color = "Levantamiento") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# ¿Diferencia entre Raw y Equipado?
ggplot(df_trabajo, aes(x = Equipment, fill = Sex)) + 
  geom_bar(position = "stack") +
  labs(x = "Categoría de equipamiento", y = "Número de atletas", fill = "Género")

# si filtramos por peso especifico es mas informativo
df_p1 <- df_trabajo %>% 
  filter(WeightClassKg == "90", Best3DeadliftKg > 0)
ggplot(df_p1, aes(x = Equipment, y = Best3DeadliftKg, fill = Equipment)) +
  geom_boxplot() +
  labs(title = "Peso Muerto en Clase 90kg: Raw vs Equipado",
       x = "Equipamiento", y = "Mejor Peso Muerto (kg)") +
  theme_minimal()

# Evolución Temporal
df_evolucion <- df_trabajo %>%
  mutate(Year = year(as.Date(Date))) %>%
  count(Year, Equipment)
ggplot(df_evolucion, aes(x = Year, y = n, color = Equipment)) +
  geom_line() +
  labs(title = "Evolución de competidores por equipamiento en USA",
       x = "Año", y = "Cantidad de Atletas") +
  theme_minimal()


# Dots entre hombres y mujeres 
ggplot(df_trabajo, aes(x = BodyweightKg, y = Dots, color = Sex)) +
  geom_point(alpha = 0.1) + # alpha ayuda a ver la densidad de puntos
  geom_smooth(method = "lm") + 
  labs(title = "Rendimiento DOTS por Peso Corporal y Sexo",
       x = "Peso Corporal (kg)", y = "Puntaje DOTS") +
  theme_minimal()


# Edad y DOts 

ggplot(df_trabajo, aes(x = Age, y = Dots, color = Sex)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Sex) +
  labs(title = "Relación Edad y Rendimiento (DOTS)",
       x = "Edad", y = "Puntaje DOTS") +
  theme_minimal()
