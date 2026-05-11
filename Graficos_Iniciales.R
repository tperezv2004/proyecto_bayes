
# --------------------------------------------------------
# --------------------------------------------------------
#                     Librerias
# --------------------------------------------------------
# --------------------------------------------------------

set.seed(123)

library(tidyverse)
library(lubridate)
library(skimr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(dplyr)


# --------------------------------------------------------
# --------------------------------------------------------
#                       Datos
# --------------------------------------------------------
# --------------------------------------------------------

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

# HeatMap 
vars_interes <- c("BodyweightKg", "Age", "Best3SquatKg", 
                  "Best3BenchKg", "Best3DeadliftKg", "TotalKg", 
                  "Dots", "Wilks") # cambiar o agregar ¿?
df_corr <- df_trabajo %>%
  select(all_of(vars_interes)) %>%
  na.omit()
matriz_corr <- cor(df_corr)
corrplot(matriz_corr, 
         method = "color", 
         type = "upper", 
         tl.col = "grey", 
         tl.srt = 45,
         addCoef.col = "grey",
         number.cex = 0.7,
         sig.level = 0.05, 
         insig = "blank",
         diag = FALSE)

# Grafico: Relacion entre Wilks y TotalKg, coloreado por sexo
ggplot(df_trabajo, aes(x = Wilks, y = TotalKg, color = Sex)) + 
  geom_point() +
  labs(x = "Puntos Wilks", y = "Total levantado (kg)", color = "Género")


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

# Grafico comparativo entre Raw y Equipado para el peso muerto
# (si filtramos por peso especifico es mas informativo)
df_p1 <- df_trabajo %>% 
  filter(WeightClassKg == "90", Best3DeadliftKg > 0)
ggplot(df_p1, aes(x = Equipment, y = Best3DeadliftKg, fill = Equipment)) +
  geom_boxplot() +
  labs(title = "Peso Muerto en Clase 90kg: Raw vs Equipado",
       x = "Equipamiento", y = "Mejor Peso Muerto (kg)") +
  theme_minimal()

# Evolucion
df_trabajo %>%
  mutate(Year = year(as.Date(Date))) %>%
  count(Year, Equipment) %>%
  ggplot(aes(x = Year, y = n, fill = Equipment, alpha = Equipment)) +
  geom_area() +
  scale_alpha_manual(values = c(0.8, 0.4, 0.5, 0.6, 0.7, 0.3)) +
  labs(title = "Evolución de Competidores por Tipo de Equipo",
       y = "Número de Atletas")

# Edad y Dots 
ggplot(df_trabajo, aes(x = Age, y = Dots, color = Sex)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Sex) +
  labs(title = "Relación Edad y Rendimiento (DOTS)",
       x = "Edad", y = "Puntaje DOTS") +
  theme_minimal()

# Distribución de Dots por sexo y edad
df_trabajo %>%
  filter(Sex %in% c("M","F"), !is.na(AgeClass)) %>%
  ggplot(aes(x=Sex, y=Dots, fill=Sex))+
  geom_boxplot() +
  facet_wrap(~AgeClass, scales = "free_y") +
  labs(title="Dots por Sexo y Categoría de Edad", x="Sexo", y="Puntos Dots")

# Segmentación de Élite vs No Élite por Edad y Peso
# buscar que puntaje es elite ej: Dots>500
df_trabajo <- df_trabajo %>% mutate(Elite = ifelse(Dots>500, "Si","No"))

df_trabajo %>%
  mutate(AgeRange = cut(Age, breaks = seq(10,100,by=10)),
         WeightRange = cut(BodyweightKg, breaks = seq(30,200,by=10))) %>%
  group_by(AgeRange, WeightRange) %>%
  summarise(AvgDots = mean(Dots, na.rm=TRUE)) %>%
  ggplot(aes(x=AgeRange, y=WeightRange, fill=AvgDots)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="steelblue") +
  labs(title="Dots Promedio por Rangos de Edad y Peso", x="Rango de Edad", y="Rango de Peso (kg)")

# Rendimiento por Sexo segun dots 
ggplot(df_trabajo, aes(x = Dots, fill = Sex)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de Puntaje DOTS por Sexo",
       x = "Puntaje DOTS")



