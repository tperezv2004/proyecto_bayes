
# --------------------------------------------------------
# --------------------------------------------------------
# 1. LIBRERIAS
# --------------------------------------------------------
# --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(skimr)


# --------------------------------------------------------
# --------------------------------------------------------
# 2. CARGAR BASE DE DATOS
# --------------------------------------------------------
# --------------------------------------------------------

ruta_datos <- "Data/completos.csv"

df <- read_csv(ruta_datos)



# --------------------------------------------------------
# --------------------------------------------------------
# 3. REVUSION INICIAL
# --------------------------------------------------------
# --------------------------------------------------------

dim(df)

head(df)

names(df)

# --------------------------------------------------------
# --------------------------------------------------------
# 4. SELECCION DE COLUMNAS (cambiar? revisar README.txt en carpeta datos)
# --------------------------------------------------------
# --------------------------------------------------------

df_seleccion <- df %>%
  select(
    # Identificación
    Name,
    
    # Variables del atleta
    Sex,
    Age,
    AgeClass,
    BirthYearClass,
    BodyweightKg,
    WeightClassKg,
    
    # Tipo de competencia
    Event,
    Equipment,
    Division,
    Tested,
    
    # --- Intentos de Levantamiento ---
    # Sentadilla (Squat)
    Squat1Kg, Squat2Kg, Squat3Kg, Squat4Kg,
    
    # Press de Banca (Bench)
    Bench1Kg, Bench2Kg, Bench3Kg, Bench4Kg,
    
    # Peso Muerto (Deadlift)
    Deadlift1Kg, Deadlift2Kg, Deadlift3Kg, Deadlift4Kg,
    
    # --- Mejores Marcas y Totales ---
    Best3SquatKg,
    Best3BenchKg,
    Best3DeadliftKg,
    TotalKg,
    
    # Puntajes de rendimiento
    Dots,
    Wilks,
    Goodlift,
    
    # Resultado competitivo
    Place,
    
    # Información geográfica del atleta
    Country,
    State,
    
    # Información de la competencia
    Federation,
    ParentFederation,
    Date,
    MeetCountry,
    MeetState,
    # MeetTown, # Nota: Esta columna no aparece en el README proporcionado, verifica si existe en tu DF
    MeetName,
    Sanctioned
  )

dim(df_seleccion)

head(df_seleccion)


# --------------------------------------------------------
# --------------------------------------------------------
# 5. BORRARS NULOS
# --------------------------------------------------------
# --------------------------------------------------------


df_limpio <- df_seleccion %>%
  drop_na(
    Sex,
    Event,
    Equipment,
    Age,
    BodyweightKg,
    Best3DeadliftKg,
    TotalKg,
    Dots,
    Country,
    Date
  )

dim(df_seleccion)
dim(df_limpio)

# --------------------------------------------------------
# --------------------------------------------------------
# 6. REVISAR DATOS LIMPIA
# --------------------------------------------------------
# --------------------------------------------------------

df_limpio %>%
  count(Sex)

df_limpio %>%
  count(Event, sort = TRUE)

df_limpio %>%
  count(Equipment, sort = TRUE)

df_limpio %>%
  count(es_chileno = Country == "Chile")

df_limpio %>%
  count(Country, sort = TRUE) %>%
  head(20)

df_limpio %>%
  count(es_chileno = Country == "Chile") # SON MUY POCOS CHILENOS?

# --------------------------------------------------------
# --------------------------------------------------------
# 7. PASI A TRABAJAR
# --------------------------------------------------------
# --------------------------------------------------------


pais_trabajo <- "USA"


df_trabajo <- df_limpio %>%
  filter(Country == pais_trabajo)

dim(df_trabajo)


df_trabajo %>%
  count(Sex)

df_trabajo %>%
  count(Event, sort = TRUE)

df_trabajo %>%
  count(Equipment, sort = TRUE)

df_trabajo %>%
  count(WeightClassKg, sort = TRUE)



# --------------------------------------------------------
# --------------------------------------------------------
# 8. GRAFICOS
# --------------------------------------------------------
# --------------------------------------------------------


# Grafico 1: Distribucion del puntaje DOTS

hist(
  df_trabajo$Dots,
  breaks = 30,
  main = paste("Distribucion del puntaje DOTS en atletas de", pais_trabajo),
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
  main = paste("Relacion entre peso corporal y DOTS en", pais_trabajo),
  xlab = "Peso corporal (kg)",
  ylab = "Puntaje DOTS"
)


# Grafico 5: Relacion entre edad y DOTS

plot(
  df_trabajo$Age,
  df_trabajo$Dots,
  main = paste("Relacion entre edad y DOTS en", pais_trabajo),
  xlab = "Edad",
  ylab = "Puntaje DOTS"
)


# Grafico 6: Peso muerto segun equipamiento

boxplot(
  Best3DeadliftKg ~ Equipment,
  data = df_trabajo,
  main = paste("Peso muerto segun equipamiento en", pais_trabajo),
  xlab = "Equipamiento",
  ylab = "Mejor peso muerto registrado (kg)"
)

