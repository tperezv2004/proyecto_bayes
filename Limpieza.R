
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
    Goodlift
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
    # Agregar otras columnas que puedan dar problema
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
nombre_archivo <- paste0("reporte_", pais_trabajo, ".csv")
write.csv(df_trabajo, nombre_archivo, row.names = FALSE)

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

