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
# 2. cargar base de datos
# --------------------------------------------------------
# --------------------------------------------------------

ruta_datos <- "Data/completos.csv"

df <- read_csv(ruta_datos)



# --------------------------------------------------------
# --------------------------------------------------------
# 3. revision inicial
# --------------------------------------------------------
# --------------------------------------------------------

dim(df)

head(df)

names(df)

# --------------------------------------------------------
# --------------------------------------------------------
# 4. seleccionar columnas importantes
# --------------------------------------------------------
# --------------------------------------------------------

df_seleccion <- df %>%
  select(
    # Identificación
    Name, Country,
    
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
    
    # Fecha
    Date
  )

dim(df_seleccion)

# --------------------------------------------------------
# --------------------------------------------------------
# 5. borrar datos nulos
# --------------------------------------------------------
# --------------------------------------------------------

df_limpio <- df_seleccion %>%
  drop_na(
    Sex,
    Event,
    Equipment,
    Age,
    BodyweightKg,
    WeightClassKg,
    Best3SquatKg,
    Best3BenchKg,
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
# 7. Pais a trabajar y filtros
# --------------------------------------------------------
# --------------------------------------------------------

# aplique filtros para que sean menos datos,
# despues lo podemos modificar


pais_trabajo <- "USA"

df_trabajo <- df_limpio %>%
  filter(
    Country == pais_trabajo,
    Event == "SBD",
    Sex %in% c("M", "F"),
    Equipment %in% c("Raw", "Wraps", "Single-ply", "Multi-ply"),
    Age >= 15,
    Age <= 70,
    BodyweightKg >= 35,
    BodyweightKg <= 200,
    Best3SquatKg > 10,
    Best3BenchKg > 10,
    Best3DeadliftKg > 10,
    TotalKg > 10,
    Dots > 10,
    year(Date) >= 2000
  )

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
# 8. Crear muestra final de 10.000 datos
# --------------------------------------------------------
# --------------------------------------------------------

set.seed(123)

df_final <- df_trabajo %>%
  slice_sample(n = min(10000, nrow(df_trabajo))) %>%
  mutate(
    Year = year(Date),
    Elite = if_else(Dots >= 400, "Elite", "No elite"),
    TipoEquipo = case_when(
      Equipment == "Raw" ~ "Raw",
      Equipment %in% c("Wraps", "Single-ply", "Multi-ply") ~ "Equipado",
      TRUE ~ NA_character_
    )
  )

dim(df_final)


# --------------------------------------------------------
# --------------------------------------------------------
# 9. Revision base final
# --------------------------------------------------------
# --------------------------------------------------------

df_final %>%
  summarise(
    registros = n(),
    atletas_unicos = n_distinct(Name),
    edad_minima = min(Age),
    edad_maxima = max(Age),
    peso_minimo = min(BodyweightKg),
    peso_maximo = max(BodyweightKg),
    dots_promedio = mean(Dots),
    total_promedio = mean(TotalKg)
  )

df_final %>%
  count(Sex)

df_final %>%
  count(Equipment, sort = TRUE)

df_final %>%
  count(TipoEquipo, sort = TRUE)

df_final %>%
  count(Elite, sort = TRUE)


# --------------------------------------------------------
# --------------------------------------------------------
# 10. Guardar nueva base de datos
# --------------------------------------------------------
# --------------------------------------------------------

nombre_archivo <- paste0("Data/reporte_", pais_trabajo, ".csv")
write.csv(df_final, nombre_archivo, row.names = FALSE)

