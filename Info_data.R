# Info de la Data 

ruta_datos <- "Data/reporte_USA.csv"

df_trabajo <- read_csv(ruta_datos)

# Analisis de los datos

# Tamaño de la muestra: 
nrow(df_trabajo)

# nombres col: 
names(df_trabajo)

# Variables:
sapply(df_trabajo, class)

# info cols
str(df_trabajo)

# medidas de resumen
summary(df_trabajo)



