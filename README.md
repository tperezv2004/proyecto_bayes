hi team

aca van las ideas sueltas


gitignore ingora la base de datos completa pq son muchos datos, y tienen nombre de "completos" 

tal vez sea mejor usar los datos de latinoamerica o de USA, deberian ser mas
usamos toda la base de datos?


creo q son como 1600 chilenos, osea filas tal vez un chileno haya particiapdo mas de una vez entonoces el total sean menos chilenos

la limpieza de datos la ponemos en el informe? segun yo no solo mensionamos

podriamos usar USA son muchos datos pero no se 


la info importante este en extra.R y en informe.Rmd

en extra.R algunos graficos se demoran mucho pq son muchisimos datos podriamos usar una pequeña muestra para graficar pero no se. seria algo asi:

set.seed(123)

df_grafico <- df_trabajo %>%
  slice_sample(n = min(10000, nrow(df_trabajo)))
