# ==============================================================
# Proyecto: Accidentes gestionados por la Guardia Urbana de Barcelona
# Reto 2 - Proyecto de Ciencia de Datos reproducible
# Script 01: importacion y depuracion de datos
#
# Fuente: Open Data BCN, Ajuntament de Barcelona
#         https://opendata-ajuntament.barcelona.cat/data/ca/dataset/accidents-gu-bcn
# Licencia de los datos: Creative Commons Attribution 4.0
# Periodo analizado: 2019-2025
#
# Este script lee los ficheros CSV anuales originales, armoniza sus
# diferencias de estructura, depura los valores problematicos y genera
# nuevas variables de utilidad. El resultado se guarda en la carpeta
# "Datos/Base de datos depurada".
#
# IMPORTANTE: todas las rutas son relativas a la raiz del proyecto de
# RStudio. No hay ninguna referencia a directorios locales, de modo que
# el script funciona en cualquier maquina que clone el repositorio.
# ==============================================================


# --------------------------------------------------------------
# 0. Paquetes necesarios
# --------------------------------------------------------------
# Si es la primera vez, descomenta y ejecuta la linea siguiente:
# install.packages(c("tidyverse", "janitor"))

library(tidyverse)   # lectura, manipulacion y visualizacion de datos
library(janitor)     # limpieza de nombres de columna


# --------------------------------------------------------------
# 1. Localizacion de los ficheros originales
# --------------------------------------------------------------
# En lugar de escribir las siete rutas a mano, pedimos a R que busque
# todos los CSV que haya en la carpeta. Asi, si anadimos un ano nuevo,
# el script lo recoge automaticamente sin tocar el codigo.

ruta_origen <- "Datos/Base de datos original"

ficheros <- list.files(
  path       = ruta_origen,
  pattern    = "\\.csv$",   # solo ficheros terminados en .csv
  full.names = TRUE         # devuelve la ruta completa, no solo el nombre
)

# Comprobacion: deberian aparecer 7 ficheros (2019-2025)
cat("Ficheros encontrados:", length(ficheros), "\n")
print(basename(ficheros))


# --------------------------------------------------------------
# 2. Diagnostico previo: que columnas cambian entre anos
# --------------------------------------------------------------
# Antes de unir nada conviene documentar las diferencias de estructura.
# Esta tabla es util para explicarlas en el informe tecnico.

nombres_por_fichero <- map(ficheros, function(ruta) {
  names(read_csv(ruta, n_max = 0, show_col_types = FALSE))
})
names(nombres_por_fichero) <- basename(ficheros)

# Columnas que aparecen en TODOS los ficheros
columnas_en_todos <- reduce(nombres_por_fichero, intersect)

# Columnas que aparecen solo en algunos
columnas_en_alguno <- reduce(nombres_por_fichero, union)
columnas_conflictivas <- setdiff(columnas_en_alguno, columnas_en_todos)

cat("\nColumnas presentes en todos los anos:", length(columnas_en_todos), "\n")
cat("Columnas que NO coinciden entre anos:\n")
print(columnas_conflictivas)


# --------------------------------------------------------------
# 3. Funcion de lectura y armonizacion
# --------------------------------------------------------------
# Definimos una funcion que lee un fichero y deja sus columnas con
# nombres homogeneos. Despues la aplicaremos a los siete ficheros.
#
# Pasos que realiza:
#   a) lee el CSV (separador coma)
#   b) clean_names() pasa los nombres a minusculas y sin espacios
#      (esto ya resuelve el "Num_postal " con espacio final de 2025)
#   c) renombra las columnas que cambian de nombre entre anos
#   d) convierte todo a texto temporalmente, para que al unir los
#      ficheros no haya conflictos de tipo entre anos

leer_accidentes <- function(ruta) {

  read_csv(
    file           = ruta,
    locale         = locale(encoding = "UTF-8"),  # ver nota al final
    show_col_types = FALSE
  ) |>
    clean_names() |>
    rename_with(~ case_when(
      .x == "num_postal_caption"    ~ "num_postal",
      .x == "coordenada_utm_x_ed50" ~ "coordenada_utm_x",
      .x == "coordenada_utm_y_ed50" ~ "coordenada_utm_y",
      .x == "longitud_wgs84"        ~ "longitud",
      .x == "latitud_wgs84"         ~ "latitud",
      .default = .x
    )) |>
    mutate(across(everything(), as.character))
}


# --------------------------------------------------------------
# 4. Lectura conjunta de los siete anos
# --------------------------------------------------------------
# map_dfr aplica la funcion a cada fichero y apila los resultados
# uno debajo de otro en una sola tabla.

accidentes_bruto <- map_dfr(ficheros, leer_accidentes)

cat("\nFilas totales importadas:", nrow(accidentes_bruto), "\n")


# --------------------------------------------------------------
# 5. Seleccion del subconjunto comun de variables
# --------------------------------------------------------------
# Nos quedamos con las variables presentes en todos los anos y
# relevantes para los objetivos del proyecto. Descartamos
# "dia_setmana" y "descripcio_tipus_dia" (solo en anos antiguos)
# porque el dia de la semana ya lo tenemos y lo recrearemos nosotros.

variables_utiles <- c(
  "numero_expedient",           # identificador del accidente
  "codi_districte", "nom_districte",
  "codi_barri", "nom_barri",
  "nom_carrer",
  "descripcio_dia_setmana",
  "nk_any", "mes_any", "nom_mes", "dia_mes", "hora_dia",
  "descripcio_torn",
  "descripcio_causa_vianant",
  "numero_morts",
  "numero_lesionats_lleus",
  "numero_lesionats_greus",
  "numero_victimes",
  "numero_vehicles_implicats",
  "longitud", "latitud"
)

accidentes <- accidentes_bruto |>
  select(all_of(variables_utiles))


# --------------------------------------------------------------
# 6. Conversion de tipos de datos
# --------------------------------------------------------------
# Devolvemos a cada variable su tipo correcto. Es un paso clave:
# el tipo de dato determina que operaciones y que graficos podemos
# hacer con cada variable.

accidentes <- accidentes |>
  mutate(
    # Numericas discretas (recuentos)
    across(
      c(nk_any, mes_any, dia_mes, hora_dia,
        numero_morts, numero_lesionats_lleus, numero_lesionats_greus,
        numero_victimes, numero_vehicles_implicats,
        codi_districte, codi_barri),
      as.integer
    ),
    # Numericas continuas (coordenadas geograficas)
    across(c(longitud, latitud), as.numeric)
  )


# --------------------------------------------------------------
# 7. Depuracion de valores problematicos
# --------------------------------------------------------------
# La documentacion del dataset indica que el valor -1 senala
# "dato no disponible". Si no lo convertimos a NA, R lo trataria como
# un numero real y falsearia medias, sumas y graficos.

accidentes <- accidentes |>
  mutate(
    across(where(is.numeric), ~ if_else(.x == -1, NA_integer_, as.integer(.x))),
    # Coordenadas: valores 0 o vacios no son posiciones validas en Barcelona
    longitud = if_else(longitud == 0 | is.na(longitud), NA_real_, longitud),
    latitud  = if_else(latitud  == 0 | is.na(latitud),  NA_real_, latitud)
  )

# Textos: unificamos las etiquetas que indican ausencia de informacion
etiquetas_desconocido <- c("Desconegut", "Desconegut ", "-1", "NULL", "")

accidentes <- accidentes |>
  mutate(
    across(
      where(is.character),
      ~ if_else(str_trim(.x) %in% etiquetas_desconocido, NA_character_, str_trim(.x))
    )
  )

# Registros duplicados: un mismo expediente no deberia aparecer dos veces
n_antes <- nrow(accidentes)
accidentes <- accidentes |> distinct(numero_expedient, .keep_all = TRUE)
cat("Duplicados eliminados:", n_antes - nrow(accidentes), "\n")


# --------------------------------------------------------------
# 8. Creacion de nuevas variables
# --------------------------------------------------------------
# Variables derivadas que resultaran utiles para el dashboard y el
# informe, y que no existen en los datos originales.

accidentes <- accidentes |>
  mutate(

    # Fecha completa, como tipo Date. Permite ordenar y agregar por tiempo.
    fecha = make_date(year = nk_any, month = mes_any, day = dia_mes),

    # Dia de la semana derivado de la fecha (mas fiable que el original,
    # que no existe en todos los anos y esta en catalan sin orden)
    dia_semana = wday(fecha, label = TRUE, abbr = FALSE, week_start = 1),

    # Variable categorica ordinal: fin de semana si / no
    fin_de_semana = if_else(wday(fecha, week_start = 1) >= 6,
                            "Fin de semana", "Laborable"),

    # Franja horaria: agrupa las 24 horas en 4 bloques interpretables
    franja_horaria = case_when(
      hora_dia >= 6  & hora_dia < 12 ~ "Manana (06-12h)",
      hora_dia >= 12 & hora_dia < 18 ~ "Tarde (12-18h)",
      hora_dia >= 18 & hora_dia < 24 ~ "Noche (18-24h)",
      hora_dia >= 0  & hora_dia < 6  ~ "Madrugada (00-06h)",
      .default = NA_character_
    ),

    # Total de personas lesionadas (leves + graves)
    total_lesionados = numero_lesionats_lleus + numero_lesionats_greus,

    # Gravedad del accidente: variable ORDINAL de tres niveles.
    # Es la variable clave del proyecto: resume en una sola dimension
    # la severidad, que en los datos originales esta repartida en
    # tres columnas de recuento distintas.
    gravedad = case_when(
      numero_morts > 0            ~ "Mortal",
      numero_lesionats_greus > 0  ~ "Grave",
      .default = "Leve"
    ),

    # Indicador binario util para calcular tasas de siniestralidad grave
    accidente_con_victimas = numero_victimes > 0
  )


# --------------------------------------------------------------
# 9. Conversion a factores ordenados
# --------------------------------------------------------------
# Los factores con orden explicito garantizan que los graficos
# respeten la secuencia logica en lugar del orden alfabetico.

accidentes <- accidentes |>
  mutate(
    gravedad = factor(gravedad,
                      levels = c("Leve", "Grave", "Mortal"),
                      ordered = TRUE),
    franja_horaria = factor(
      franja_horaria,
      levels = c("Madrugada (00-06h)", "Manana (06-12h)",
                 "Tarde (12-18h)", "Noche (18-24h)"),
      ordered = TRUE
    ),
    nom_districte = factor(nom_districte),
    nom_barri     = factor(nom_barri)
  )


# --------------------------------------------------------------
# 10. Controles de calidad finales
# --------------------------------------------------------------
# Comprobaciones que conviene documentar en el informe tecnico.

cat("\n--- RESUMEN DE LA BASE DEPURADA ---\n")
cat("Registros:", nrow(accidentes), "\n")
cat("Variables:", ncol(accidentes), "\n")
cat("Periodo:", min(accidentes$fecha, na.rm = TRUE),
    "a", max(accidentes$fecha, na.rm = TRUE), "\n\n")

cat("Accidentes por ano:\n")
print(table(accidentes$nk_any))

cat("\nAccidentes por gravedad:\n")
print(table(accidentes$gravedad))

cat("\nPorcentaje de valores faltantes por variable:\n")
accidentes |>
  summarise(across(everything(), ~ round(100 * mean(is.na(.x)), 2))) |>
  pivot_longer(everything(),
               names_to = "variable", values_to = "porcentaje_NA") |>
  filter(porcentaje_NA > 0) |>
  arrange(desc(porcentaje_NA)) |>
  print(n = Inf)


# --------------------------------------------------------------
# 11. Guardado de la base depurada
# --------------------------------------------------------------
# Guardamos en dos formatos:
#   - .rds  conserva los tipos de dato y los factores ordenados.
#           Es el que usaran el dashboard y el informe.
#   - .csv  formato abierto y universal, para cualquier persona que
#           quiera reutilizar los datos sin usar R.

ruta_destino <- "Datos/Base de datos depurada"

saveRDS(accidentes, file.path(ruta_destino, "accidentes_bcn_2019_2025.rds"))
write_csv(accidentes, file.path(ruta_destino, "accidentes_bcn_2019_2025.csv"))

cat("\nBase depurada guardada correctamente en", ruta_destino, "\n")


# ==============================================================
# NOTA SOBRE LA CODIFICACION DE CARACTERES
# ==============================================================
# Si al ejecutar el script los nombres de barrios y distritos aparecen
# con simbolos extranos (por ejemplo "Ciutat Vella" como "Ciutat Vella"),
# el problema es la codificacion del fichero original.
#
# Solucion: en la funcion leer_accidentes(), cambia la linea
#     locale = locale(encoding = "UTF-8")
# por
#     locale = locale(encoding = "latin1")
# y vuelve a ejecutar el script desde el principio.
#
# Para saber que codificacion tiene un fichero, puedes usar:
#     readr::guess_encoding("Datos/Base de datos original/accidents_2019.csv")
# ==============================================================
