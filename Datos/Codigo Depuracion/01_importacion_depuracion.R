# ==============================================================
# Proyecto: Accidentes gestionados por la Guardia Urbana de Barcelona
# Nombre y Apellidos: Ioana Bendris Greab
# Script 01: importación y depuración de datos
#
# Fuente: Open Data BCN, Ajuntament de Barcelona
#   https://opendata-ajuntament.barcelona.cat/data/ca/dataset/accidents-gu-bcn
# Licencia de los datos: Creative Commons Attribution 4.0
# Período analizado: 2019-2025 (7 ficheros, uno para cada año)
#
# Este script (1) lee los CSV anuales originales, (2) armoniza sus diferencias
# de estructura, (3) depura los valores problemáticos y (4) genera nuevas
# variables. 
# El resultado se guarda en la carpeta "Datos/Base de datos depurada".
#
# El script funciona en cualquier máquina que clone el repositorio.
#
# --------------------------------------------------------------
# QUÉ SE HA DETECTADO EN LOS DATOS ORIGINALES?
# --------------------------------------------------------------
# La inspección previa de los siete ficheros reveló siete problemas
# y que este script se resuelven:
#
# (1) Los nombres de columna cambian entre años. En 2019-2020 las
#     coordenadas se llaman Coordenada_UTM_X / Longitud; a partir de
#     2021 pasan a Coordenada_UTM_X_ED50 / Longitud_WGS84. Además
#     Num_postal_caption se convierte en "Num_postal " (con espacio
#     final) y en 2023-2025 se invierte el orden de X e Y.
#
# (2) 2019 y 2020 incluyen dos columnas que después desaparecen:
#     Dia_setmana y Descripcio_tipus_dia.
#
# (3) EN 2024 Y 2025 LOS CEROS SE GUARDAN COMO CELDA VACIA. En los
#     años anteriores un accidente sin fallecidos registra un 0;
#     en 2024-2025 la celda queda en blanco. Si se tratan
#     esos blancos como "dato desconocido" se pierde el 97% de los
#     accidentes recientes al clasificar la gravedad.
#
# (4) El valor -1 significa "dato no disponible". Aparece en
#     Codi_districte, Codi_barri, Codi_carrer y las coordenadas UTM,
#     nunca en los recuentos de víctimas.
#
# (5) Multiples campos de texto llegan con espacios sobrantes al final
#     o dobles espacios internos ("No es causa del  vianant"), lo que
#     generaría categorias duplicadas si no se normaliza.
#
# (6) EL FICHERO DE 2020 CODIFICA Codi_barri DE FORMA DISTINTA: usa
#     valores compuestos del tipo "72-7-36" mientras el resto de añoos
#     usa un entero simple ("26"). El campo resulta por tanto no
#     comparable entre años y se descarta; se conserva Nom_barri, que
#     si es homogéneo y está completo en los siete ficheros.
#
# (7) La adivinación automática de tipos de read_csv corrompe el campo
#     Numero_expedient (convierte "2019S000001" en "20190"). Lo he evitado
#     forzando la lectura de TODAS las columnas como texto y
#     convirtiendo después cada variable a su tipo correcto.
# ==============================================================


# --------------------------------------------------------------
# 0. Paquetes necesarios
# --------------------------------------------------------------
# install.packages(c("tidyverse", "janitor"))

library(tidyverse)   # lectura, manipulacion y visualizacion de datos
library(janitor)     # limpieza de nombres de columna


# --------------------------------------------------------------
# 1. Localizacion de los ficheros originales
# --------------------------------------------------------------
# En lugar de escribir las siete rutas a mano, pido a R que busque
# todos los CSV de la carpeta. Así, si se anade un añoo nuevo, el
# script lo recoge automáticamente sin tener que tocar el código.

ruta_origen  <- "Datos/Base de datos original"
ruta_destino <- "Datos/Base de datos depurada"

ficheros <- list.files(
  path       = ruta_origen,
  pattern    = "\\.csv$",   # solo ficheros terminados en .csv
  full.names = TRUE         # ruta completa, no solo el nombre
)

cat("=== 1. FICHEROS LOCALIZADOS ===\n")
cat("Encontrados:", length(ficheros), "\n")
print(basename(ficheros))


# --------------------------------------------------------------
# 2. Diagnostico de la estructura: qué columnas cambian entre añoos
# --------------------------------------------------------------
# Primero documento las diferencias antes de tocar nada. 

nombres_por_fichero <- map(ficheros, function(ruta) {
  names(read_csv(ruta, n_max = 0, show_col_types = FALSE))
})
names(nombres_por_fichero) <- basename(ficheros)

columnas_en_todos     <- reduce(nombres_por_fichero, intersect)
columnas_en_alguno    <- reduce(nombres_por_fichero, union)
columnas_conflictivas <- setdiff(columnas_en_alguno, columnas_en_todos)

cat("\n=== 2. ESTRUCTURA DE LOS FICHEROS ===\n")
cat("Columnas comunes a todos los anos:", length(columnas_en_todos), "\n")
cat("Columnas que NO coinciden entre anos:\n")
print(columnas_conflictivas)


# --------------------------------------------------------------
# 3. Función de lectura y armonización
# --------------------------------------------------------------
# Lee un fichero y devuelve sus columnas con nombres homogéneos.
#
#   a) col_types = cols(.default = col_character())
#      CLAVE: desactiva la adivinacion automatica de tipos. Sin esto,
#      read_csv corrompe identificadores alfanumericos como
#      "2019S000001" y emite avisos de parsing. Los tipos correctos
#      se asignan más adelante, de forma explícita y controlada.
#   b) codificación UTF-8 (verificada en los 7 ficheros; los de
#      2024-2025 llevan BOM, que read_csv gestiona automaticamente)
#   c) na = c("", "NA") -> las celdas vacias entran como NA
#   d) clean_names() pasa los nombres a minusculas y sin espacios,
#      resolviendo el "Num_postal " con espacio final
#   e) renombra las columnas que cambian de nombre entre años
#   f) registra el fichero de origen de cada fila (trazabilidad)

leer_accidentes <- function(ruta) {

  read_csv(
    file           = ruta,
    col_types      = cols(.default = col_character()),
    locale         = locale(encoding = "UTF-8"),
    na             = c("", "NA"),
    progress       = FALSE
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
    mutate(fichero_origen = basename(ruta))
}


# --------------------------------------------------------------
# 4. Lectura de los siete años
# --------------------------------------------------------------

accidentes_bruto <- map_dfr(ficheros, leer_accidentes)

cat("\n=== 3. IMPORTACION ===\n")
cat("Filas totales importadas:", nrow(accidentes_bruto), "\n")
cat("Filas por fichero:\n")
print(table(accidentes_bruto$fichero_origen))


# --------------------------------------------------------------
# 5. Normalización de los campos de texto
# --------------------------------------------------------------
# str_squish() elimina espacios al principio y al final y reduce los
# espacios internos múltiples a uno solo. 

accidentes_bruto <- accidentes_bruto |>
  mutate(across(where(is.character), str_squish))


# --------------------------------------------------------------
# 6. Comprobación del identificador
# --------------------------------------------------------------
# Verifico que el número de expediente identifica cada accidente.
# No se elimina ninguna fila en este paso.

n_filas       <- nrow(accidentes_bruto)
n_expedientes <- n_distinct(accidentes_bruto$numero_expedient)

cat("\n=== 4. IDENTIFICADOR ===\n")
cat("Filas:                       ", n_filas, "\n")
cat("Numeros de expediente unicos:", n_expedientes, "\n")
cat("Ejemplos:", head(accidentes_bruto$numero_expedient, 3), "\n")
cat("(deben tener el formato 2019S000001; si aparecen truncados,\n")
cat(" la lectura como texto no se ha aplicado correctamente)\n")


# --------------------------------------------------------------
# 7. Selección del subconjunto de variables
# --------------------------------------------------------------
# Me quedo con las variables presentes en todos los años y
# relevantes para los objetivos del proyecto. Se descartan:
#   - dia_setmana y descripcio_tipus_dia: solo en 2019-2020. El día
#     de la semana la consigo a partir de la fecha.
#   - codi_barri: formato incompatible en 2020 (ver incidencia 6).
#     Opto por consevar nom_barri, homogéneo y completo.
#   - coordenadas UTM: ya disponemos de longitud/latitud en WGS84,
#     el sistema estándar para cartografia web.

variables_utiles <- c(
  "numero_expedient",            # identificador del accidente
  "codi_districte", "nom_districte",
  "nom_barri",
  "nom_carrer",
  "nk_any", "mes_any", "nom_mes", "dia_mes", "hora_dia",
  "descripcio_dia_setmana",
  "descripcio_torn",
  "descripcio_causa_vianant",
  "numero_morts",
  "numero_lesionats_lleus",
  "numero_lesionats_greus",
  "numero_victimes",
  "numero_vehicles_implicats",
  "longitud", "latitud",
  "fichero_origen"
)

accidentes <- accidentes_bruto |>
  select(all_of(variables_utiles))


# --------------------------------------------------------------
# 8. Eliminación de duplicados
# --------------------------------------------------------------
# Criterio conservador: solo eliminaré filas identicas en TODAS sus
# columnas. No eliminaré filas por compartir identificador, para
# no destruir registros válidos.

n_antes    <- nrow(accidentes)
accidentes <- accidentes |> distinct()

cat("\n=== 5. DUPLICADOS ===\n")
cat("Filas identicas eliminadas:", n_antes - nrow(accidentes), "\n")
cat("Registros restantes:       ", nrow(accidentes), "\n")


# --------------------------------------------------------------
# 9. Conversión de tipos de datos
# --------------------------------------------------------------
# Ahora que todo se ha leído como texto, asigno a cada variable su
# tipo correcto. 
#
# Hay dos grupos que no reciben el mismo trato:
#   - recuentos y codigos -> enteros
#   - coordenadas         -> numeros con decimales (nunca enteros, sinó
#                            se destruiría la precisión geográfica)

variables_enteras <- c(
  "nk_any", "mes_any", "dia_mes", "hora_dia",
  "codi_districte",
  "numero_morts", "numero_lesionats_lleus", "numero_lesionats_greus",
  "numero_victimes", "numero_vehicles_implicats"
)

accidentes <- accidentes |>
  mutate(
    across(all_of(variables_enteras), ~ suppressWarnings(as.integer(.x))),
    across(c(longitud, latitud),      ~ suppressWarnings(as.numeric(.x)))
  )


# --------------------------------------------------------------
# 10. Depuración: valores y etiquetas de "dato no disponible"
# --------------------------------------------------------------
# El -1 en el código de distrito y la etiqueta "Desconegut" en los
# nombres territoriales indican que la ubicacion no está registrada.
# Se unifican como NA para que R los excluya automáticamente de
# medias, mapas y tablas en lugar de tratarlos como una zona real.

accidentes <- accidentes |>
  mutate(
    codi_districte = if_else(codi_districte == -1L, NA_integer_, codi_districte),
    nom_districte  = if_else(nom_districte %in% c("Desconegut", "Desconeguda"),
                             NA_character_, nom_districte),
    nom_barri      = if_else(nom_barri %in% c("Desconegut", "Desconeguda"),
                             NA_character_, nom_barri)
  )


# --------------------------------------------------------------
# 11. Depuracióon: recuentos vacios en 2024-2025
# --------------------------------------------------------------
# PASO MUY IMPORTANTE. En 2024 y 2025 un accidente sin fallecidos deja la
# celda en blanco en lugar de escribir un 0. Los años anteriores sí
# escriben el 0. Para que la serie temporal sea comparable, esos
# blancos se convierten en 0.
#
# La interpretación está respaldada por los datos: en 2025 hay 7.730
# celdas vacias y 11 con valor 1 en Numero_morts, lo que equivale a
# 11 accidentes mortales en el año, cifra coherente con la serie
# historica de la ciudad.

variables_recuento <- c(
  "numero_morts", "numero_lesionats_lleus", "numero_lesionats_greus",
  "numero_victimes", "numero_vehicles_implicats"
)

cat("\n=== 6. RECUENTOS VACIOS ANTES DE IMPUTAR ===\n")
accidentes |>
  group_by(ano = nk_any) |>
  summarise(across(all_of(variables_recuento), ~ sum(is.na(.x))), .groups = "drop") |>
  print()

accidentes <- accidentes |>
  mutate(across(all_of(variables_recuento), ~ coalesce(.x, 0L)))


# --------------------------------------------------------------
# 12. Depuración: coordenadas geográficas
# --------------------------------------------------------------
# Barcelona se situa aproximadamente entre 2.0 y 2.3 de longitud y
# entre 41.3 y 41.5 de latitud. Cualquier valor fuera de ese
# rectángulo (incluidos los -1) es un error de registro.

n_coord_antes <- sum(!is.na(accidentes$longitud))

accidentes <- accidentes |>
  mutate(
    longitud = if_else(between(longitud, 1.9, 2.4),  longitud, NA_real_),
    latitud  = if_else(between(latitud, 41.2, 41.6), latitud,  NA_real_)
  )

cat("\n=== 7. COORDENADAS ===\n")
cat("Coordenadas invalidadas por caer fuera de Barcelona:",
    n_coord_antes - sum(!is.na(accidentes$longitud)), "\n")


# --------------------------------------------------------------
# 13. Depuración: causa del peatón
# --------------------------------------------------------------
# En 2024-2025 el campo queda vacio cuando el peatón no interviene;
# en los años anteriores se escribe "No es causa del vianant".
# Decido unificar el criterio.

accidentes <- accidentes |>
  mutate(
    descripcio_causa_vianant = coalesce(descripcio_causa_vianant,
                                        "No es causa del vianant"),
    es_causa_vianant = !str_detect(descripcio_causa_vianant,
                                   regex("no .s causa", ignore_case = TRUE))
  )


# --------------------------------------------------------------
# 14. Creación de nuevas variables
# --------------------------------------------------------------
# Variables derivadas útiles para el dashboard y el informe, que no
# existen en los datos originales.

accidentes <- accidentes |>
  mutate(

    # Fecha completa como tipo Date: para ordenar y agregar por tiempo
    fecha = make_date(year = nk_any, month = mes_any, day = dia_mes),

    # Dia de la semana derivado de la fecha. Más fiable que el campo
    # original.
    dia_semana = wday(fecha, label = TRUE, abbr = FALSE, week_start = 1),

    # Fin de semana si / no
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

    # Total de personas lesionadas
    total_lesionados = numero_lesionats_lleus + numero_lesionats_greus,

    # Gravedad: variable ordinal de tres niveles. Es una variable clave,
    # porque resume en una sola dimensión la severidad
    # que los datos originales reparten en tres columnas de recuento.
    gravedad = case_when(
      numero_morts           > 0 ~ "Mortal",
      numero_lesionats_greus > 0 ~ "Grave",
      .default = "Leve"
    ),

    # Indicador binario, para calcular tasas de siniestralidad
    accidente_con_victimas = numero_victimes > 0
  )


# --------------------------------------------------------------
# 15. Conversión a factores ordenados
# --------------------------------------------------------------
# Los factores con orden explícito aseguran que los gráficos
# respeten la secuencia lógica en lugar del orden alfabético.

accidentes <- accidentes |>
  mutate(
    gravedad = factor(gravedad,
                      levels  = c("Leve", "Grave", "Mortal"),
                      ordered = TRUE),
    franja_horaria = factor(
      franja_horaria,
      levels  = c("Madrugada (00-06h)", "Manana (06-12h)",
                  "Tarde (12-18h)", "Noche (18-24h)"),
      ordered = TRUE
    ),
    descripcio_torn = factor(descripcio_torn,
                             levels  = c("Matí", "Tarda", "Nit"),
                             ordered = TRUE),
    nom_districte   = factor(nom_districte),
    nom_barri       = factor(nom_barri),
    fin_de_semana   = factor(fin_de_semana,
                             levels = c("Laborable", "Fin de semana"))
  )


# --------------------------------------------------------------
# 16. Guardado de la base depurada
# --------------------------------------------------------------
# Dos formatos complementarios:
#   - .rds  Lo usaré para el dashboard, el informe y la presentacion.

#   - .csv  formato abierto y universal, para cualquier persona que
#           quiera reutilizar los datos sin usar R.

saveRDS(accidentes, file.path(ruta_destino, "accidentes_bcn_2019_2025.rds"))
write_csv(accidentes, file.path(ruta_destino, "accidentes_bcn_2019_2025.csv"))

cat("\nBase depurada guardada en", ruta_destino, "\n")
cat("Fecha de ejecucion:", format(Sys.time()), "\n")


# ==============================================================

