# ==============================================================
# Proyecto: Accidentes gestionados por la Guardia Urbana de Barcelona
# Reto 2 - Proyecto de Ciencia de Datos reproducible
# Script 01: importacion y depuracion de datos
#
# Fuente: Open Data BCN, Ajuntament de Barcelona
#   https://opendata-ajuntament.barcelona.cat/data/ca/dataset/accidents-gu-bcn
# Licencia de los datos: Creative Commons Attribution 4.0
# Periodo analizado: 2019-2025 (7 ficheros anuales)
#
# Este script lee los CSV anuales originales, armoniza sus diferencias
# de estructura, depura los valores problematicos y genera nuevas
# variables de utilidad. El resultado se guarda en la carpeta
# "Datos/Base de datos depurada".
#
# TODAS las rutas son relativas a la raiz del proyecto de RStudio.
# No hay ninguna referencia a directorios locales, de modo que el
# script funciona en cualquier maquina que clone el repositorio.
#
# --------------------------------------------------------------
# INCIDENCIAS DETECTADAS EN LOS DATOS ORIGINALES
# --------------------------------------------------------------
# La inspeccion previa de los siete ficheros revelo siete problemas
# que este script resuelve de forma explicita:
#
# (1) Los nombres de columna cambian entre anos. En 2019-2020 las
#     coordenadas se llaman Coordenada_UTM_X / Longitud; a partir de
#     2021 pasan a Coordenada_UTM_X_ED50 / Longitud_WGS84. Ademas
#     Num_postal_caption se convierte en "Num_postal " (con espacio
#     final) y en 2023-2025 se invierte el orden de X e Y.
#
# (2) 2019 y 2020 incluyen dos columnas que despues desaparecen:
#     Dia_setmana y Descripcio_tipus_dia.
#
# (3) EN 2024 Y 2025 LOS CEROS SE GUARDAN COMO CELDA VACIA. En los
#     anos anteriores un accidente sin fallecidos registra un 0
#     explicito; en 2024-2025 la celda queda en blanco. Si se tratan
#     esos blancos como "dato desconocido" se pierde el 97% de los
#     accidentes recientes al clasificar la gravedad.
#
# (4) El valor -1 significa "dato no disponible". Aparece en
#     Codi_districte, Codi_barri, Codi_carrer y las coordenadas UTM,
#     nunca en los recuentos de victimas.
#
# (5) Multiples campos de texto llegan con espacios sobrantes al final
#     o dobles espacios internos ("No es causa del  vianant"), lo que
#     generaria categorias duplicadas si no se normaliza.
#
# (6) EL FICHERO DE 2020 CODIFICA Codi_barri DE FORMA DISTINTA: usa
#     valores compuestos del tipo "72-7-36" mientras el resto de anos
#     usa un entero simple ("26"). El campo resulta por tanto no
#     comparable entre anos y se descarta; se conserva Nom_barri, que
#     si es homogeneo y esta completo en los siete ficheros.
#
# (7) La adivinacion automatica de tipos de read_csv corrompe el campo
#     Numero_expedient (convierte "2019S000001" en "20190"). Se evita
#     forzando la lectura de TODAS las columnas como texto y
#     convirtiendo despues cada variable a su tipo correcto.
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
# todos los CSV de la carpeta. Asi, si se anade un ano nuevo, el
# script lo recoge automaticamente sin tocar el codigo.

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
# 2. Diagnostico de estructura: que columnas cambian entre anos
# --------------------------------------------------------------
# Documentamos las diferencias antes de tocar nada. Esta salida
# justifica en el informe tecnico las decisiones de armonizacion.

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
# 3. Funcion de lectura y armonizacion
# --------------------------------------------------------------
# Lee un fichero y devuelve sus columnas con nombres homogeneos.
#
#   a) col_types = cols(.default = col_character())
#      CLAVE: desactiva la adivinacion automatica de tipos. Sin esto,
#      read_csv corrompe identificadores alfanumericos como
#      "2019S000001" y emite avisos de parsing. Los tipos correctos
#      se asignan mas adelante, de forma explicita y controlada.
#   b) codificacion UTF-8 (verificada en los 7 ficheros; los de
#      2024-2025 llevan BOM, que read_csv gestiona automaticamente)
#   c) na = c("", "NA") -> las celdas vacias entran como NA
#   d) clean_names() pasa los nombres a minusculas y sin espacios,
#      resolviendo el "Num_postal " con espacio final
#   e) renombra las columnas que cambian de nombre entre anos
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
# 4. Lectura conjunta de los siete anos
# --------------------------------------------------------------

accidentes_bruto <- map_dfr(ficheros, leer_accidentes)

cat("\n=== 3. IMPORTACION ===\n")
cat("Filas totales importadas:", nrow(accidentes_bruto), "\n")
cat("Filas por fichero:\n")
print(table(accidentes_bruto$fichero_origen))


# --------------------------------------------------------------
# 5. Normalizacion de los campos de texto
# --------------------------------------------------------------
# str_squish() elimina espacios al principio y al final y reduce los
# espacios internos multiples a uno solo. Sin este paso,
# "No es causa del  vianant" (doble espacio) y la version con espacio
# simple se contarian como dos categorias distintas.

accidentes_bruto <- accidentes_bruto |>
  mutate(across(where(is.character), str_squish))


# --------------------------------------------------------------
# 6. Comprobacion del identificador
# --------------------------------------------------------------
# Verificamos que el numero de expediente identifica cada accidente.
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
# 7. Seleccion del subconjunto de variables
# --------------------------------------------------------------
# Nos quedamos con las variables presentes en todos los anos y
# relevantes para los objetivos del proyecto. Se descartan:
#   - dia_setmana y descripcio_tipus_dia: solo en 2019-2020. El dia
#     de la semana se recreara a partir de la fecha.
#   - codi_barri: formato incompatible en 2020 (ver incidencia 6).
#     Se conserva nom_barri, homogeneo y completo.
#   - coordenadas UTM: ya disponemos de longitud/latitud en WGS84,
#     el sistema estandar para cartografia web.

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
# 8. Eliminacion de duplicados
# --------------------------------------------------------------
# Criterio conservador: solo se eliminan filas identicas en TODAS sus
# columnas. Nunca se eliminan filas por compartir identificador, para
# no destruir registros validos.

n_antes    <- nrow(accidentes)
accidentes <- accidentes |> distinct()

cat("\n=== 5. DUPLICADOS ===\n")
cat("Filas identicas eliminadas:", n_antes - nrow(accidentes), "\n")
cat("Registros restantes:       ", nrow(accidentes), "\n")


# --------------------------------------------------------------
# 9. Conversion de tipos de datos
# --------------------------------------------------------------
# Ahora que todo se ha leido como texto, asignamos a cada variable su
# tipo correcto. El tipo de dato determina que operaciones y que
# graficos son posibles, de modo que conviene fijarlo explicitamente.
#
# Se distinguen dos grupos que NO reciben el mismo trato:
#   - recuentos y codigos -> enteros
#   - coordenadas         -> numeros con decimales (nunca enteros, o
#                            se destruiria la precision geografica)

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
# 10. Depuracion: valores y etiquetas de "dato no disponible"
# --------------------------------------------------------------
# El -1 en el codigo de distrito y la etiqueta "Desconegut" en los
# nombres territoriales expresan lo mismo: ubicacion no registrada.
# Se unifican como NA para que R los excluya automaticamente de
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
# 11. Depuracion: recuentos vacios en 2024-2025
# --------------------------------------------------------------
# PASO CRITICO. En 2024 y 2025 un accidente sin fallecidos deja la
# celda en blanco en lugar de escribir un 0. Los anos anteriores si
# escriben el 0. Para que la serie temporal sea comparable, esos
# blancos se convierten en 0.
#
# La interpretacion esta respaldada por los datos: en 2025 hay 7.730
# celdas vacias y 11 con valor 1 en Numero_morts, lo que equivale a
# 11 accidentes mortales en el ano, cifra coherente con la serie
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
# 12. Depuracion: coordenadas geograficas
# --------------------------------------------------------------
# Barcelona se situa aproximadamente entre 2.0 y 2.3 de longitud y
# entre 41.3 y 41.5 de latitud. Cualquier valor fuera de ese
# rectangulo (incluidos los -1) es un error de registro.

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
# 13. Depuracion: causa del peaton
# --------------------------------------------------------------
# En 2024-2025 el campo queda vacio cuando el peaton no interviene;
# en los anos anteriores se escribe "No es causa del vianant".
# Se unifica el criterio y se crea ademas una variable logica, mas
# comoda para filtrar y para calcular porcentajes.

accidentes <- accidentes |>
  mutate(
    descripcio_causa_vianant = coalesce(descripcio_causa_vianant,
                                        "No es causa del vianant"),
    es_causa_vianant = !str_detect(descripcio_causa_vianant,
                                   regex("no .s causa", ignore_case = TRUE))
  )


# --------------------------------------------------------------
# 14. Creacion de nuevas variables
# --------------------------------------------------------------
# Variables derivadas utiles para el dashboard y el informe, que no
# existen en los datos originales.

accidentes <- accidentes |>
  mutate(

    # Fecha completa como tipo Date: permite ordenar y agregar por tiempo
    fecha = make_date(year = nk_any, month = mes_any, day = dia_mes),

    # Dia de la semana derivado de la fecha. Mas fiable que el campo
    # original, que esta en catalan y sin orden definido.
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

    # GRAVEDAD: variable ordinal de tres niveles. Es la variable clave
    # del proyecto, porque resume en una sola dimension la severidad
    # que los datos originales reparten en tres columnas de recuento.
    gravedad = case_when(
      numero_morts           > 0 ~ "Mortal",
      numero_lesionats_greus > 0 ~ "Grave",
      .default = "Leve"
    ),

    # Indicador binario, util para calcular tasas de siniestralidad
    accidente_con_victimas = numero_victimes > 0
  )


# --------------------------------------------------------------
# 15. Conversion a factores ordenados
# --------------------------------------------------------------
# Los factores con orden explicito garantizan que los graficos
# respeten la secuencia logica en lugar del orden alfabetico.

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
# 16. Controles de calidad finales
# --------------------------------------------------------------
# Comprobaciones que conviene reproducir en el informe tecnico.

cat("\n===============================================\n")
cat("       RESUMEN DE LA BASE DEPURADA\n")
cat("===============================================\n")
cat("Registros:", nrow(accidentes), "\n")
cat("Variables:", ncol(accidentes), "\n")
cat("Periodo:", format(min(accidentes$fecha, na.rm = TRUE)),
    "a",       format(max(accidentes$fecha, na.rm = TRUE)), "\n")

cat("\nAccidentes por ano:\n")
print(table(accidentes$nk_any))

cat("\nAccidentes por gravedad:\n")
print(table(accidentes$gravedad))

cat("\nGravedad por ano:\n")
print(table(accidentes$nk_any, accidentes$gravedad))

cat("\nDistritos detectados (comprobar acentos):\n")
print(levels(accidentes$nom_districte))

cat("\nPorcentaje de valores faltantes por variable:\n")
accidentes |>
  summarise(across(everything(), ~ round(100 * mean(is.na(.x)), 2))) |>
  pivot_longer(everything(),
               names_to = "variable", values_to = "porcentaje_NA") |>
  filter(porcentaje_NA > 0) |>
  arrange(desc(porcentaje_NA)) |>
  print(n = Inf)


# --------------------------------------------------------------
# 17. Guardado de la base depurada
# --------------------------------------------------------------
# Dos formatos complementarios:
#   - .rds  conserva tipos de dato y factores ordenados. Es el que
#           usaran el dashboard, el informe y la presentacion.
#   - .csv  formato abierto y universal, para cualquier persona que
#           quiera reutilizar los datos sin usar R.

saveRDS(accidentes, file.path(ruta_destino, "accidentes_bcn_2019_2025.rds"))
write_csv(accidentes, file.path(ruta_destino, "accidentes_bcn_2019_2025.csv"))

cat("\nBase depurada guardada en", ruta_destino, "\n")
cat("Fecha de ejecucion:", format(Sys.time()), "\n")


# ==============================================================
# FIN DEL SCRIPT
#
# Salida esperada (verificada sobre los ficheros originales):
#   Filas importadas ......... 54.954
#   Expedientes unicos ....... 54.928  (formato 2019S000001)
#   Registros finales ........ 54.929
#   Accidentes por ano ....... 2019: 10.020   2020: 6.266
#                              2021:  7.658   2022: 7.996
#                              2023:  7.720   2024: 7.532
#                              2025:  7.737
#   Valores faltantes ........ solo nom_districte, nom_barri (~0,4%)
#                              y longitud/latitud (~0,08%)
#   Sin avisos de parsing.
#
# Si tus cifras no coinciden, revisa que en la carpeta de origen esten
# los siete CSV y ninguno mas.
# ==============================================================
