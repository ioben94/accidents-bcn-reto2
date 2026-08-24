# ==============================================================
# Proyecto: Accidentes gestionados por la Guardia Urbana de Barcelona
# Nombre y Apellidos: Ioana Bendris Greab
# Reto 1 - Tarea 1: Análisis Exploratorio de Datos (EDA)
# Script 02: exploración de distribuciones y relaciones
#
# Este script produce las tablas y los graficos que sustentan el 
# informe de planificacion de la visualización y parte de la base 
# ya depurada por el script 01.
#
# Los graficos se guardan en "EDA/Graficos" en formato PNG,
# para insertarse en el informe.
#
# ORDEN DE EJECUCION:
#   1. Datos/Codigo Depuracion/01_importacion_depuracion.R
#   2. este script
# ==============================================================


# --------------------------------------------------------------
# 0. Preparación
# --------------------------------------------------------------
# install.packages(c("tidyverse", "scales"))

library(tidyverse)
library(scales)

# Corrección para que los nombres de días y meses estén en castellano

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

accidentes <- readRDS("Datos/Base de datos depurada/accidentes_bcn_2019_2025.rds") |>
  mutate(dia_semana = factor(
    as.integer(dia_semana),
    levels = 1:7,
    labels = c("Lunes", "Martes", "Miércoles", "Jueves",
               "Viernes", "Sábado", "Domingo"),
    ordered = TRUE
  ))

# Carpeta de salida para las figuras
dir.create("EDA/Graficos", recursive = TRUE, showWarnings = FALSE)


# 
# Defino el tema visual una sola vez para que todo el conjunto tenga coherencia visual.

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(color = "grey30"),
      plot.caption     = element_text(color = "grey50", size = 8),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
)

# Paleta para la variable gravedad.
# Es una escala SECUENCIAL: la severidad es una variable ordinal, de
# modo que el color debe aumentar en saturacion y oscurecerse conforme
# aumenta el valor.
col_gravedad <- c("Leve" = "#A8C4D9", "Grave" = "#E08B3C", "Mortal" = "#9E2A2B")

pie_fuente <- "Fuente: Open Data BCN — Accidents gestionats per la Guardia Urbana"


# ==============================================================
# BLOQUE 1. DESCRIPCIÓN GENERAL DEL CONJUNTO
# ==============================================================

cat("\n########## 1. ESTRUCTURA DEL CONJUNTO ##########\n\n")
cat("Registros:", nrow(accidentes), "| Variables:", ncol(accidentes), "\n\n")

# Tipo de dato de cada variable. Esta tabla alimenta directamente el
# apartado del informe donde hay que clarificar, campo a campo, de qué
# tipo de dato se trata.
tabla_variables <- tibble(
  variable = names(accidentes),
  tipo_R   = map_chr(accidentes, ~ class(.x)[1]),
  n_unicos = map_int(accidentes, n_distinct),
  n_NA     = map_int(accidentes, ~ sum(is.na(.x))),
  pct_NA   = round(100 * n_NA / nrow(accidentes), 2)
)
print(tabla_variables, n = Inf)

write_csv(tabla_variables, "EDA/Graficos/tabla_variables.csv")


# Rango y dominio de las variables numéricas
cat("\n########## 2. RANGO DE LAS VARIABLES NUMERICAS ##########\n\n")

resumen_numericas <- accidentes |>
  select(where(is.numeric)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  filter(!is.na(valor)) |>
  group_by(variable) |>
  summarise(
    minimo   = min(valor),
    q1       = quantile(valor, 0.25),
    mediana  = median(valor),
    media    = round(mean(valor), 3),
    q3       = quantile(valor, 0.75),
    maximo   = max(valor),
    .groups  = "drop"
  )
print(resumen_numericas, n = Inf)


# ==============================================================
# BLOQUE 2. DISTRIBUCIóN DE LA VARIABLE CLAVE: GRAVEDAD
# ==============================================================

cat("\n########## 3. GRAVEDAD ##########\n\n")

tabla_gravedad <- accidentes |>
  count(gravedad) |>
  mutate(porcentaje = round(100 * n / sum(n), 2))
print(tabla_gravedad)

# NOTA PARA EL INFORME: la distribución está muy desequilibrada (más
# del 97% son leves). Si represento la gravedad en valores absolutos, 
# las categorias graves resultan invisibles. Decido trabajar con 
# proporciones no con recuentos. 



# ==============================================================
# BLOQUE 3. EVOLUCIÓN TEMPORAL
# ==============================================================

cat("\n########## 4. EVOLUCIÓN ANUAL ##########\n\n")

evolucion_anual <- accidentes |>
  group_by(ano = nk_any) |>
  summarise(
    accidentes    = n(),
    graves        = sum(gravedad == "Grave"),
    mortales      = sum(gravedad == "Mortal"),
    fallecidos    = sum(numero_morts),
    pct_gravedad  = round(100 * mean(gravedad != "Leve"), 2),
    .groups = "drop"
  )
print(evolucion_anual)

# --- Grafico 1: evolución mensual del número de accidentes ---------
# Función: EVOLUCION TEMPORAL. Uso un gráfico de líneas porque el
# eje X es una variable contínua ordenada (tiempo) y también para seguir con
# el principio Gestalt de continuidad para que el ojo siga la
# trayectoria sin esfuerzo.

g1 <- accidentes |>
  mutate(mes = floor_date(fecha, "month")) |>
  count(mes) |>
  ggplot(aes(mes, n)) +
  geom_line(color = "#2E5C8A", linewidth = 0.7) +
  geom_point(size = 0.8, color = "#2E5C8A") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Accidentes mensuales en Barcelona, 2019-2025",
    subtitle = "El confinamiento de 2020 marca una caida sin recuperacion posterior",
    x = NULL, y = "Accidentes", caption = pie_fuente
  )
ggsave("EDA/Graficos/01_evolucion_mensual.png", g1, width = 9, height = 4.5, dpi = 150)


# --- Grafico 2: accidentes totales frente a proporcion de gravedad --
# Función: DESVIACIÓN / EVOLUCIÓN. Dos paneles apilados en lugar de un
# doble eje Y: los ejes dobles inducen a error porque la relación entre
# las dos escalas es arbitraria y el lector puede leer cruces
# inexistentes. Compartir el eje X mantiene la comparación sin engañar.

datos_g2 <- evolucion_anual |>
  select(ano, accidentes, pct_gravedad) |>
  pivot_longer(-ano) |>
  mutate(name = factor(name,
                       levels = c("accidentes", "pct_gravedad"),
                       labels = c("Numero de accidentes",
                                  "% accidentes graves o mortales")))

g2 <- ggplot(datos_g2, aes(ano, value)) +
  geom_col(fill = "#2E5C8A", width = 0.6) +
  facet_wrap(~ name, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = 2019:2025) +
  labs(
    title    = "Menos accidentes, pero mas graves",
    subtitle = "Los siniestros caen un 23% desde 2019 mientras la gravedad relativa aumenta",
    x = NULL, y = NULL, caption = pie_fuente
  )
ggsave("EDA/Graficos/02_volumen_vs_gravedad.png", g2, width = 8, height = 6, dpi = 150)


# --- Gráfico 3: estacionalidad mensual ------------------------------
# Función: DISTRIBUCIÓN sobre una variable CÍCLICA. El mes es un dato
# ciclico: al agregarlo entre aÑos se detectan patrones estacionales
# que la serie contínua oculta.

g3 <- accidentes |>
  count(nom_mes, mes_any) |>
  group_by(mes_any, nom_mes) |>
  summarise(media = sum(n) / 7, .groups = "drop") |>
  ggplot(aes(factor(mes_any), media)) +
  geom_col(fill = "#5B8FB9") +
  scale_x_discrete(labels = c("Ene","Feb","Mar","Abr","May","Jun",
                              "Jul","Ago","Sep","Oct","Nov","Dic")) +
  labs(
    title    = "Estacionalidad: media mensual de accidentes (2019-2025)",
    subtitle = "Agosto concentra el minimo anual coincidiendo con el periodo vacacional",
    x = NULL, y = "Accidentes (media anual)", caption = pie_fuente
  )
ggsave("EDA/Graficos/03_estacionalidad.png", g3, width = 8, height = 4.5, dpi = 150)


# ==============================================================
# BLOQUE 4. DIMENSIÓN TERRITORIAL
# ==============================================================

cat("\n########## 5. DISTRITOS ##########\n\n")

por_distrito <- accidentes |>
  filter(!is.na(nom_districte)) |>
  group_by(nom_districte) |>
  summarise(
    accidentes   = n(),
    pct_gravedad = round(100 * mean(gravedad != "Leve"), 2),
    fallecidos   = sum(numero_morts),
    .groups = "drop"
  ) |>
  arrange(desc(accidentes))
print(por_distrito)

# --- Gráfico 4: volumen por distrito --------------------------------
# Función: MAGNITUD. Barras horizontales ordenadas de mayor a menor.
# La longitud es la propiedad que mejor permite comparar
# magnitudes y el eje empieza en 0 para no distorsionar la relación
# entre barras. Ordenar convierte el grafico en un ranking legible.

g4 <- por_distrito |>
  ggplot(aes(x = accidentes, y = fct_reorder(nom_districte, accidentes))) +
  geom_col(fill = "#2E5C8A") +
  geom_text(aes(label = comma(accidentes)), hjust = -0.15, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = comma) +
  labs(
    title    = "Volumen de accidentes por distrito, 2019-2025",
    subtitle = "El Eixample concentra mas de una cuarta parte del total",
    x = "Accidentes", y = NULL, caption = pie_fuente
  )
ggsave("EDA/Graficos/04_distrito_volumen.png", g4, width = 8, height = 5, dpi = 150)


# --- Gráfico 5: gravedad relativa por distrito ----------------------
# Función: RANKING. Se usa un gráfico lollipop porque el mensaje es la
# POSICIÓN relativa de cada distrito, no la magnitud absoluta. 

media_ciudad <- 100 * mean(accidentes$gravedad != "Leve")

g5 <- por_distrito |>
  ggplot(aes(x = pct_gravedad, y = fct_reorder(nom_districte, pct_gravedad))) +
  geom_vline(xintercept = media_ciudad, linetype = "dashed", color = "grey55") +
  geom_segment(aes(x = 0, xend = pct_gravedad,
                   yend = fct_reorder(nom_districte, pct_gravedad)),
               color = "grey75") +
  geom_point(size = 4, color = "#E08B3C") +
  annotate("text", x = media_ciudad, y = 0.7,
           label = paste0("Media ciudad: ", round(media_ciudad, 2), "%"),
           hjust = -0.05, size = 3, color = "grey40") +
  labs(
    title    = "Gravedad relativa por distrito",
    subtitle = "Horta-Guinardo lidera el riesgo pese a ser el quinto en volumen",
    x = "% de accidentes graves o mortales", y = NULL, caption = pie_fuente
  )
ggsave("EDA/Graficos/05_distrito_gravedad.png", g5, width = 8, height = 5, dpi = 150)


# --- Gráfico 6: relación volumen-gravedad ---------------------------
# Función: CORRELACIÓN. Un gráfico de dispersión permite comprobar si
# los distritos con más accidentes son tambián los más peligrosos.

g6 <- por_distrito |>
  ggplot(aes(accidentes, pct_gravedad)) +
  geom_hline(yintercept = media_ciudad, linetype = "dotted", color = "grey60") +
  geom_point(size = 3, color = "#2E5C8A", alpha = 0.8) +
  geom_text(aes(label = nom_districte), vjust = -1, size = 3, color = "grey25") +
  scale_x_continuous(labels = comma, expand = expansion(mult = 0.15)) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(
    title    = "Volumen y gravedad son dimensiones independientes",
    subtitle = "Los distritos con mas accidentes no son necesariamente los mas peligrosos",
    x = "Numero de accidentes", y = "% graves o mortales", caption = pie_fuente
  )
ggsave("EDA/Graficos/06_correlacion_distrito.png", g6, width = 8, height = 5.5, dpi = 150)

cat("\nCorrelacion de Pearson entre volumen y gravedad por distrito:",
    round(cor(por_distrito$accidentes, por_distrito$pct_gravedad), 3), "\n")


# ==============================================================
# BLOQUE 5. PATRONES TEMPORALES INTRASEMANALES
# ==============================================================

cat("\n########## 6. DIA DE LA SEMANA Y HORA ##########\n\n")

por_dia <- accidentes |>
  group_by(dia_semana) |>
  summarise(
    accidentes   = n(),
    pct_gravedad = round(100 * mean(gravedad != "Leve"), 2),
    .groups = "drop"
  )
print(por_dia)

# --- Gráfico 7: mapa de calor hora x dia ----------------------------
# Funcián: DISTRIBUCIÓN en dos dimensiones categóricas. El mapa de
# calor codifica la tercera variable (frecuencia) mediante SATURACIÓN. Se usa una
# escala secuencial: a mayor valor, mayor saturación.

g7 <- accidentes |>
  filter(!is.na(dia_semana), !is.na(hora_dia)) |>
  count(dia_semana, hora_dia) |>
  ggplot(aes(hora_dia, fct_rev(dia_semana), fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#EAF2F8", high = "#1B3B5A", name = "Accidentes") +
  scale_x_continuous(breaks = seq(0, 23, 3), expand = c(0, 0)) +
  labs(
    title    = "Concentracion de accidentes por hora y dia de la semana",
    subtitle = "El pico se situa en la franja de tarde de los dias laborables",
    x = "Hora del dia", y = NULL, caption = pie_fuente
  ) +
  theme(panel.grid = element_blank())
ggsave("EDA/Graficos/07_heatmap_hora_dia.png", g7, width = 9, height = 4.5, dpi = 150)


# --- Gráfico 8: gravedad por día de la semana -----------------------
# Función: DESVIACIÓN respecto a la media de la ciudad.

g8 <- por_dia |>
  mutate(desviacion = pct_gravedad - media_ciudad) |>
  ggplot(aes(dia_semana, desviacion, fill = desviacion > 0)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0, color = "grey30") +
  scale_fill_manual(values = c("TRUE" = "#E08B3C", "FALSE" = "#7FA8C9"),
                    guide = "none") +
  labs(
    title    = "Desviacion de la gravedad respecto a la media semanal",
    subtitle = "Los fines de semana concentran proporcionalmente mas siniestros graves",
    x = NULL, y = "Puntos porcentuales sobre la media", caption = pie_fuente
  )
ggsave("EDA/Graficos/08_gravedad_dia_semana.png", g8, width = 8, height = 4.5, dpi = 150)


# ==============================================================
# BLOQUE 6. VEHICULOS IMPLICADOS Y VÍCTIMAS
# ==============================================================

cat("\n########## 7. VEHICULOS Y VÍCTIMAS ##########\n\n")

print(accidentes |> count(numero_vehicles_implicats) |> head(10))

cat("\nAccidentes con alguna victima:",
    sum(accidentes$accidente_con_victimas),
    paste0("(", round(100 * mean(accidentes$accidente_con_victimas), 1), "%)"), "\n")
cat("Total lesionados leves: ", sum(accidentes$numero_lesionats_lleus), "\n")
cat("Total lesionados graves:", sum(accidentes$numero_lesionats_greus), "\n")
cat("Total fallecidos:       ", sum(accidentes$numero_morts), "\n")

# --- Gráfico 9: distribución de vehiculos implicados ----------------
# Función: DISTRIBUCIÓN de una variable numérica discreta. Se usa un
# gráfico de barras (no un histograma) porque la variable toma pocos
# valores enteros y cada uno tiene significado propio.

g9 <- accidentes |>
  filter(numero_vehicles_implicats <= 5) |>
  count(numero_vehicles_implicats, gravedad) |>
  ggplot(aes(factor(numero_vehicles_implicats), n, fill = gravedad)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = col_gravedad, name = NULL) +
  scale_y_continuous(labels = percent) +
  labs(
    title    = "Gravedad segun el numero de vehiculos implicados",
    subtitle = "Los siniestros con un solo vehiculo presentan mayor severidad relativa",
    x = "Vehiculos implicados", y = NULL, caption = pie_fuente
  )
ggsave("EDA/Graficos/09_vehiculos_gravedad.png", g9, width = 8, height = 4.5, dpi = 150)


# ==============================================================
# BLOQUE 7. DIMENSIÓN GEOGRÁFICA
# ==============================================================

# --- Grafico 10: dispersión geográfica ------------------------------
# Función: ESPACIAL. Un mapa de símbolos sobre las coordenadas reales.
# Se representan sólo los accidentes graves y mortales porque
# dibujar 55.000 puntos produciria una mancha ilegible.

g10 <- accidentes |>
  filter(!is.na(longitud), gravedad != "Leve") |>
  ggplot(aes(longitud, latitud, color = gravedad)) +
  geom_point(alpha = 0.55, size = 1.1) +
  scale_color_manual(values = col_gravedad, name = NULL) +
  coord_fixed(ratio = 1.34) +
  labs(
    title    = "Localizacion de los accidentes graves y mortales",
    subtitle = "Los grandes ejes viarios estructuran la distribucion del riesgo",
    x = "Longitud", y = "Latitud", caption = pie_fuente
  ) +
  theme(panel.grid = element_blank(),
        axis.text  = element_text(size = 7))
ggsave("EDA/Graficos/10_mapa_puntos.png", g10, width = 8, height = 6, dpi = 150)


cat("\n########## 8. VIAS CON MAS ACCIDENTES ##########\n\n")
print(accidentes |> count(nom_carrer, sort = TRUE) |> head(10))


# ==============================================================
# FIN
# ==============================================================
cat("\nGraficos guardados en EDA/Graficos\n")
cat("Ejecucion completada:", format(Sys.time()), "\n")
