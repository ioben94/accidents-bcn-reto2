# Siniestralidad vial en Barcelona (2019-2025)

Proyecto de Ciencia de Datos reproducible sobre los accidentes de tráfico
gestionados por la Guàrdia Urbana de Barcelona.

Ioana Bendris Greab - 
Data Visualization y Reproducibility - 
2025-2026

---

## 1. Objetivos del proyecto

### Objetivo principal

Analizar la evolución y la distribución de la siniestralidad vial gestionada
por la Guàrdia Urbana de Barcelona entre 2019 y 2025, para
identificar en qué medida la reducción del número de accidentes se ha
acompañado de una reducción equivalente de su gravedad y qué zonas y
momentos concentran el riesgo más alto.

### Objetivos específicos

1. **Evolución temporal.** Describir la evolución del número de accidentes y
   de su gravedad, distinguiendo el efecto puntual de la pandemia de la
   tendencia de fondo posterior.

2. **Comparación territorial.** Comparar la siniestralidad entre los diez
   distritos, tanto en volumen absoluto como en proporción de accidentes
   graves, para detectar territorios con perfiles de riesgo distintos.

3. **Patrones intrasemanales.** Caracterizar los patrones temporales
   cruzando hora del día con día de la semana, e identificar las franjas de
   mayor concentración de siniestros.

4. **Localización del riesgo.** Localizar geográficamente los puntos de mayor
   acumulación de accidentes graves dentro de la ciudad.

5. **Calidad del dato.** Evaluar la calidad y las limitaciones del conjunto
   de datos, documentando los cambios de criterio de registro entre
   años y cómo ha efectado la comparabilidad de la serie.

---

## 2. Origen de los datos

| | |
|---|---|
| **Fuente** | [Open Data BCN — Ajuntament de Barcelona](https://opendata-ajuntament.barcelona.cat/data/ca/dataset/accidents-gu-bcn) |
| **Conjunto** | Accidents gestionats per la Guàrdia Urbana a la ciutat de Barcelona |
| **Licencia** | Creative Commons Attribution 4.0 |
| **Periodo** | 2019-2025 (siete ficheros CSV anuales) |
| **Registros** | 54.954 accidentes |
| **Unidad de análisis** | Un accidente registrado |
| **Actualización** | Anual |

Los ficheros originales se conservan sin modificar dentro del repositorio para
garantizar que el proyecto sea reproducible con independencia de la
disponibilidad futura del portal.

---

## 3. Estructura del repositorio

```
Repositorio
├── README.md
├── Datos
│   ├── Base de datos original      Los 7 CSV anuales tal como se descargaron
│   ├── Codigo Depuracion           Script de importación y depuración
│   └── Base de datos depurada      Base final en formato .rds y .csv
├── Dashboard
│   ├── Codigo
│   └── HTML
├── Informe
│   ├── Codigo
│   └── PDF
└── Presentacion
    ├── Codigo
    └── PDF, HTML o similar
```

---

## 4. Cómo reproducir el proyecto

1. Clonar el repositorio.
2. Abrir el fichero `accidents-bcn-reto2.Rproj` con RStudio.
3. Instalar las dependencias:

   ```r
   install.packages(c("tidyverse", "janitor"))
   ```

4. Ejecutar el script de depuración:

   ```r
   source("Datos/Codigo Depuracion/01_importacion_depuracion.R")
   ```

Todas las rutas del proyecto son relativas a la raíz, de modo que no existe
ninguna referencia a directorios locales.

---

## 5. Principales incidencias detectadas en los datos originales

El proceso de depuración reveló varios problemas de homogeneidad entre los
ficheros anuales. Se documentan aquí porque condicionan la comparabilidad de
la serie temporal y forman parte de los resultados del proyecto (objetivo
específico 5).

| Incidencia | Años afectados | Tratamiento aplicado |
|---|---|---|
| Cambios en los nombres de columna (coordenadas, número postal) | Todos | Renombrado y armonización |
| Columnas `Dia_setmana` y `Descripcio_tipus_dia` desaparecen | 2021-2025 | Descartadas; día de la semana recalculado a partir de la fecha |
| **Los ceros se registran como celda vacía** | 2024-2025 | Imputados como 0 tras verificar coherencia con la serie histórica |
| Valor `-1` como código de dato no disponible | 2019-2023 | Convertido a `NA` |
| Formato incompatible en `Codi_barri` (`72-7-36` frente a `26`) | 2020 | Campo descartado; se conserva `Nom_barri` |
| Adivinación automática de tipos que corrompe el identificador | Todos | Lectura forzada como texto |
| Espacios sobrantes y dobles espacios en campos de texto | Todos | Normalizados con `str_squish()` |

La incidencia más relevante es la tercera porque si las celdas vacías de 2024-2025
se interpretaran como dato desconocido, perdería el 97% de los accidentes
de esos dos años al clasificar la gravedad.

---

## 6. Variables de la base depurada

Además de las variables originales, el script genera las siguientes:

| Variable | Tipo | Descripción |
|---|---|---|
| `fecha` | Fecha | Fecha completa del accidente |
| `dia_semana` | Categórica ordinal | Día de la semana, derivado de la fecha |
| `fin_de_semana` | Categórica nominal | Laborable / Fin de semana |
| `franja_horaria` | Categórica ordinal | Madrugada, mañana, tarde, noche |
| `total_lesionados` | Numérica discreta | Suma de lesionados leves y graves |
| `gravedad` | Categórica ordinal | Leve / Grave / Mortal |
| `accidente_con_victimas` | Lógica | Indica si hubo alguna víctima |
| `es_causa_vianant` | Lógica | Indica si el peatón fue causa del accidente |

---

## 7. Limitaciones

El conjunto de datos recoge únicamente los accidentes gestionados por la
Guàrdia Urbana, de modo que no incluye los siniestros no reportados. Al no
disponerse de datos de intensidad de tráfico ni de desplazamientos, no es
posible calcular tasas de riesgo por vehículo-kilómetro, así que las conclusiones se
refieren a accidentes registrados, no a riesgo real por desplazamiento.
