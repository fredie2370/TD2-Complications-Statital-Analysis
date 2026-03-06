# Frecuencia y Complicaciones de Diabetes Mellitus Tipo 2
### Hospital General de Valle de Bravo — Primer Semestre 2024

[![R](https://img.shields.io/badge/R-4.4.2-blue?logo=r)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Status](https://img.shields.io/badge/Estado-En%20desarrollo-orange)]()

Repositorio de analisis estadistico para tesis de especialidad medica.
Estudio observacional, retrospectivo, transversal y descriptivo-analitico.

> **AVISO DE PRIVACIDAD:** Este repositorio NO contiene datos clinicos.
> Los archivos de datos (`data/raw/`) estan excluidos del control de versiones
> en cumplimiento con la NOM-024-SSA3-2010 y la Ley Federal de Proteccion
> de Datos Personales en Posesion de los Particulares.

---

## Descripcion del Estudio

| Parametro            | Detalle                                          |
|----------------------|--------------------------------------------------|
| Diseno               | Observacional, retrospectivo, transversal        |
| Estadistica          | Descriptivo-analitico (censo)                    |
| Periodo              | Enero - Junio 2024                               |
| Sede                 | Hospital General de Valle de Bravo, ISEM         |
| Universo             | 531 pacientes unicos atendidos                   |
| N incluidos          | 233 pacientes con DM2 que cumplen criterios      |
| Software             | R version 4.4.2                                  |
| Nivel significancia  | alfa = 0.05 / IC al 95%                          |

---

## Objetivos del Analisis

1. Frecuencia hospitalaria de DM2 con IC 95%
2. Estadistica descriptiva de variables demograficas, clinicas y de laboratorio
3. Analisis bivariado:
   - Chi-cuadrada / Fisher exacto + OR crudos (variables categoricas)
   - U de Mann-Whitney (variables continuas vs complicacion)
   - Correlacion de Spearman + ajuste Holm (entre variables continuas)
4. Regresion logistica binaria para cada complicacion cronica
5. Curvas ROC de biomarcadores individuales con Indice de Youden

---

## Estructura del Repositorio

```
dm2-hgvb-2024/
|-- R/
|   |-- 00_setup.R          # Librerias, constantes, semilla aleatoria
|   |-- 01_limpieza.R       # Carga, transformacion, QC, saveRDS
|   |-- 02_descriptiva.R    # Tablas 1-3, graficas 1-4, prevalencias
|   |-- 03_bivariado.R      # Tablas 4-6, graficas 5-8
|   |-- 04_regresion.R      # Tablas 7-8, forest plots, curvas ROC
|   `-- 05_roc.R            # Tabla 9, graficas 11-16, biomarcadores
|-- data/
|   |-- raw/                # [.gitignore] Datos originales Excel
|   `-- processed/          # [.gitignore] df_clean.rds
|-- output/
|   |-- tables/             # [.gitignore] Tablas Word regenerables
|   `-- figures/            # [.gitignore] Figuras PNG 300dpi
|-- .gitignore
|-- README.md
|-- renv.lock
`-- LICENSE
```

---

## Instalacion

### Con renv (recomendado)

```r
install.packages("renv")
renv::restore()
```

### Manual

```r
install.packages(c(
  "here", "readxl", "dplyr", "tidyr", "stringr", "ggplot2", "scales",
  "gtsummary", "flextable", "officer", "janitor", "skimr",
  "ggpubr", "corrplot", "rstatix", "car", "ResourceSelection",
  "pROC", "broom", "MASS"
))
```

---

## Uso

### Preparacion

1. Colocar el archivo Excel original en `data/raw/`
2. Verificar que la hoja se llame exactamente `"DIABETICOS"`
3. La primera fila del Excel es el encabezado agrupador (se omite con `skip=1`)

### Ejecucion

```r
source("R/00_setup.R")       # Siempre primero
source("R/01_limpieza.R")    # Genera df_clean.rds
source("R/02_descriptiva.R") # Tablas 1-3, graficas 1-4
source("R/03_bivariado.R")   # Tablas 4-6, graficas 5-8
source("R/04_regresion.R")   # Tablas 7-8, forest plots, ROC
source("R/05_roc.R")         # Tabla 9, heatmap AUC
```

---

## Metodologia Estadistica

| Analisis               | Metodo                        | Paquete R          |
|------------------------|-------------------------------|--------------------|
| Normalidad             | Shapiro-Wilk                  | stats (base)       |
| IC proporciones        | Clopper-Pearson exacto        | stats (base)       |
| Categoricas vs outcome | Chi-cuadrada / Fisher exacto  | stats (base)       |
| OR crudos              | Wald (tablas 2x2)             | calculado          |
| Continuas vs outcome   | U de Mann-Whitney             | stats (base)       |
| Correlacion            | Spearman + ajuste Holm        | rstatix            |
| Regresion logistica    | Stepwise backward AIC         | MASS               |
| Colinealidad           | VIF < 5                       | car                |
| Bondad de ajuste       | Hosmer-Lemeshow (g=10)        | ResourceSelection  |
| R2 del modelo          | Nagelkerke                    | calculado          |
| AUC del modelo         | DeLong IC 95%                 | pROC               |
| Corte optimo           | Indice de Youden              | pROC::coords       |
| Semilla aleatoria      | set.seed(2024)                | base               |

---

## Paquetes Requeridos

| Paquete           | Version min | Uso                                     |
|-------------------|-------------|----------------------------------------|
| here              | 1.0.1       | Rutas relativas portables               |
| readxl            | 1.4.3       | Leer Excel                              |
| dplyr             | 1.1.4       | Manipulacion de datos                   |
| ggplot2           | 3.5.0       | Graficas                                |
| gtsummary         | 2.0.0       | Tablas descriptivas y comparativas      |
| flextable         | 0.9.5       | Exportar tablas a Word                  |
| pROC              | 1.18.5      | Curvas ROC, AUC, DeLong, Youden        |
| car               | 3.1.2       | VIF                                     |
| ResourceSelection | 0.3.5       | Hosmer-Lemeshow                         |
| corrplot          | 0.94        | Heatmap de correlaciones                |
| rstatix           | 0.7.2       | Spearman tidy                           |
| MASS              | 7.3.60      | stepAIC                                 |

---

## Salidas Generadas

| Archivo                             | Contenido                              |
|-------------------------------------|----------------------------------------|
| Tabla_1_Caracteristicas_Generales   | Caracteristicas de la muestra          |
| Tabla_2_Comparacion_por_Sexo        | Comparacion por sexo con p-valor       |
| Tabla_3_Prevalencias_Complicaciones | Frecuencias con IC 95%                 |
| Tabla_4_Chi2_Fisher                 | Asociaciones categoricas + OR crudos   |
| Tabla_5a-d_MannWhitney              | Variables continuas por complicacion   |
| Tabla_6_Correlaciones_Spearman      | Matriz de correlaciones + p ajustado   |
| Tabla_Regresion_[complicacion]      | OR ajustados + validacion del modelo   |
| Tabla_8_Resumen_Modelos             | AUC, R2, HL de todos los modelos       |
| Tabla_9_ROC_Biomarcadores           | AUC + Youden por biomarcador           |
| Grafica1-16                         | Figuras para resultados de tesis       |

---

## Cita

```
[Autor]. (2026). Analisis estadistico: Frecuencia y complicaciones
de DM2, Hospital General de Valle de Bravo 2024 [Software].
GitHub: https://github.com/[usuario]/dm2-hgvb-2024
```

---

## Contacto

**Investigador principal:** [Nombre completo]
**Director de tesis:** [Nombre completo]
**Institucion:** Hospital General de Valle de Bravo, ISEM
**Facultad:** Facultad de Medicina, UAEMex

---

## Licencia

MIT License. Ver archivo LICENSE para detalles.
Los datos clinicos no se distribuyen y no forman parte del repositorio.
