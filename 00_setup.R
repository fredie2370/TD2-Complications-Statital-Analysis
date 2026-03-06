# =============================================================================
# 00_setup.R — Instalación y carga de todas las librerías del proyecto
# =============================================================================
# Ejecutar este script UNA VEZ antes de correr cualquier bloque.
# =============================================================================

# --- Instalar paquetes faltantes automáticamente ---
paquetes_necesarios <- c(
  "readxl",           # Leer archivos Excel
  "dplyr",            # Manipulación de datos (tidyverse)
  "tidyr",            # Transformación / pivot de datos
  "stringr",          # Manejo de texto y expresiones regulares
  "ggplot2",          # Gráficas (Grammar of Graphics)
  "gtsummary",        # Tablas estadísticas para publicación
  "flextable",        # Exportar tablas a Word (.docx)
  "officer",          # Soporte Word/PowerPoint (requerido por flextable)
  "janitor",          # Limpieza de nombres de columnas
  "skimr",            # Resumen estadístico rápido del data.frame
  "scales",           # Formato de ejes en ggplot2
  "ggpubr",           # Añadir p-valor a boxplots (stat_compare_means)
  "corrplot",         # Mapa de calor de correlaciones (corrplot)
  "rstatix",          # cor_test() Spearman en formato tidy
  "car",              # vif() — Factor de Inflación de Varianza
  "ResourceSelection",# hoslem.test() — Hosmer-Lemeshow
  "pROC",             # Curvas ROC, AUC, IC DeLong, coords Youden
  "broom"             # tidy() — convierte modelos a data.frame
)


paquetes_faltantes <- paquetes_necesarios[
  !sapply(paquetes_necesarios, requireNamespace, quietly = TRUE)
]
if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ",
          paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes)
}

# --- Cargar todas las librerías ---
invisible(lapply(paquetes_necesarios, library, character.only = TRUE))

# --- Configurar idioma de gtsummary ---
theme_gtsummary_language("es", decimal.mark = ".", big.mark = ",")

# --- Crear carpetas de salida si no existen ---
source("config.R")
for (d in c(DIR_TABLAS, DIR_GRAFICAS, DIR_MODELOS, DIR_DATA)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

message("=== Setup completado. Todas las librerías cargadas. ===")
