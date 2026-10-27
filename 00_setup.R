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
  "broom",            # tidy() — convierte modelos a data.frame
  "cowplot",          # plot_grid() — paneles multi-figura (02_descriptivo.R)
  "svglite"           # dispositivo SVG para figuras vectoriales (02, 04, 05)
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
theme_gtsummary_language("en", decimal.mark = "·", big.mark = ",")

# --- Crear carpetas de salida si no existen ---
# Los scripts deben ejecutarse desde la raíz del proyecto (carpeta con config.R).
if (!file.exists("config.R")) {
  stop("Ejecuta los scripts desde la raíz del proyecto (la carpeta que contiene config.R).",
       call. = FALSE)
}
source("config.R")
for (d in c(DIR_TABLAS, DIR_GRAFICAS, DIR_MODELOS, DIR_DATA)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Registrar versiones (R y paquetes) de cada ejecución ---
writeLines(capture.output(sessionInfo()), file.path(DIR_MODELOS, "sessionInfo.txt"))

message("=== Setup completado. Todas las librerías cargadas. ===")

