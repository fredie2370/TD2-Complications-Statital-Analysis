# =============================================================================
# config.R — Configuración global del proyecto
# =============================================================================
# INSTRUCCIÓN: Este es el ÚNICO archivo que debes modificar para adaptar
# el análisis a tu entorno. Todos los demás scripts lo leen con source().
# =============================================================================

# --- Rutas ---
# Usar rutas relativas desde la raíz del proyecto (donde está config.R)
# (Cada ruta puede sobrescribirse con una variable de entorno; se usa para
#  correr el pipeline sobre los datos sintéticos de demo/ sin editar este archivo.)
RUTA_DATOS   <- Sys.getenv("TD2_DATA_FILE", unset = file.path("data", "raw", "Datos.xlsx"))
HOJA_DATOS   <- "DIABETICOS"  # Nombre exacto de la hoja en el Excel
SKIP_FILAS   <- 1             # Filas a omitir (encabezado agrupador)

# --- Constantes del estudio ---
N_TOTAL_HOSPITAL <- 531   # Pacientes únicos atendidos en el semestre (denominador)
N_DM2_ESPERADO   <- 228   # Pacientes con DM2 que cumplen criterios (para validación)
ALPHA            <- 0.05  # Nivel de significancia estadística

# --- Semilla de reproducibilidad ---
# Usada en fisher.test(simulate.p.value) y cualquier proceso aleatorio
SEMILLA <- 2024

# --- Carpetas de salida ---
DIR_TABLAS   <- Sys.getenv("TD2_DIR_TABLES",  unset = file.path("output", "tables"))
DIR_GRAFICAS <- Sys.getenv("TD2_DIR_FIGURES", unset = file.path("output", "figures"))
DIR_MODELOS  <- Sys.getenv("TD2_DIR_MODELS",  unset = file.path("output", "models"))
DIR_DATA     <- Sys.getenv("TD2_DIR_DATA",    unset = file.path("data", "processed"))
