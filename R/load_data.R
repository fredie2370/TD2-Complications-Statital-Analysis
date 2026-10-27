# =============================================================================
# R/load_data.R — Carga de datos y diccionarios para la app Shiny y el reporte
# =============================================================================
# Orden de búsqueda del RDS limpio:
#   1. data/processed/df_clean_tesis.rds  (datos reales, generado por 01_limpieza.R; NO está en GitHub)
#   2. data/demo/df_clean_demo.rds        (datos SINTÉTICOS incluidos en el repo)
# Se puede forzar una ruta con la variable de entorno TD2_APP_DATA.
# =============================================================================

find_project_root <- function(start = getwd()) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in 1:5) {
    if (file.exists(file.path(d, "config.R"))) return(d)
    d <- dirname(d)
  }
  stop("No se encontró config.R; ejecuta desde la raíz del proyecto o desde app/ o report/.")
}

load_td2_data <- function(root = find_project_root()) {
  forced <- Sys.getenv("TD2_APP_DATA", unset = "")
  real   <- file.path(root, "data", "processed", "df_clean_tesis.rds")
  demo   <- file.path(root, "data", "demo", "df_clean_demo.rds")
  path <- if (nzchar(forced)) forced else if (file.exists(real)) real else demo
  if (!file.exists(path)) stop("No hay datos: corre 01_limpieza.R o demo/make_demo_data.R")
  list(data = readRDS(path), path = path, synthetic = identical(basename(path), "df_clean_demo.rds"))
}

# --- Diccionarios (etiquetas en inglés, como en las figuras del manuscrito) ---
lab_dict <- c(
  Edad = "Age (years)", IMC = "BMI (kg/m²)", TAS = "Systolic BP (mmHg)", TAD = "Diastolic BP (mmHg)",
  Glucosa = "Glucose (mg/dL)", Hb = "Haemoglobin (g/dL)", Colesterol = "Cholesterol (mg/dL)",
  Trigliceridos = "Triglycerides (mg/dL)", BUN = "BUN (mg/dL)", Creatinina = "Creatinine (mg/dL)",
  Urea = "Urea (mg/dL)", Acido_Urico = "Uric acid (mg/dL)",
  TFG_CKD_EPI = "eGFR CKD-EPI (mL/min/1.73 m²)", TyG = "TyG index"
)

# Resultados / complicaciones (nombre de columna -> etiqueta)
outcome_dict <- c(
  ERC = "Chronic kidney disease", Dialisis = "Dialysis", Retinopatia = "Retinopathy",
  IC = "Heart failure", Amputacion = "Amputation", HTA = "Hypertension", EPOC = "COPD"
)

# Mismos conjuntos que 04_regresion.R y 05_roc.R
roc_outcomes    <- c("ERC", "Dialisis", "Retinopatia", "Amputacion")
roc_biomarkers  <- c("Acido_Urico", "TyG", "Glucosa", "Hb", "TFG_CKD_EPI", "Creatinina", "TAS", "IMC")
reg_outcomes    <- c("ERC", "HTA", "Dialisis", "Retinopatia", "IC", "Amputacion")
reg_predictors  <- c("Edad", "Sexo_bin", "TAS", "HTA_bin", "Glucosa", "Hb", "Acido_Urico", "TyG")
numeric_vars    <- names(lab_dict)
