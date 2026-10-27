# =============================================================================
# 01_limpieza.R — Carga, limpieza y control de calidad de datos
# =============================================================================
# Protocolo: Etapa 3 (captura) + Etapa 4 (depuración y validación)
# Genera: data/processed/df_clean_tesis.rds
# =============================================================================

source("config.R")   # Carga rutas, constantes y ALPHA
source("00_setup.R") # Carga librerías y crea carpetas

# =============================================================================
# 1. CARGA DE DATOS
# =============================================================================
cat("=== CARGANDO DATOS ===\n")
cat("Ruta:", RUTA_DATOS, "\n")

# Verificar que el archivo existe antes de intentar cargarlo
if (!file.exists(RUTA_DATOS)) {
  stop("Archivo no encontrado: ", RUTA_DATOS,
       "\nVerifica la ruta en config.R")
}

df_raw <- read_excel(RUTA_DATOS, sheet = HOJA_DATOS, skip = SKIP_FILAS)

cat("Filas cargadas:   ", nrow(df_raw), "\n")
cat("Columnas cargadas:", ncol(df_raw), "\n")

# =============================================================================
# 2. FUNCIÓN: TFG POR ECUACIÓN CKD-EPI 2009
# =============================================================================
# Fórmula original: Levey et al., Ann Intern Med 2009.
# Sin ajuste por raza/etnia (KDIGO 2012; apropiado para población mexicana).
#
# Mujeres: TFG = 144 × (Cr/0.7)^α × 0.993^Edad
#   α = -0.329 si Cr ≤ 0.7
#   α = -1.209 si Cr > 0.7
# Hombres: TFG = 141 × (Cr/0.9)^α × 0.993^Edad
#   α = -0.411 si Cr ≤ 0.9
#   α = -1.209 si Cr > 0.9

calcular_tfg_ckdepi <- function(creatinina, edad, sexo) {
  dplyr::case_when(
    sexo == "Assigned female at birth"  & creatinina <= 0.7 ~
      144 * (creatinina / 0.7)^(-0.329) * (0.993^edad),
    sexo == "Assigned female at birth"  & creatinina >  0.7 ~
      144 * (creatinina / 0.7)^(-1.209) * (0.993^edad),
    sexo == "Assigned male at birth" & creatinina <= 0.9 ~
      141 * (creatinina / 0.9)^(-0.411) * (0.993^edad),
    sexo == "Assigned male at birth" & creatinina >  0.9 ~
      141 * (creatinina / 0.9)^(-1.209) * (0.993^edad),
    TRUE ~ NA_real_
  ) %>% round(1)
}

# =============================================================================
# 3. LIMPIEZA Y TRANSFORMACIÓN
# =============================================================================

df_clean <- df_raw %>%

  # 3.1 Eliminar filas completamente vacías (sin nombre de paciente)
  filter(!is.na(Nombre) & Nombre != "") %>%

  # 3.2 Separar tensión arterial sistólica y diastólica (formato "156-72")
  mutate(
    TAS = as.numeric(stringr::str_split_fixed(TA, "-", 2)[, 1]),
    TAD = as.numeric(stringr::str_split_fixed(TA, "-", 2)[, 2])
  ) %>%

  # 3.3 Estandarizar variables categóricas (S/N → Sí/No; F/M → Femenino/Masculino)
  mutate(
    Sexo = case_when(
      str_to_upper(Sexo) == "F" ~ "Assigned female at birth",
      str_to_upper(Sexo) == "M" ~ "Assigned male at birth",
      TRUE ~ NA_character_
    ),
    across(
      c(`HTDS-HA`, `Retinopatía diabética`, `insuficencia cardiaca`,
        ERC, Dialisis, EPOC, `Amputación`),
      ~ case_when(
        str_to_upper(.) == "S" ~ "Yes",
        str_to_upper(.) == "N" ~ "No",
        TRUE ~ NA_character_
      )
    )
  ) %>%

  # 3.4 Crear variables con nombres limpios y tipos de datos correctos
  mutate(
    ID           = as.character(Nombre),

    # Demográficas
    Sexo         = factor(Sexo, levels = c("Assigned female at birth", "Assigned male at birth")),
    Edad         = as.numeric(Edad),

    # Antropometría — IMC recalculado (no usar el de Excel por ser fórmula)
    Peso         = as.numeric(`Peso (Kg)`),
    Talla        = as.numeric(`Talla (m)`),
    IMC          = round(Peso / (Talla^2), 2),

    # Clasificación IMC según OMS
    IMC_cat = factor(
      case_when(
        IMC < 18.5              ~ "Underweight (<18·5)",
        IMC >= 18.5 & IMC < 25 ~ "Normal (18·5–24·9)",
        IMC >= 25   & IMC < 30 ~ "Overweight (25·0–29·9)",
        IMC >= 30               ~ "Obesity (≥30·0)",
        TRUE                    ~ NA_character_
      ),
      levels = c("Underweight (<18·5)", "Normal (18·5–24·9)", 
                 "Overweight (25·0–29·9)", "Obesity (≥30·0)")
    ),

    # Signos vitales
    FC   = as.numeric(FC),
    FR   = as.numeric(FR),

    # Complicaciones — factores con niveles explícitos
    HTA         = factor(`HTDS-HA`,               levels = c("No", "Yes")),
    Retinopatia = factor(`Retinopatía diabética`,  levels = c("No", "Yes")),
    IC          = factor(`insuficencia cardiaca`,  levels = c("No", "Yes")),
    ERC         = factor(ERC,                      levels = c("No", "Yes")),
    Dialisis    = factor(Dialisis,                 levels = c("No", "Yes")),
    EPOC        = factor(EPOC,                     levels = c("No", "Yes")),
    Amputacion  = factor(`Amputación`,             levels = c("No", "Yes")),

    # Laboratorio
    Glucosa       = as.numeric(Glucosa),
    Hb            = as.numeric(`Anemia (HB)`),
    Colesterol    = as.numeric(colesi),
    Trigliceridos = as.numeric(Tg),
    BUN           = as.numeric(Bun),
    Creatinina    = as.numeric(Creatiniina),  # Typo original en Excel: doble 'i'
    Urea          = as.numeric(urea),
    Acido_Urico   = as.numeric(`acido urico`)
  ) %>%

  # 3.5 Variables derivadas
  mutate(
    # TFG por CKD-EPI 2009 (sin ajuste étnico — población mexicana)
    TFG_CKD_EPI = calcular_tfg_ckdepi(Creatinina, Edad, as.character(Sexo)),

    # Estadio ERC según clasificación KDIGO 2012
    Estadio_ERC = factor(
      case_when(
        TFG_CKD_EPI >= 90 ~ "G1 (\u226590)",
        TFG_CKD_EPI >= 60 ~ "G2 (60-89)",
        TFG_CKD_EPI >= 45 ~ "G3a (45-59)",
        TFG_CKD_EPI >= 30 ~ "G3b (30-44)",
        TFG_CKD_EPI >= 15 ~ "G4 (15-29)",
        TFG_CKD_EPI <  15 ~ "G5 (<15)",
        TRUE              ~ NA_character_
      ),
      levels = c("G1 (\u226590)", "G2 (60-89)", "G3a (45-59)",
                 "G3b (30-44)", "G4 (15-29)", "G5 (<15)")
    ),

    # Índice TyG = ln(Triglicéridos × Glucosa / 2) — marcador de RI
    TyG = round(log(Trigliceridos * Glucosa / 2), 3),
    
    
    #Indice de complicaciones
    N_Complicaciones = rowSums(
      across(c(Retinopatia, IC, ERC, Dialisis, Amputacion),
             ~as.integer(.== "Yes")),
      na.rm=TRUE
    ),
    Tiene_complicacion = factor(
      ifelse(N_Complicaciones > 0, "Yes","No"),
      levels = c("No", "Yes")
    ),
    
    #Indice de comorbilidad
    N_Comorbilidad = rowSums(
      across(c(HTA,EPOC),
             ~as.integer(.== "Yes")),
      na.rm=TRUE
    ),
    Tiene_comorbilidad = factor(
      if_else(N_Comorbilidad >0,"Yes","No"),
      levels = c("No","Yes")
    ),

    # Índice de carga de complicaciones y comorbilidades
    N_Complicaciones_y_Comorbilidades = rowSums(
      across(c(HTA, EPOC,Retinopatia, IC, ERC, Dialisis, Amputacion),
             ~ as.integer(. == "Yes")),
      na.rm = TRUE
    ),
    Tiene_complicacion_o_comorbilidad = factor(
      ifelse(N_Complicaciones_y_Comorbilidades > 0, "Yes", "No"),
      levels = c("No", "Yes")
    )
  ) %>%

  # 3.6 Seleccionar solo las variables del análisis (excluir ID para exportar)
  select(
    Sexo, Edad, Peso, Talla, IMC, IMC_cat,
    TAS, TAD, FC, FR,
    HTA, Retinopatia, IC, ERC, Dialisis, EPOC, Amputacion,
    Tiene_complicacion, N_Complicaciones,
    Tiene_comorbilidad, N_Comorbilidad,
    Tiene_complicacion_o_comorbilidad, N_Complicaciones_y_Comorbilidades,
    Glucosa, Hb, Colesterol, Trigliceridos, BUN,
    Creatinina, Urea, Acido_Urico, TFG_CKD_EPI, Estadio_ERC, TyG
  )

cat("\nDimensiones finales:", nrow(df_clean), "filas ×", ncol(df_clean), "columnas\n")

# =============================================================================
# 4. CONTROL DE CALIDAD
# =============================================================================
cat("\n=== CONTROL DE CALIDAD ===\n")

# 4.1 Verificar n esperado
cat(sprintf("n pacientes: %d (esperado: %d) %s\n",
            nrow(df_clean), N_DM2_ESPERADO,
            ifelse(nrow(df_clean) == N_DM2_ESPERADO, "✅", "⚠️  REVISAR")))

# 4.2 Valores faltantes por variable
na_resumen <- df_clean %>%
  select(-Estadio_ERC) %>%  # Excluir factor derivado (NAs esperados si no hay Cr)
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NAs") %>%
  filter(NAs > 0) %>%
  arrange(desc(NAs))

cat("\n--- Valores faltantes ---\n")
if (nrow(na_resumen) == 0) cat("Sin valores faltantes.\n") else print(na_resumen)

# 4.3 Detección de valores extremos (regla IQR × 3 — protocolo Etapa 4)
cat("\n--- Valores extremos (IQR × 3) ---\n")
vars_outlier <- c("Edad", "IMC", "TAS", "TAD", "FC", "FR",
                  "Glucosa", "Hb", "Creatinina", "TFG_CKD_EPI",
                  "BUN", "Urea", "Acido_Urico", "Colesterol",
                  "Trigliceridos", "TyG")

for (v in vars_outlier) {
  x <- df_clean[[v]]
  x_valido <- x[!is.na(x)]
  q1 <- quantile(x_valido, 0.25); q3 <- quantile(x_valido, 0.75)
  iqr <- q3 - q1
  outliers <- which(x < (q1 - 3 * iqr) | x > (q3 + 3 * iqr))
  if (length(outliers) > 0) {
    cat(sprintf("  %-18s → %d caso(s) en filas: %s | valores: %s\n",
                v, length(outliers),
                paste(outliers, collapse = ", "),
                paste(round(x[outliers], 1), collapse = ", ")))
    cat("    → Verificar contra expediente físico antes de retener.\n")
  }
}
cat("  (Variables sin outliers no se muestran)\n")

# 4.4 Verificar TFG calculada
cat(sprintf("\nTFG CKD-EPI calculada: %d / %d pacientes\n",
            sum(!is.na(df_clean$TFG_CKD_EPI)), nrow(df_clean)))

# 4.5 Resumen rápido con skimr
cat("\n--- Resumen general ---\n")
skimr::skim(df_clean %>% select(-Estadio_ERC))

# =============================================================================
# 5. GUARDAR BASE LIMPIA
# =============================================================================
ruta_rds <- file.path(DIR_DATA, "df_clean_tesis.rds")
saveRDS(df_clean, file = ruta_rds)
cat("\n✅ Base limpia guardada en:", ruta_rds, "\n")
cat("=== BLOQUE 1 COMPLETADO ===\n")

