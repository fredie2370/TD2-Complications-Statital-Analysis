# =============================================================================
# 02_descriptivo.R — Estadística descriptiva, frecuencias y tablas
# =============================================================================
# Protocolo: Sección VI.viii.a (descriptivo) + VI.viii.b (frecuencia hospitalaria)
# Genera: Tablas 1–3 en .docx, Gráficas 1–4 en .png, histogramas de normalidad
# =============================================================================

source("config.R")
source("00_setup.R")
df_clean <- readRDS(file.path(DIR_DATA, "df_clean_tesis.rds"))

# =============================================================================
# 1. DEFINICIÓN DE GRUPOS DE VARIABLES Y ETIQUETAS
# =============================================================================

vars_generales    <- c("Sexo", "Edad", "Peso", "Talla", "IMC", "IMC_cat")
vars_vitales      <- c("TAS", "TAD", "FC", "FR")
vars_complicacion <- c("HTA", "Retinopatia", "IC", "ERC",
                       "Dialisis", "EPOC", "Amputacion",
                       "Tiene_complicacion", "N_Complicaciones",
                       "Tiene_complicacion_o_comorbilidad","N_Complicaciones_y_Comorbilidades")
vars_laboratorio  <- c("Glucosa", "Hb", "Colesterol", "Trigliceridos",
                       "BUN", "Creatinina", "Urea", "Acido_Urico",
                       "TFG_CKD_EPI", "TyG")
vars_todas        <- c(vars_generales, vars_vitales,
                       vars_complicacion, vars_laboratorio)

etiquetas <- list(
  Sexo = "Sex assigned at birth", 
  Edad = "Age (years)", 
  Peso = "Weight (kg)",
  Talla = "Height (m)", 
  IMC = "BMI (kg/m\u00b2)", 
  IMC_cat = "BMI category",
  TAS = "Systolic BP (mmHg)", 
  TAD = "Diastolic BP (mmHg)",
  FC = "Heart rate (bpm)", 
  FR = "Respiratory rate (rpm)",
  # HTA = "Arterial hypertension", 
  # Retinopatia = "Diabetic retinopathy",
  # IC = "Heart failure", ERC = "Chronic kidney disease",
  # Dialisis = "Dialysis", EPOC = "COPD", Amputacion = "Prior amputation",
  # Tiene_complicacion = "At least 1 complication",
  # N_Complicaciones = "Number of complications",
  # Tiene_complicacion_o_comorbilidad = "At least 1 complication or comorbidity",
  # N_Complicaciones_y_Comorbilidades = "Number of complications or comorbidities",
  Glucosa = "Fasting glucose (mg/dL)", Hb = "Hemoglobin (g/dL)",
  Colesterol = "Total cholesterol (mg/dL)", 
  Trigliceridos = "Triglycerides (mg/dL)",
  BUN = "BUN (mg/dL)", Creatinina = "Creatinine (mg/dL)",
  Urea = "Urea (mg/dL)", Acido_Urico = "Uric acid (mg/dL)",
  TFG_CKD_EPI = "eGFR (mL/min/1.73m\u00b2)", TyG = "TyG index"
)

# =============================================================================
# 2. NORMALIDAD (Shapiro-Wilk) + HISTOGRAMAS VISUALES
# =============================================================================
# Protocolo: "evaluadas para normalidad mediante Shapiro-Wilk y la
#              INSPECCIÓN VISUAL DE HISTOGRAMAS"
# =============================================================================
cat("=== PRUEBA DE NORMALIDAD (Shapiro-Wilk) ===\n")

vars_numericas <- c(vars_generales, vars_vitales, vars_laboratorio)
vars_numericas <- vars_numericas[sapply(vars_numericas,
                                         function(v) is.numeric(df_clean[[v]]))]

resultados_normalidad <- list()

for (v in vars_numericas) {
  datos <- na.omit(df_clean[[v]])
  if (length(datos) < 3) next
  p <- shapiro.test(datos)$p.value
  es_normal <- p > ALPHA
  resultados_normalidad[[v]] <- list(p = p, normal = es_normal)
  cat(sprintf("  %-18s  p = %.4f  →  %s\n", v, p,
              ifelse(es_normal, "NORMAL     (Media \u00b1 DE)",
                     "NO NORMAL  (Mediana [RIC])")))
}

# Reglas estadísticas para gtsummary
mis_reglas <- setNames(
  lapply(vars_numericas, function(v) {
    if (!is.null(resultados_normalidad[[v]]) &&
        resultados_normalidad[[v]]$normal) {
      "{mean} \u00b1 {sd}"
    } else {
      "{median} ({p25} - {p75})"
    }
  }),
  vars_numericas
)

# --- Histogramas para inspección visual (REQUERIDO POR PROTOCOLO) ---
cat("\nGenerando histogramas de normalidad...\n")
dir_hist <- file.path(DIR_GRAFICAS, "normalidad")
dir.create(dir_hist, showWarnings = FALSE, recursive = TRUE)

for (v in vars_numericas) {
  datos <- na.omit(df_clean[[v]])
  if (length(datos) < 3) next
  etiq_v <- ifelse(!is.null(etiquetas[[v]]), etiquetas[[v]], v)
  p_sw   <- round(resultados_normalidad[[v]]$p, 4)

  p_hist <- ggplot(data.frame(x = datos), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20,
                   fill = "#4E79A7", color = "white", alpha = 0.85) +
    geom_density(color = "#E15759", linewidth = 1) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(datos), sd = sd(datos)),
                  color = "black", linetype = "dashed", linewidth = 0.8) +
    labs(
      title    = paste("Histograma:", etiq_v),
      subtitle = sprintf("Shapiro-Wilk p = %.4f — %s",
                         p_sw,
                         ifelse(p_sw > ALPHA, "Distribución NORMAL",
                                "Distribución NO NORMAL")),
      x = etiq_v, y = "Densidad",
      caption = "Línea discontinua: curva normal teórica. Línea roja: densidad empírica."
    ) +
    theme_classic(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

  ggsave(file.path(dir_hist, paste0("Hist_", v, ".png")),
         plot = p_hist, width = 6, height = 4, dpi = 300)
}
cat("  ✅ Histogramas guardados en:", dir_hist, "\n")

# =============================================================================
# 3. FUNCIÓN: GUARDAR TABLA EN WORD
# =============================================================================

guardar_tabla <- function(tbl_gt, nombre_archivo, pie_tabla) {
  ft <- tbl_gt %>%
    as_flex_table() %>%
    add_footer_lines(values = pie_tabla) %>%
    fontsize(size = 9, part = "footer") %>%
    italic(part = "footer") %>%
    fontsize(size = 10, part = "body") %>%
    bold(part = "header") %>%
    set_table_properties(layout = "autofit") %>%
    autofit()
  ruta <- file.path(DIR_TABLAS, paste0(nombre_archivo, ".docx"))
  save_as_docx(ft, path = ruta)
  cat("  ✅ Tabla guardada:", ruta, "\n")
}

# =============================================================================
# 4. TABLA 1 — CARACTERÍSTICAS GENERALES
# =============================================================================

tbl_total <- df_clean %>%
  select(all_of(vars_todas)) %>%
  tbl_summary(
    statistic    = mis_reglas,
    type         = list(all_categorical() ~ "categorical",
                        N_Complicaciones  ~ "continuous"),
    digits       = all_continuous() ~ 2,
    label        = etiquetas,
    missing      = "ifany",
    missing_text = "Without registration"
  ) %>%
  bold_labels() %>% italicize_levels() %>% add_n() %>%
  modify_caption("**Tabla 1.** Características generales, clínicas y de laboratorio de pacientes con DM2")
# view(tbl_total)  # solo interactivo: usar View() en RStudio (view() no existe en base R)
guardar_tabla(tbl_total, "Tabla_1_Caracteristicas_Generales",
  as_paragraph(
    as_b("DM2"), ": Diabetes mellitus tipo 2. ",
    as_b("IMC"), ": \u00cdndice de masa corporal (recalculado). ",
    as_b("TFG"), ": Tasa de filtrado glomerular (CKD-EPI 2009, sin ajuste \u00e9tnico). ",
    as_b("TyG"), ": \u00cdndice triglic\u00e9ridos-glucosa = ln(Tg \u00d7 Glu / 2). ",
    as_b("HTA"), ": Hipertensi\u00f3n arterial. ",
    as_b("ERC"), ": Enfermedad renal cr\u00f3nica. ",
    as_b("IC"), ": Insuficiencia card\u00edaca. ",
    as_b("EPOC"), ": Enfermedad pulmonar obstructiva cr\u00f3nica. ",
    "Variables continuas: media \u00b1 DE o mediana (RIC) seg\u00fan Shapiro-Wilk. ",
    "Variables categ\u00f3ricas: n (%)."
  ))

# =============================================================================
# 5. TABLA 2 — COMPARACIÓN POR SEXO
# =============================================================================

# Determinar qué variables continuas son normales para usar t de Student
vars_normales    <- names(Filter(function(x) x$normal, resultados_normalidad))
vars_no_normales <- names(Filter(function(x) !x$normal, resultados_normalidad))

# Construir lista de tests para add_p()
# lista_tests_sexo <- c(
#   setNames(rep(list("t.test"),       length(vars_normales)),    vars_normales),
#   setNames(rep(list("wilcox.test"),  length(vars_no_normales)), vars_no_normales),
#   list(all_categorical() = "chisq.test")
# )

lista_tests_sexo <- c(
  setNames(
    rep(list("t.test"), length(vars_normales)),
    vars_normales
  ),
  setNames(
    rep(list("wilcox.test"), length(vars_no_normales)),
    vars_no_normales
  )
)

tbl_sexo_base <- df_clean %>%
  select(all_of(c("Sexo", vars_generales[-1], vars_vitales,
                  vars_laboratorio))) %>%
  tbl_summary(
    by           = "Sexo",
    statistic    = mis_reglas,
    type         = list(all_categorical() ~ "categorical"),
    digits       = all_continuous() ~ 2,
    label        = etiquetas,
    missing      = "ifany",
    missing_text = "Missing data (n)"
  ) %>%
  add_p(
    test = c(
      lista_tests_sexo,
      list(
        all_categorical() ~ "chisq.test"
      )
    ),
    # test = list(
    #   all_continuous()  ~ "wilcox.test",  
    #   all_categorical() ~ "chisq.test"
    # ),
    pvalue_fun = function(x) {
      sapply(x, function(val) {
        if (is.na(val)) {
          return(NA_character_)
        } else if (val < 0.0001) {
          return("<0.0001")
        } else {
          res <- formatC(val, digits = 2, format = "g", drop0trailing = FALSE)
          val_num <- as.numeric(res)
          if (val_num >= 0.0001 && val_num < 1) {
            return(formatC(val_num, format = "f", digits = max(2, -floor(log10(val_num)))))
          } else if (val_num >= 1) {
            return(formatC(val_num, format = "f", digits = 2))
          } else {
            return(res)
          }
        }
      })
    }
  ) %>%
  add_overall() %>% bold_labels() %>% italicize_levels() %>%
  bold_p(t = ALPHA)

add_section_header <- function(tbl, section_name, after_var = NULL) {
  tbl_body <- tbl$table_body
  
  # Crear fila de encabezado
  header_row <- tibble::tibble(
    variable = paste0("section_", section_name),
    row_type = "label",
    label = section_name
  )
  
  # Rellenar columnas con NA (logical)
  for (col in setdiff(names(tbl_body), c("variable", "row_type", "label"))) {
    header_row[[col]] <- NA 
  }
  
  if (is.null(after_var)) {
    tbl_body <- dplyr::bind_rows(header_row, tbl_body)
  } else {
    matches <- which(tbl_body$variable == after_var)
    
    if (length(matches) > 0) {
      idx <- max(matches) 
      
      tbl_body <- dplyr::bind_rows(
        tbl_body[1:idx, , drop = FALSE],
        header_row,
        tbl_body[(idx + 1):nrow(tbl_body), , drop = FALSE]
      )
    } else {
      tbl_body <- dplyr::bind_rows(tbl_body, header_row)
    }
  }
  
  tbl$table_body <- tbl_body
  return(tbl)
}

tbl_sexo <- tbl_sexo_base %>%
  add_section_header("Demographics and anthropometry", after_var = NULL) %>%
  add_section_header("Vital signs", after_var = "IMC_cat") %>%
  add_section_header("Laboratory parameters", after_var = "FR") %>%
  modify_caption("Table 1. Baseline clinical and laboratory characteristics of 228 patients with type 2 diabetes, stratified by sex assigned at birth.") %>%
  modify_footnote(
    update = all_stat_cols() ~ "Continuous variables are presented as median (Q1–Q3) or mean (SD); categorical variables as n (%)."
  ) %>%
  modify_footnote(
    update = p.value ~ "Compared using Wilcoxon rank-sum, t-test, Pearson's chi-squared, or Fisher's exact test as appropriate."
  )

guardar_tabla(tbl_sexo, "Tabla_2_Comparacion_Sexo",
  as_paragraph(
      "Abbreviations: BMI, body mass index; BP, blood pressure; BUN, blood urea nitrogen; COPD, chronic obstructive pulmonary disease; eGFR, estimated glomerular filtration rate; TyG, triglyceride-glucose.",
      "\nStatistical significance: p < 0.05. Boldface indicates statistically significant differences between groups."
  ))

# =============================================================================
# 6. FRECUENCIAS CON IC 95% (Clopper-Pearson)
# =============================================================================

n_dm2 <- nrow(df_clean)

# 6.1 Frecuencia hospitalaria de DM2
ic_dm2 <- binom.test(n_dm2, N_TOTAL_HOSPITAL, conf.level = 1 - ALPHA)$conf.int
cat(sprintf("\n=== FRECUENCIA HOSPITALARIA DE DM2 ===\n  %d / %d = %.1f%% (IC95%%: %.1f%%-%.1f%%)\n",
            n_dm2, N_TOTAL_HOSPITAL,
            n_dm2 / N_TOTAL_HOSPITAL * 100,
            ic_dm2[1] * 100, ic_dm2[2] * 100))

# 6.2 Frecuencia de cada complicación
calcular_prevalencia <- function(var, etiqueta) {
  x     <- df_clean[[var]]
  n_si  <- sum(x == "Yes", na.rm = TRUE)
  n_obs <- sum(!is.na(x))
  ic    <- binom.test(n_si, n_obs, conf.level = 1 - ALPHA)$conf.int
  data.frame(
    Complicacion   = etiqueta,
    n              = n_si,
    N_observado    = n_obs,
    Prevalencia_pct= round(n_si / n_obs * 100, 1),
    IC95_inf       = round(ic[1] * 100, 1),
    IC95_sup       = round(ic[2] * 100, 1)
  )
}

complicaciones_lista <- list(
# #  c("HTA",         "Hipertensi\u00f3n Arterial"),
#   c("Retinopatia", "Retinopat\u00eda Diab\u00e9tica"),
#   c("IC",          "Insuficiencia Card\u00edaca"),
#   c("ERC",         "Enfermedad Renal Cr\u00f3nica"),
#   c("Dialisis",    "Di\u00e1lisis"),
# #  c("EPOC",        "EPOC"),
#   c("Amputacion",  "Amputaci\u00f3n Previa"),
#   c("Tiene_complicacion", "Al menos 1 complicaci\u00f3n"),
  c("HTA" , "Arterial hypertension"), 
  c("Retinopatia" , "Diabetic retinopathy"),
  c("IC","Heart failure"), 
  c("ERC","Chronic kidney disease") ,
  c("Dialisis","Dialysis") , 
  c("EPOC" , "COPD") , 
  c("Amputacion" , "Prior amputation")#,
  # c(Tiene_complicacion = "At least 1 complication"),
  # c(N_Complicaciones = "Number of complications"),
  # c(Tiene_Comorbilidad = "At least 1 comorbidity"),
  # c(N_Comorbilidad = "Number of comorbidity"),
  # c(Tiene_complicacion_o_comorbilidad = "At least 1 complication or comorbidity"),
  # c(N_Complicaciones_y_Comorbilidades = "Number of complications or comorbidities")
)

tabla_prevalencias <- bind_rows(
  lapply(complicaciones_lista, function(x) calcular_prevalencia(x[1], x[2]))
) %>%
  mutate(IC_95 = paste0(IC95_inf, " \u2013 ", IC95_sup, "%")) %>%
  select(Complicacion, n, Prevalencia_pct, IC_95)


cat("\n=== PREVALENCIA DE COMPLICACIONES ===\n")
print(tabla_prevalencias)



# Guardar como Word
ft_prev <- tabla_prevalencias %>%
  rename(`Complicación` = Complicacion,
         `n` = n, `Prevalencia (%)` = Prevalencia_pct, `IC 95%` = IC_95) %>%
  flextable() %>%
  set_caption("Tabla 3. Prevalencia de complicaciones cr\u00f3nicas en pacientes con DM2") %>%
  add_footer_lines(
    "IC 95% calculado con m\u00e9todo exacto de Clopper-Pearson. n: n\u00famero de pacientes con la complicaci\u00f3n."
  ) %>%
  fontsize(size = 9, part = "footer") %>% italic(part = "footer") %>%
  bold(part = "header") %>% autofit()

save_as_docx(ft_prev,
             path = file.path(DIR_TABLAS, "Tabla_3_Prevalencias_Complicaciones.docx"))
cat("  ✅ Tabla 3 guardada.\n")




########

# Tabla 2: Prevalencia de complicaciones crónicas estratificada por sexo
# Incluye desglose del número de complicaciones/comorbilidades

# Variables binarias de complicaciones
vars_complicaciones_binarias <- c(
  "HTA", "Retinopatia", "IC", "ERC", "Dialisis", "EPOC", "Amputacion",
  "Tiene_complicacion", "Tiene_comorbilidad", "Tiene_complicacion_o_comorbilidad"
)

# Variables de conteo (categóricas ordinales)
vars_conteo <- c(
  "N_Complicaciones",
  "N_Comorbilidad", 
  "N_Complicaciones_y_Comorbilidades"
)

tbl_complicaciones <- df_clean %>%
  select(all_of(c("Sexo", vars_complicaciones_binarias, vars_conteo))) %>%
  tbl_summary(
    by           = "Sexo",
    statistic    = all_categorical() ~ "{n} ({p}%)",
    digits       = all_categorical() ~ 1,
    label        = list(
      HTA = "Arterial hypertension",
      Retinopatia = "Diabetic retinopathy",
      IC = "Heart failure",
      ERC = "Chronic kidney disease",
      Dialisis = "Dialysis",
      EPOC = "COPD",
      Amputacion = "Prior amputation",
      Tiene_complicacion = "At least 1 complication",
      N_Complicaciones = "Number of complications",
      Tiene_comorbilidad = "At least 1 comorbidity",
      N_Comorbilidad = "Number of comorbidities",
      Tiene_complicacion_o_comorbilidad = "At least 1 complication or comorbidity",
      N_Complicaciones_y_Comorbilidades = "Number of complications or comorbidities"
    ),
    missing      = "ifany",
    missing_text = "Missing data (n)",
    type         = list(
      all_of(vars_complicaciones_binarias) ~ "dichotomous",
      all_of(vars_conteo) ~ "categorical"
    ),
    include      = all_of(c(vars_complicaciones_binarias, vars_conteo))
  ) %>%
  add_overall() %>%
  add_ci(
    method = list(all_of(vars_complicaciones_binarias) ~ "exact"),
    statistic = list(all_of(vars_complicaciones_binarias) ~ "{conf.low}–{conf.high}")
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_caption("Table 2. Prevalence of chronic complications in patients with type 2 diabetes, stratified by sex assigned at birth.") %>%
  modify_footnote(
    update = all_stat_cols() ~ "Data are n (%). Percentages are based on the number of patients with available data for each variable."
  ) %>%
  modify_footnote(
    update = starts_with("ci_") ~ "95% CI calculated using the Clopper-Pearson exact method."
  ) %>%
  modify_fmt_fun(
    update = list(
      all_stat_cols() ~ function(x) gsub("\\.", "·", x),
      starts_with("ci_") ~ function(x) gsub("\\.", "·", x)
    )
  )

# Guardar la tabla
guardar_tabla(tbl_complicaciones, "Tabla_2_Prevalencia_Complicaciones",
              as_paragraph(
                "Abbreviations: COPD, chronic obstructive pulmonary disease.",
                "\nCI = confidence interval. Percentages might not sum to 100% due to rounding."
              ))

cat("  ✅ Tabla 2 (Prevalencia de complicaciones con desglose) guardada.\n")

#######33


# =============================================================================
# 7. GRÁFICAS
# =============================================================================

# --- Gráfica 1: Prevalencia de complicaciones con IC 95% ---
datos_graf <- bind_rows(
  lapply(complicaciones_lista[1:7], 
         function(x) calcular_prevalencia(x[1], x[2]))
) %>%
  arrange(Prevalencia_pct) %>%
  mutate(Complicacion = factor(Complicacion, levels = Complicacion))

Figure <- ggplot(datos_graf, aes(x = Complicacion, y = Prevalencia_pct)) +
  geom_col(fill = "#4E79A7", width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = IC95_inf, ymax = IC95_sup),
                width = 0.25, color = "gray30", linewidth = 0.6) +
  geom_text(aes(label = paste0(Prevalencia_pct, "%")),
            hjust = 0.5,vjust = 3.5, size = 3.5, color = "grey30") +
  coord_flip(ylim = c(0, max(datos_graf$IC95_sup) * 1.18)) +
  labs(
   # title    = "Prevalencia de complicaciones cr\u00f3nicas y comorbilidades en pacientes con diabetes tipo 2",
  #  subtitle = paste0("Hospital General de Valle de Bravo, 2024 (n = ", n_dm2, ")"),
    x = NULL, y = "Prevalence (%)",
    caption  = "Rango de IC 95% (m\u00e9todo Clopper-Pearson)"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "gray40", size = 10),
        legend.position = "top")


ggsave(file.path(DIR_GRAFICAS, "Grafica1_Prevalencia_Complicaciones.png"),
       Figure, width = 9, height = 5, dpi = 300)

# --- Gráfica 2: Pirámide poblacional ---
df_piramide <- df_clean %>%
  filter(!is.na(Edad), !is.na(Sexo)) %>%
  mutate(Grupo_edad = cut(Edad,
    breaks = c(0, 30, 40, 50, 60, 70, 80, 120),
    labels = c("<30","30-39","40-49","50-59","60-69","70-79","\u226580"),
    right  = FALSE)) %>%
  count(Sexo, Grupo_edad) %>%
  mutate(n_plot = ifelse(Sexo == "Assigned male at birth", -n, n))

Figure_1B <- ggplot(df_piramide, aes(x = Grupo_edad, y = n_plot, fill = Sexo)) +
  geom_col(width = 0.75, alpha = 0.85) +
  geom_text(aes(label = abs(n_plot),
                hjust = ifelse(Sexo == "Assigned male at birth", 1.3, -0.3)),
            size = 3.2, color = "gray30") +
  scale_y_continuous(labels = function(x) abs(x),
    limits = c(-max(abs(df_piramide$n_plot)) * 1.2,
               max(abs(df_piramide$n_plot)) * 1.2)) +
  scale_fill_manual(values = c("Assigned female at birth" = "#FFC5D3", "Assigned male at birth" = "#92c5fc")) +
  coord_flip() +
  labs(#title = "Distribuci\u00f3n de pacientes con DM2 por edad y sexo",
       #subtitle = paste0("Hospital General de Valle de Bravo, 2024 (n = ", n_dm2, ")"),
       x = "Age group (years)", y = "Number of patients", fill = "") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top",
        panel.grid.major.x = element_line(color = "gray90", linetype = "dashed"))
Figure_1B

ggsave(file.path(DIR_GRAFICAS, "Grafica2_Piramide_Poblacional.png"),
       Figure_1B, width = 8, height = 5, dpi = 300)

# --- Gráfica 3: Distribución IMC ---
g3 <- df_clean %>% filter(!is.na(IMC_cat)) %>%
  count(IMC_cat) %>%
  mutate(pct = round(n / sum(n) * 100, 1),
         etiqueta = paste0(n, "\n(", pct, "%)")) %>%
  ggplot(aes(x = IMC_cat, y = n, fill = IMC_cat)) +
  geom_col(width = 0.6, alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = etiqueta), vjust = -0.4, size = 3.5) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Clasificaci\u00f3n del IMC en pacientes con DM2 (OMS)",
       subtitle = paste0("Hospital General de Valle de Bravo, 2024 (n = ", n_dm2, ")"),
       x = "Categor\u00eda IMC", y = "N\u00famero de pacientes") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(DIR_GRAFICAS, "Grafica3_Clasificacion_IMC.png"),
       g3, width = 7, height = 5, dpi = 300)

# --- Gráfica 4: Estadios ERC KDIGO ---
g4 <- df_clean %>% filter(!is.na(Estadio_ERC)) %>%
  count(Estadio_ERC) %>%
  mutate(pct = round(n / sum(n) * 100, 1),
         etiqueta = paste0(n, "\n(", pct, "%)")) %>%
  ggplot(aes(x = Estadio_ERC, y = n, fill = Estadio_ERC)) +
  geom_col(width = 0.65, alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = etiqueta), vjust = -0.4, size = 3.5) +
  scale_fill_brewer(palette = "YlOrRd") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Distribuci\u00f3n de estadios ERC seg\u00fan TFG (CKD-EPI 2009)",
       subtitle = paste0("Hospital General de Valle de Bravo, 2024 (n v\u00e1lido = ",
                         sum(!is.na(df_clean$Estadio_ERC)), ")"),
       x = "Estadio KDIGO", y = "N\u00famero de pacientes",
       caption = "TFG: Tasa de filtrado glomerular. ERC: Enfermedad renal cr\u00f3nica.") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(DIR_GRAFICAS, "Grafica4_Estadios_ERC.png"),
       g4, width = 8, height = 5, dpi = 300)

cat("\n=== BLOQUE 2 COMPLETADO ===\n")
cat("Tablas: Tabla_1, Tabla_2, Tabla_3\n")
cat("Gr\u00e1ficas: Grafica1-4, histogramas en output/figures/normalidad/\n")







# Instalar ggplot2 si no lo tienes: install.packages("ggplot2")
library(ggplot2)

# Crear el dataframe con tus resultados
datos_dm2 <- data.frame(
  Categoria = "Pacientes con diabetes tipo 2",
  Porcentaje = round(n_dm2 / N_TOTAL_HOSPITAL * 100, 1),
  IC_Inf = round(ic_dm2[1] * 100, 1),
  IC_Sup = round(ic_dm2[2] * 100, 1)
)

# Generar la gráfica
ggplot(datos_dm2, aes(x = Categoria, y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "#4A90E2", width = 0.4) +
  geom_errorbar(aes(ymin = IC_Inf, ymax = IC_Sup), 
                width = 0.1, color = "black", linewidth = 0.8) +
  geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -3, fontface = "bold") +
  ylim(0, 60) + # Límite del eje Y ligeramente por encima del IC superior
  labs(title = "Frecuencia de diabetes tipo 2 ",
       subtitle = paste0("N = ", N_TOTAL_HOSPITAL),
       y = "Porcentaje de Pacientes (%)",
       x = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))




datos_pastel <- data.frame(
  Grupo = c("With type 2 diabetes", "Without type 2 diabetes"),
  Pacientes = c(n_dm2, N_TOTAL_HOSPITAL - n_dm2)
)

# Calcular porcentajes para las etiquetas
datos_pastel$Porcentaje <- round(datos_pastel$Pacientes / sum(datos_pastel$Pacientes) * 100, 1)

# Generar la gráfica
Figure_1A <-  ggplot(datos_pastel, aes(x = "", y = Pacientes, fill = Grupo)) +
    geom_bar(stat = "identity", width = 1, color = "white",linewidth = 0.8) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(Pacientes, "\n(", Porcentaje, "%)")), 
              position = position_stack(vjust = 0.5), color = "white", 
              fontface = "bold", size = 4.5) +
    scale_fill_manual(values = c("#D9777F", "#2B5C8F")) +
     labs(#title = "frequency of type 2 diabetes ",
    #      #subtitle = "(N = 531)",
          fill = "") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 15),
          legend.title = element_text(size = 12, face = "bold",
                                      family = "sans"),
          legend.text = element_text(size = 11, family = "sans"),
          legend.position = "top")
        


Figure_1A





# =============================================================================
# 6.3 Complicaciones vs. complicaciones + comorbilidades (para presentación)
# -----------------------------------------------------------------------------
# Definiciones (01_limpieza.R):
#   - Tiene_complicacion                 = ERC, Dialisis, Retinopatia, IC, Amputacion
#   - Tiene_complicacion_o_comorbilidad  = lo anterior + HTA + EPOC
# Este bloque NO modifica df_clean; solo calcula prevalencias con IC 95%.
# =============================================================================

cols_solo_complicaciones <- c("ERC", "Dialisis", "Retinopatia", "IC", "Amputacion")

n_solo_complicaciones <- rowSums(
  sapply(df_clean[cols_solo_complicaciones], function(x) as.integer(x == "Yes")),
  na.rm = TRUE
)
solo_complicaciones <- factor(
  ifelse(n_solo_complicaciones > 0, "Yes", "No"),
  levels = c("No", "Yes")
)

# --- Estadísticos de ambas definiciones ---
n_si_todas   <- sum(df_clean$Tiene_complicacion_o_comorbilidad == "Yes", na.rm = TRUE)
n_obs_todas  <- sum(!is.na(df_clean$Tiene_complicacion_o_comorbilidad))
ic_todas     <- binom.test(n_si_todas, n_obs_todas, conf.level = 1 - ALPHA)$conf.int

n_si_solo    <- sum(solo_complicaciones == "Yes", na.rm = TRUE)
n_obs_solo   <- sum(!is.na(solo_complicaciones))
ic_solo      <- binom.test(n_si_solo, n_obs_solo, conf.level = 1 - ALPHA)$conf.int

comparacion_definiciones <- data.frame(
  Definición      = c("Complicaciones y comorbilidades (incluye HTA y EPOC)",
                      "Solo complicaciones (excluye HTA y EPOC)"),
  n               = c(n_si_todas, n_si_solo),
  N_observado     = c(n_obs_todas, n_obs_solo),
  Prevalencia_pct = round(c(n_si_todas / n_obs_todas, n_si_solo / n_obs_solo) * 100, 1),
  IC95_inf        = round(c(ic_todas[1], ic_solo[1]) * 100, 1),
  IC95_sup        = round(c(ic_todas[2], ic_solo[2]) * 100, 1)
)

cat("\n=== COMPLICACIONES VS. COMPLICACIONES+COMORBILIDADES (solo para presentación) ===\n")
print(comparacion_definiciones)
cat(sprintf(
  "\n  Complicaciones y comorbilidades: %.1f%% (IC95%%: %.1f%%-%.1f%%), n=%d/%d\n",
  comparacion_definiciones$Prevalencia_pct[1],
  comparacion_definiciones$IC95_inf[1], comparacion_definiciones$IC95_sup[1],
  n_si_todas, n_obs_todas
))
cat(sprintf(
  "  Solo complicaciones:             %.1f%% (IC95%%: %.1f%%-%.1f%%), n=%d/%d\n",
  comparacion_definiciones$Prevalencia_pct[2],
  comparacion_definiciones$IC95_inf[2], comparacion_definiciones$IC95_sup[2],
  n_si_solo, n_obs_solo
))

# --- Verificación rápida: ERC vs. Diálisis (la diálisis debería ocurrir solo con ERC) ---
# View(df_clean)  # solo interactivo

print(table(ERC = df_clean$ERC, Dialisis = df_clean$Dialisis, useNA = "ifany"))
cat("Pacientes con ERC y diálisis:",
    sum(df_clean$ERC == "Yes" & df_clean$Dialisis == "Yes", na.rm = TRUE), "\n")

# --------------------------------------------------------------------------
# Clasificación de IMC — Global, Femenino, Masculino
# --------------------------------------------------------------------------

# 1) Tabla Global (como ya la tenías)
tabla_global <- df_clean %>%
  filter(!is.na(IMC_cat)) %>%
  count(IMC_cat) %>%
  mutate(pct = round(n / sum(n) * 100, 1), Grupo = "Global")

# 2) Tabla por sexo — el % se calcula DENTRO de cada sexo, no contra el total
tabla_sexo <- df_clean %>%
  filter(!is.na(IMC_cat), !is.na(Sexo)) %>%
  count(Sexo, IMC_cat) %>%
  group_by(Sexo) %>%                      
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  rename(Grupo = Sexo)

# 3) Unir ambas tablas y fijar el orden de categorías y de grupos
tabla_imc <- bind_rows(tabla_global, tabla_sexo) %>%
  mutate(
    IMC_cat = factor(IMC_cat, levels = c("Underweight (<18·5)", "Normal (18·5–24·9)",
    "Overweight (25·0–29·9)", "Obesity (≥30·0)")),
    Grupo   = factor(Grupo, levels = c("Global", "Assigned female at birth", "Assigned male at birth"))
  )

n_grupo <- tabla_imc %>%
  group_by(Grupo) %>%
  summarise(n_total = sum(n)) %>%
  { setNames(.$n_total, as.character(.$Grupo)) }

# 4) Gráfica de barras agrupadas
Figure_1C <- ggplot(tabla_imc, aes(x = IMC_cat, y = n, fill = Grupo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0(n, "\n(", pct, "%)")),
    position = position_dodge(width = 0.8),
    vjust = -0.3, size = 3.2, colour = "grey30"
  ) +
  scale_fill_manual(values = c(
    "Global"    = "#88E788",
    "Assigned female at birth"  = "#FFC5D3",
    "Assigned male at birth" = "#92c5fc"
  ),
  labels = function(x) paste0(x, " (n=", n_grupo[x], ")")) +
  labs(
    #title    = "Clasificación del IMC en pacientes con diabetes tipo 2",
    #subtitle = "Hospital General de Valle de Bravo, Enero-Junio 2024",
    x = "BMI category", y = "Number of patients", fill = NULL
  ) +
  expand_limits(y = max(tabla_imc$n) * 1.15) +
  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    panel.grid.major.y   = element_line(color = "gray90", linetype = "dashed"),
 #   axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "top"
  )

Figure_1C




# Función para calcular prevalencia global y por sexo para una complicación
calcular_prevalencia_completa <- function(var, etiqueta) {
  x_vals <- df_clean[[var]]
  
  # 1. Cálculo Global (Total)
  n_global <- sum(x_vals == "Yes", na.rm = TRUE)
  N_obs_global <- sum(!is.na(x_vals))
  ic_global <- if (N_obs_global > 0) {
    binom.test(n_global, N_obs_global, conf.level = 1 - ALPHA)$conf.int
  } else {
    c(0, 0)
  }
  
  df_global <- data.frame(
    Complicacion = etiqueta,
    Grupo = "Global",
    n = n_global,
    N_observado = N_obs_global,
    Prevalencia_pct = round(n_global / N_obs_global * 100, 1),
    IC95_inf = round(ic_global[1] * 100, 1),
    IC95_sup = round(ic_global[2] * 100, 1)
  )
  
  # 2. Cálculo por Sexo
  df_sexo <- df_clean %>%
    filter(!is.na(.data[[var]]), !is.na(Sexo)) %>%
    group_by(Sexo) %>%
    summarise(
      n = sum(.data[[var]] == "Yes", na.rm = TRUE),
      N_observado = n(),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      Complicacion = etiqueta,
      Grupo = as.character(Sexo), 
      Prevalencia_pct = round(n / N_observado * 100, 1),
      IC95_inf = ifelse(N_observado > 0, round(binom.test(n, N_observado, conf.level = 1 - ALPHA)$conf.int[1] * 100, 1), 0),
      IC95_sup = ifelse(N_observado > 0, round(binom.test(n, N_observado, conf.level = 1 - ALPHA)$conf.int[2] * 100, 1), 0)
    ) %>%
    ungroup() %>%
    select(Complicacion, Grupo, n, N_observado, Prevalencia_pct, IC95_inf, IC95_sup)
  
  # Combinar ambos resultados
  bind_rows(df_global, df_sexo)
}


# Lista de complicaciones
complicaciones_lista <- list(
  c("HTA", "Arterial hypertension"),
  c("Retinopatia", "Diabetic retinopathy"),
  c("IC", "Heart failure"),
  c("ERC", "Chronic kidney disease"),
  c("Dialisis", "Dialysis"),
  c("EPOC", "COPD"),
  c("Amputacion", "Prior amputation")
)

tabla_imc <- bind_rows(tabla_global, tabla_sexo) %>%
  mutate(
    IMC_cat = factor(IMC_cat, levels = c("Underweight (<18·5)", "Normal (18·5–24·9)",
                                         "Overweight (25·0–29·9)", "Obesity (≥30·0)")),
    Grupo   = factor(Grupo, levels = c("Global", "Assigned female at birth", "Assigned male at birth"))
  )


# Unir todo en un solo dataframe de trabajo
datos_graf_completo <- bind_rows(
  lapply(complicaciones_lista, function(x) calcular_prevalencia_completa(x[1], x[2]))
) %>%
  mutate(
    Complicacion = factor(Complicacion, levels = c("Arterial hypertension",
                                                   "Chronic kidney disease",
                                                   "Dialysis",
                                                   "COPD",
                                                   "Diabetic retinopathy",
                                                   "Prior amputation",
                                                   "Heart failure")),
    Grupo =  factor(Grupo, levels = c("Global", "Assigned female at birth", "Assigned male at birth")) 
  )

# Ordenar las complicaciones basado en la prevalencia Global de mayor a menor (o menor a mayor)
# orden_complicaciones <- datos_graf_completo %>%
#   filter(Grupo == "Global") %>%
#   arrange(n) %>%
#   pull(Complicacion)
# 
# datos_graf_completo <- datos_graf_completo %>%
#   mutate(Complicacion = factor(Complicacion, levels = orden_complicaciones))

n_global_total_2 <- sum(!is.na(df_clean$HTA)) # O puedes usar nrow(df_clean) según corresponda al total de pacientes evaluados
n_fem_total_2 <- sum(df_clean$Sexo == "Assigned female at birth" & !is.na(df_clean$Sexo), na.rm = TRUE)
n_mal_total_2 <- sum(df_clean$Sexo == "Assigned male at birth" & !is.na(df_clean$Sexo), na.rm = TRUE)

# Si en tu columna Sexo usas "Female" / "Male" cortos, ajusta los nombres de abajo acorde:
n_grupo_2 <- c(
  "Global" = nrow(df_clean), 
  "Assigned female at birth" = n_fem_total_2, 
  "Assigned male at birth" = n_mal_total_2
)

  
Figure_1D <- ggplot(datos_graf_completo, aes(x = Complicacion, y = n, fill = Grupo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  
  # Barras de error adaptadas a la posición agrupada (dodge)
  # geom_errorbar(
  #   aes(ymin = IC95_inf, ymax = IC95_sup),
  #   position = position_dodge(width = 0.8),
  #   width = 0.25, color = "gray30", linewidth = 0.6
  # ) +
  
  # Etiquetas de texto con el porcentaje, posicionadas de forma alineada al grupo
  geom_text(
    aes(label = paste0(n, "\n(", Prevalencia_pct, "%)")),
    position = position_dodge(width = 0.8),
    hjust = .5, vjust = -.5,size = 3.2, color = "gray30"
  ) +
  
  # Mismos colores exactos solicitados y estéticos de Figure_1C
  scale_fill_manual(
    values = c(
      "Global" = "#88E788",
      "Assigned female at birth" = "#FFC5D3",
      "Assigned male at birth" = "#92c5fc"
  ),
  labels = function(x) paste0(x, " (n=", n_grupo_2[x], ")"))+
# labels = c(
#   "Global" = "Global",
#   "Assigned female at birth" = "Assigned female at birth",
#   "Assigned male at birth" = "Assigned male at birth" )
# ) +
  
  # Coordenadas invertidas para gráfico de barras horizontal, ampliando el límite de Y para los textos
 # coord_flip(ylim = c(0, max(datos_graf_sexo$n, na.rm = TRUE) * 2)) +
  
  labs(
    x = "Complication or comorbidity", 
    y = "Number of patients", 
    fill = NULL,
    #caption = "95% CI range (Clopper-Pearson method)"
  ) +
  expand_limits(y = max(datos_graf_completo$n) * 1.15) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
   # axis.text.x = element_text(vjust = 1, hjust = 1),
    legend.position = "top"
  )

Figure_1D

ggsave(file.path(DIR_GRAFICAS, "IMC_por_sexo.png"), plot = Figure_1C,
       width = 9, height = 6, dpi = 300, bg = "white")


Figure_1A
Figure_1B
Figure_1C
Figure_1D


#########################################
#Hacer panel paper
#########################################

# install.packages("patchwork")
# 
# # Cargar la librería para unir gráficos
# library(patchwork)
# 
# Panel_Figura1 <- (Figure_1A | Figure_1B) / 
#   (Figure_1C | Figure_1D) +
#   plot_annotation(
#     tag_levels = 'A'
#   ) & 
#   theme(
#     plot.tag = element_text(face = "bold", size = 14, family = "sans") #
#   )
# 
# Panel_Figura1
# 
# ggsave(
#   filename = "graficas/Figura_1_Multipanel", 
#   plot = Panel_Figura1, 
#   width = 180,       
#   height = 150,
#   units = "mm",
#   dpi = 600,        
#   device =        
# )

############################### segunda opcion



# # Combinar los 4 gráficos en una cuadrícula 2x2
# Panel_Figura1_cowplot <- plot_grid(
#   Figure_1B, Figure_1C,
#   Figure_1D,
#   labels = c("A", "B", "C"), 
#   label_size = 14#,                
# #  ncol = 2,                    
# #  nrow = 2                      
# )
# 
# # Visualizar
# Panel_Figura1_cowplot
# ggsave(
#   filename = "graficas/Figure_1_Multipanel_cowplot.pdf", 
#   plot = Panel_Figura1_cowplot, 
#   width = 580, 
#   height = 420, 
#   units = "mm", 
#   device = cairo_pdf
# )



fila_superior <- plot_grid(
  Figure_1B, Figure_1C,
  labels = c("A", "B"),
  label_size = 14,
  ncol = 2,
  nrow = 1
)

# 2. Crear la segunda fila con un solo gráfico (C) que abarque todo el ancho inferior
fila_inferior <- plot_grid(
  Figure_1D,
  labels = c("C"),
  label_size = 14,
  ncol = 1,
  nrow = 1
)

# 3. Unir ambas filas en una sola columna vertical final (Fila 1 de dos columnas arriba, Fila 2 abajo)
Panel_Figura1_asimetrico <- plot_grid(
  fila_superior, 
  fila_inferior, 
  ncol = 1, 
  nrow = 2,
  rel_heights = c(1, 1) # Proporción de altura entre la primera y segunda fila
)

# Visualizar el panel resultante en pantalla
Panel_Figura1_asimetrico

# (svglite y cowplot se instalan y cargan en 00_setup.R)

ggsave(
  filename = file.path(DIR_GRAFICAS, "Figure_1C.svg"),  # panel C del paper = objeto Figure_1D
  plot = Figure_1D, 
  width = 320,         
  height = 150,        
  units = "mm",
  dpi = 600,
  device = svglite     
)

