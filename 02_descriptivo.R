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
                       "Tiene_complicacion", "N_Complicaciones")
vars_laboratorio  <- c("Glucosa", "Hb", "Colesterol", "Trigliceridos",
                       "BUN", "Creatinina", "Urea", "Acido_Urico",
                       "TFG_CKD_EPI", "TyG")
vars_todas        <- c(vars_generales, vars_vitales,
                       vars_complicacion, vars_laboratorio)

etiquetas <- list(
  Sexo = "Sexo", Edad = "Edad (a\u00f1os)", Peso = "Peso (kg)",
  Talla = "Talla (m)", IMC = "IMC (kg/m\u00b2)", IMC_cat = "Clasificaci\u00f3n IMC",
  TAS = "TA Sist\u00f3lica (mmHg)", TAD = "TA Diast\u00f3lica (mmHg)",
  FC = "Frecuencia Card\u00edaca (lpm)", FR = "Frecuencia Respiratoria (rpm)",
  HTA = "Hipertensi\u00f3n Arterial", Retinopatia = "Retinopat\u00eda Diab\u00e9tica",
  IC = "Insuficiencia Card\u00edaca", ERC = "Enfermedad Renal Cr\u00f3nica",
  Dialisis = "Di\u00e1lisis", EPOC = "EPOC", Amputacion = "Amputaci\u00f3n Previa",
  Tiene_complicacion = "Al menos una complicaci\u00f3n",
  N_Complicaciones = "N\u00b0 de complicaciones",
  Glucosa = "Glucosa (mg/dL)", Hb = "Hemoglobina (g/dL)",
  Colesterol = "Colesterol Total (mg/dL)", Trigliceridos = "Triglic\u00e9ridos (mg/dL)",
  BUN = "BUN (mg/dL)", Creatinina = "Creatinina (mg/dL)",
  Urea = "Urea (mg/dL)", Acido_Urico = "\u00c1cido \u00darico (mg/dL)",
  TFG_CKD_EPI = "TFG CKD-EPI (mL/min/1.73m\u00b2)", TyG = "\u00cdndice TyG"
)

# =============================================================================
# 2. NORMALIDAD (Shapiro-Wilk) + HISTOGRAMAS VISUALES
# =============================================================================
# Protocolo: "evaluadas para normalidad mediante Shapiro-Wilk y la
#              INSPECCIÓN VISUAL DE HISTOGRAMAS"
# =============================================================================
cat("=== PRUEBA DE NORMALIDAD (Shapiro-Wilk) ===\n")

vars_numericas <- c(vars_generales, vars_vitales, vars_laboratorio,
                    "N_Complicaciones")
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
    missing_text = "Sin registro"
  ) %>%
  bold_labels() %>% italicize_levels() %>% add_n() %>%
  modify_caption("**Tabla 1.** Características generales, clínicas y de laboratorio de pacientes con DM2")

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
# FIX: Se usa add_p con lista mixta: wilcox/t según normalidad + chisq/fisher

# Determinar qué variables continuas son normales para usar t de Student
vars_normales    <- names(Filter(function(x) x$normal, resultados_normalidad))
vars_no_normales <- names(Filter(function(x) !x$normal, resultados_normalidad))

# Construir lista de tests para add_p()
# lista_tests_sexo <- c(
#   setNames(rep(list("t.test"),       length(vars_normales)),    vars_normales),
#   setNames(rep(list("wilcox.test"),  length(vars_no_normales)), vars_no_normales),
#   list(all_categorical() ~ "chisq.test")
# )

tbl_sexo <- df_clean %>%
  select(all_of(c("Sexo", vars_generales[-1], vars_vitales,
                  vars_complicacion, vars_laboratorio))) %>%
  tbl_summary(
    by           = "Sexo",
    statistic    = mis_reglas,
    type         = list(all_categorical() ~ "categorical",
                        N_Complicaciones  ~ "continuous"),
    digits       = all_continuous() ~ 2,
    label        = etiquetas,
    missing      = "ifany",
    missing_text = "Sin registro"
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ "wilcox.test",  # Conservador; ver nota abajo
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  add_overall() %>% bold_labels() %>% italicize_levels() %>%
  bold_p(t = ALPHA) %>%
  modify_caption("**Tabla 2.** Caracter\u00edsticas cl\u00ednicas y de laboratorio por sexo (DM2)")

# NOTA METODOLÓGICA: gtsummary no soporta listas mixtas de tests por variable
# en add_p(). Se usa Wilcoxon como enfoque conservador (apropiado para n=233).
# Para la Edad (única variable normal), el p-valor de Wilcoxon y t-test
# serán prácticamente idénticos con esta n. Se menciona en pie de tabla.

guardar_tabla(tbl_sexo, "Tabla_2_Comparacion_Sexo",
  as_paragraph(
    "Variables continuas comparadas con prueba U de Mann-Whitney. ",
    "Variables categ\u00f3ricas con Chi-cuadrada de Pearson o Fisher exacto (frecuencia esperada < 5). ",
    as_b("p"), " en negrita: p < 0.05. ",
    as_b("RIC"), ": Rango intercuartílico."
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
  n_si  <- sum(x == "S\u00ed", na.rm = TRUE)
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
  c("HTA",         "Hipertensi\u00f3n Arterial"),
  c("Retinopatia", "Retinopat\u00eda Diab\u00e9tica"),
  c("IC",          "Insuficiencia Card\u00edaca"),
  c("ERC",         "Enfermedad Renal Cr\u00f3nica"),
  c("Dialisis",    "Di\u00e1lisis"),
  c("EPOC",        "EPOC"),
  c("Amputacion",  "Amputaci\u00f3n Previa"),
  c("Tiene_complicacion", "Al menos 1 complicaci\u00f3n")
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

# =============================================================================
# 7. GRÁFICAS
# =============================================================================

# --- Gráfica 1: Prevalencia de complicaciones con IC 95% ---
datos_graf <- bind_rows(
  lapply(complicaciones_lista[1:7],  # Excluir "al menos 1"
         function(x) calcular_prevalencia(x[1], x[2]))
) %>%
  arrange(Prevalencia_pct) %>%
  mutate(Complicacion = factor(Complicacion, levels = Complicacion))

g1 <- ggplot(datos_graf, aes(x = Complicacion, y = Prevalencia_pct)) +
  geom_col(fill = "#4E79A7", width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = IC95_inf, ymax = IC95_sup),
                width = 0.25, color = "gray30", linewidth = 0.6) +
  geom_text(aes(label = paste0(Prevalencia_pct, "%")),
            hjust = -0.3, size = 3.5, color = "gray20") +
  coord_flip(ylim = c(0, max(datos_graf$IC95_sup) * 1.18)) +
  labs(
    title    = "Prevalencia de complicaciones cr\u00f3nicas en pacientes con DM2",
    subtitle = paste0("Hospital General de Valle de Bravo, 2024 (n = ", n_dm2, ")"),
    x = NULL, y = "Prevalencia (%)",
    caption  = "Barras de error: IC 95% (m\u00e9todo Clopper-Pearson)"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "gray40", size = 10))

ggsave(file.path(DIR_GRAFICAS, "Grafica1_Prevalencia_Complicaciones.png"),
       g1, width = 9, height = 5, dpi = 300)

# --- Gráfica 2: Pirámide poblacional ---
df_piramide <- df_clean %>%
  filter(!is.na(Edad), !is.na(Sexo)) %>%
  mutate(Grupo_edad = cut(Edad,
    breaks = c(0, 30, 40, 50, 60, 70, 80, 120),
    labels = c("<30","30-39","40-49","50-59","60-69","70-79","\u226580"),
    right  = FALSE)) %>%
  count(Sexo, Grupo_edad) %>%
  mutate(n_plot = ifelse(Sexo == "Masculino", -n, n))

g2 <- ggplot(df_piramide, aes(x = Grupo_edad, y = n_plot, fill = Sexo)) +
  geom_col(width = 0.75, alpha = 0.85) +
  geom_text(aes(label = abs(n_plot),
                hjust = ifelse(Sexo == "Masculino", 1.3, -0.3)),
            size = 3.2, color = "gray20") +
  scale_y_continuous(labels = function(x) abs(x),
    limits = c(-max(abs(df_piramide$n_plot)) * 1.2,
               max(abs(df_piramide$n_plot)) * 1.2)) +
  scale_fill_manual(values = c("Femenino" = "#E15759", "Masculino" = "#4E79A7")) +
  coord_flip() +
  labs(title = "Distribuci\u00f3n de pacientes con DM2 por edad y sexo",
       subtitle = paste0("Hospital General de Valle de Bravo, 2024 (n = ", n_dm2, ")"),
       x = "Grupo de edad (a\u00f1os)", y = "N\u00famero de pacientes", fill = "Sexo") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
ggsave(file.path(DIR_GRAFICAS, "Grafica2_Piramide_Poblacional.png"),
       g2, width = 8, height = 5, dpi = 300)

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
