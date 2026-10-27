# =============================================================================
# 03_bivariado.R — Bivariado para Supplementary Appendix (Tablas S1-S9)
# =============================================================================
# Protocolo: Tablas suplementarias S1-S9 para The Lancet
# S1-S8: Comparaciones univariadas por complicación (variables continuas)
# S9: Asociaciones entre variables categóricas (Chi²/Fisher)
# =============================================================================

source("config.R")
source("00_setup.R")
df_clean <- readRDS(file.path(DIR_DATA, "df_clean_tesis.rds"))

# Etiquetas en INGLÉS (formato Lancet)
etiquetas_en <- list(
  Sexo = "Sex assigned at birth",
  Edad = "Age",
  Peso = "Weight",
  Talla = "Height",
  IMC = "BMI",
  IMC_cat = "BMI category",
  TAS = "Systolic blood pressure",
  TAD = "Diastolic blood pressure",
  FC = "Heart rate",
  FR = "Respiratory rate",
  HTA = "Arterial hypertension",
  Retinopatia = "Diabetic retinopathy",
  IC = "Heart failure",
  ERC = "Chronic kidney disease",
  Dialisis = "Dialysis",
  EPOC = "COPD",
  Amputacion = "Prior amputation",
  Tiene_complicacion = "At least 1 complication",
  N_Complicaciones = "Number of complications",
  Glucosa = "Fasting glucose",
  Hb = "Hemoglobin",
  Colesterol = "Total cholesterol",
  Trigliceridos = "Triglycerides",
  BUN = "BUN",
  Creatinina = "Creatinine",
  Urea = "Urea",
  Acido_Urico = "Uric acid",
  TFG_CKD_EPI = "eGFR",
  TyG = "TyG index"
)

# Definición de variables
vars_continuas <- c("Edad", "IMC", "TAS", "TAD", "Glucosa", "Hb",
                    "Colesterol", "Trigliceridos", "BUN", "Creatinina",
                    "Urea", "Acido_Urico", "TFG_CKD_EPI", "TyG")
vars_categoricas_exp <- c("Sexo", "IMC_cat")
outcomes <- c("ERC", "HTA", "Retinopatia", "IC",
              "Dialisis", "EPOC", "Amputacion", "Tiene_complicacion")

# Función auxiliar: guardar flextable en Word
guardar_ft <- function(ft, nombre, pie = NULL) {
  if (!is.null(pie)) {
    ft <- add_footer_lines(ft, values = pie) %>%
      fontsize(size = 9, part = "footer") %>% italic(part = "footer")
  }
  ft <- bold(ft, part = "header") %>% autofit()
  ruta <- file.path(DIR_TABLAS, paste0(nombre, ".docx"))
  save_as_docx(ft, path = ruta)
  cat("  ✅ Guardado:", ruta, "\n")
}

# Función para reemplazar punto decimal por punto medio
fmt_punto_medio <- function(x) {
  if (is.numeric(x)) {
    return(gsub("\\.", "·", formatC(x, format = "f", digits = 2)))
  }
  return(x)
}

# =============================================================================
# BLOQUE 3A — CHI² / FISHER EXACTO + OR CRUDO (Tabla S9)
# =============================================================================
cat("\n=== BLOQUE 3A: CHI² / FISHER (Tabla S9) ===\n")
set.seed(SEMILLA)

calcular_chi_or <- function(df, exposicion, outcome) {
  x <- df[[exposicion]]
  y <- df[[outcome]]
  
  if (is.factor(x) && all(levels(x) %in% c("No","Yes"))) {
    x <- factor(x, levels = c("No","Yes"))
  }
  y <- factor(y, levels = c("No","Yes"))
  
  tbl <- table(x, y)
  if (any(dim(tbl) < 2)) return(NULL)
  
  esperadas  <- tryCatch(chisq.test(tbl, correct = FALSE)$expected,
                         error = function(e) matrix(0))
  es_2x2     <- all(dim(tbl) == c(2, 2))
  usar_fisher <- any(esperadas < 5)
  
  if (usar_fisher) {
    test_res <- if (es_2x2) {
      fisher.test(tbl)
    } else {
      fisher.test(tbl, simulate.p.value = TRUE, B = 2000)
    }
    p_val    <- test_res$p.value
    test_nom <- "Fisher exact"
    or       <- if (es_2x2 && !is.null(test_res$estimate))
      round(test_res$estimate, 2) else NA
    ic_inf   <- if (es_2x2 && !is.null(test_res$conf.int))
      round(test_res$conf.int[1], 2) else NA
    ic_sup   <- if (es_2x2 && !is.null(test_res$conf.int))
      round(test_res$conf.int[2], 2) else NA
  } else {
    test_res <- chisq.test(tbl, correct = TRUE)
    p_val    <- test_res$p.value
    test_nom <- "Chi-squared"
    if (es_2x2) {
      a <- tbl["Yes","Yes"]
      b <- tbl["Yes","No"]
      c <- tbl["No","Yes"]
      d <- tbl["No","No"]
      if (any(c(b, c) == 0)) {
        or <- NA; ic_inf <- NA; ic_sup <- NA
      } else {
        or    <- round((a * d) / (b * c), 2)
        ee    <- sqrt(1/a + 1/b + 1/c + 1/d)
        ic_inf <- round(exp(log(or) - 1.96 * ee), 2)
        ic_sup <- round(exp(log(or) + 1.96 * ee), 2)
      }
    } else {
      or <- ic_inf <- ic_sup <- NA
    }
  }
  
  data.frame(
    Exposure = etiquetas_en[[exposicion]],
    Outcome  = etiquetas_en[[outcome]],
    Test     = test_nom,
    OR       = or,
    IC95     = ifelse(!is.na(or), paste0(ic_inf, " – ", ic_sup), "—"),
    p_value  = round(p_val, 4),
    Sig      = ifelse(!is.na(p_val) & p_val < ALPHA, "*", "")
  )
}

# Evaluar TODAS las combinaciones categóricas × cada complicación
resultados_chi <- bind_rows(
  lapply(outcomes, function(out) {
    bind_rows(lapply(vars_categoricas_exp, function(exp) {
      if (exp == out) return(NULL)
      tryCatch(calcular_chi_or(df_clean, exp, out),
               error = function(e) NULL)
    }))
  })
)

# Asociaciones entre complicaciones
pares_entre_complicaciones <- list(
  c("HTA","ERC"), c("HTA","IC"), c("HTA","Dialisis"),
  c("ERC","Dialisis"), c("ERC","Amputacion"), c("ERC","Retinopatia"),
  c("HTA","Retinopatia"), c("IC","ERC"), c("EPOC","IC")
)
resultados_chi <- bind_rows(resultados_chi,
                            bind_rows(lapply(pares_entre_complicaciones, function(p) {
                              tryCatch(calcular_chi_or(df_clean, p[1], p[2]),
                                       error = function(e) NULL)
                            }))
)

cat("  Asociaciones calculadas:", nrow(resultados_chi), "\n")

# Guardar Tabla S9
ft_chi <- resultados_chi %>%
  rename(`Exposure` = Exposure, `Outcome` = Outcome,
         `Test` = Test, `Crude OR` = OR, `95% CI` = IC95, `p value` = p_value, ` ` = Sig) %>%
  flextable() %>%
  bold(~ `p value` < ALPHA) %>%
  set_caption("**Supplementary Table S9.** Unadjusted bivariate associations between categorical clinical variables and chronic complications (Chi-squared and Fisher exact tests).")

guardar_ft(ft_chi, "Tabla_S9_Chi2_Fisher",
           "OR: crude odds ratio. 95% CI: 95% confidence interval. Fisher exact test used when expected frequency <5. * p < 0·05.")

# =============================================================================
# BLOQUE 3B — t de STUDENT / MANN-WHITNEY (Tablas S1-S8)
# =============================================================================
cat("\n=== BLOQUE 3B: t / MANN-WHITNEY (Tablas S1-S8) ===\n")

vars_normales <- c()
for (v in vars_continuas) {
  datos <- na.omit(df_clean[[v]])
  if (length(datos) >= 3) {
    p <- shapiro.test(datos)$p.value
    if (p > ALPHA) vars_normales <- c(vars_normales, v)
  }
}
vars_no_normales <- setdiff(vars_continuas, vars_normales)

cat("  Variables normales (t-test):", paste(vars_normales, collapse = ", "), "\n")
cat("  Variables no normales (Mann-Whitney):", paste(vars_no_normales, collapse = ", "), "\n")

# Mapeo de outcomes a números de tabla suplementaria
mapeo_tablas <- list(
  "ERC" = "S1",
  "HTA" = "S2",
  "Dialisis" = "S3",
  "Retinopatia" = "S4",
  "IC" = "S5",
  "EPOC" = "S6",
  "Amputacion" = "S7",
  "Tiene_complicacion" = "S8"
)

crear_tabla_bivariada_lancet <- function(outcome_var, num_tabla) {
  cat(sprintf("  Generando Tabla %s: %s\n", num_tabla, outcome_var))
  etiq_out <- etiquetas_en[[outcome_var]]
  
  tbl <- df_clean %>%
    select(all_of(c(outcome_var, vars_continuas))) %>%
    tbl_summary(
      by        = all_of(outcome_var),
      statistic = list(all_continuous() ~ "{median} ({p25} – {p75})"),
      digits    = all_continuous() ~ 1,
      label     = etiquetas_en,
      missing   = "ifany",
      missing_text = "Missing data (n)"
    ) %>%
    add_p(
      test = c(
        setNames(rep(list("t.test"), length(vars_normales)), vars_normales),
        setNames(rep(list("wilcox.test"), length(vars_no_normales)), vars_no_normales)
      ),
      pvalue_fun = ~ style_pvalue(.x, digits = 3)
    ) %>%
    add_overall() %>% bold_labels() %>% bold_p(t = ALPHA) %>%
    modify_caption(paste0("**Supplementary Table ", num_tabla, ".** Univariable comparison of clinical and laboratory characteristics according to the presence of ", etiq_out, ".")) %>%
    modify_fmt_fun(
      update = list(
        all_stat_cols() ~ function(x) gsub("\\.", "·", x)
      )
    )
  
  ft <- tbl %>% as_flex_table() %>%
    add_footer_lines(
      paste0("Data are median (IQR) unless otherwise specified. ",
             "Normal variables (Shapiro-Wilk p>0·05): ",
             paste(vars_normales, collapse = ", "),
             " — compared using Student t-test. ",
             "Non-normal variables compared using Mann-Whitney U test. ",
             "Bold p values indicate statistical significance (p < 0·05).")
    ) %>%
    fontsize(size = 9, part = "footer") %>% italic(part = "footer") %>%
    bold(part = "header") %>% autofit()
  
  ruta <- file.path(DIR_TABLAS, paste0("Tabla_", num_tabla, "_Bivariado_", outcome_var, ".docx"))
  save_as_docx(ft, path = ruta)
  cat("    ✅", ruta, "\n")
  return(tbl)
}

# Generar Tablas S1-S8
tbl_S1 <- crear_tabla_bivariada_lancet("ERC", "S1")
tbl_S2 <- crear_tabla_bivariada_lancet("HTA", "S2")
tbl_S3 <- crear_tabla_bivariada_lancet("Dialisis", "S3")
tbl_S4 <- crear_tabla_bivariada_lancet("Retinopatia", "S4")
tbl_S5 <- crear_tabla_bivariada_lancet("IC", "S5")
tbl_S6 <- crear_tabla_bivariada_lancet("EPOC", "S6")
tbl_S7 <- crear_tabla_bivariada_lancet("Amputacion", "S7")
tbl_S8 <- crear_tabla_bivariada_lancet("Tiene_complicacion", "S8")

# =============================================================================
# BLOQUE 3C — CORRELACIONES (opcional, puede ir en suplementario también)
# =============================================================================
cat("\n=== BLOQUE 3C: CORRELACIONES ===\n")

vars_corr  <- c("Edad", "IMC", "Glucosa", "TFG_CKD_EPI", "Creatinina",
                "BUN", "Urea", "Colesterol", "Trigliceridos", "Acido_Urico",
                "Hb", "TyG")
etiq_corr  <- c("Age","BMI","Glucose","eGFR","Creatinine","BUN","Urea",
                "Cholesterol","Triglycerides","Uric acid","Hemoglobin","TyG index")

corr_spearman <- df_clean %>%
  select(all_of(vars_corr)) %>%
  rstatix::cor_test(method = "spearman") %>%
  filter(var1 != var2) %>%
  mutate(par = paste(pmin(var1, var2), pmax(var1, var2), sep = "_")) %>%
  distinct(par, .keep_all = TRUE) %>% select(-par) %>%
  mutate(
    r     = round(cor, 3),
    p_adj = round(p.adjust(p, method = "holm"), 4),
    Sig   = case_when(
      p_adj < 0.001 ~ "***", p_adj < 0.01 ~ "**",
      p_adj < 0.05  ~ "*",   TRUE          ~ ""
    )
  ) %>%
  select(var1, var2, r, p_adj, Sig) %>%
  arrange(desc(abs(r)))

cat("  Top 10 correlaciones (Spearman + Holm):\n")
print(head(corr_spearman %>%
             rename(`Variable 1`=var1,`Variable 2`=var2,
                    `r Spearman`=r,`p adjusted`=p_adj), 10))

ft_corr <- corr_spearman %>%
  rename(`Variable 1`=var1,`Variable 2`=var2,
         `r Spearman`=r,`p adjusted`=p_adj,` `=Sig) %>%
  flextable() %>%
  bold(~ `p adjusted` < ALPHA) %>%
  set_caption("**Supplementary Table S10.** Spearman correlations between quantitative variables with Holm adjustment.") %>%
  add_footer_lines(paste0(
    "r: Spearman correlation coefficient. ",
    "p adjusted by Holm method. ",
    "* p<0·05  ** p<0·01  *** p<0·001. ",
    "Normal variables (Shapiro-Wilk p>0·05): ",
    paste(vars_normales, collapse = ", "), "."
  )) %>%
  fontsize(size = 9, part = "footer") %>% italic(part = "footer") %>%
  bold(part = "header") %>% autofit()

guardar_ft(ft_corr, "Tabla_S10_Correlaciones_Spearman", NULL)

cat("\n=== BLOQUE 3 COMPLETADO ===\n")
cat("Tablas suplementarias: S1-S10\n")
