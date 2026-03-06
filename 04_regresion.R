# =============================================================================
# 04_regresión.R — REGRESIÓN LOGÍSTICA BINARIA
# =============================================================================
#   Outcome principal: ERC (Sí/No)
#   Análisis secundarios: HTA, Diálisis, Retinopatía, IC, Amputación
# =============================================================================

source("config.R")   # Carga rutas, constantes y ALPHA
source("00_setup.R") # Carga librerías y crea carpetas

df_clean <- readRDS("df_clean_tesis.rds")
theme_gtsummary_language("es", decimal.mark = ".", big.mark = ",")

if (!dir.exists("tablas"))   dir.create("tablas")
if (!dir.exists("graficas")) dir.create("graficas")

# --------------------------------------------------------------------------
# 3. PREDICTORES BASE
# --------------------------------------------------------------------------
# Candidatos seleccionados con p < 0.20 en bivariado (Bloque 3B)
# + plausibilidad biológica (Edad, Sexo).
# EXCLUIDOS: Creatinina, BUN, Urea (consecuencia de ERC, no predictores).

predictores_base <- c("Edad", "Sexo_bin", "TAS", "HTA_bin",
                      "Glucosa", "Hb", "Acido_Urico", "TyG")

# --------------------------------------------------------------------------
# 4. FUNCIÓN PRINCIPAL — REGRESIÓN + TABLA + FOREST PLOT + ROC
# --------------------------------------------------------------------------

correr_regresion <- function(outcome_var, etiqueta_outcome, vars_predictoras) {
  
  cat(sprintf("\n%s\nOUTCOME: %s\n%s\n",
              strrep("=", 50), etiqueta_outcome, strrep("=", 50)))
  
  # 4.1 Preparar datos
  df_modelo <- df_clean %>%
    mutate(
      outcome_bin = as.integer(.data[[outcome_var]] == "Sí"),
      HTA_bin     = as.integer(HTA    == "Sí"),
      Sexo_bin    = as.integer(Sexo   == "Masculino")
    ) %>%
    filter(!is.na(outcome_bin))
  
  # Casos completos para todas las variables del modelo
  df_completo <- df_modelo %>%
    select(outcome_bin, all_of(vars_predictoras)) %>%
    na.omit()
  predictor_redundante <- paste0(outcome_var, "_bin")
  vars_limpias <- vars_predictoras[vars_predictoras != predictor_redundante]
  
  vars_limpias <- vars_limpias[sapply(vars_limpias, function(v) {
    length(unique(na.omit(df_completo[[v]]))) > 1
  })]
  
  n_eventos <- sum(df_completo$outcome_bin)
  n_no      <- sum(df_completo$outcome_bin == 0)
  max_vars  <- floor(n_eventos / 10)
  
  cat(sprintf("n completos = %d | Sí = %d | No = %d\n",
              nrow(df_completo), n_eventos, n_no))
  cat(sprintf("Predictores permitidos (regla 10:1): %d\n", max_vars))
  
  # 4.2 Verificar mínimo de eventos
  if (n_eventos < 10) {
    cat("⚠️  Menos de 10 eventos — regresión logística no recomendada. Saltando.\n")
    return(NULL)
  }
  
  # 4.3 Modelos
  modelo_nulo <- glm(outcome_bin ~ 1,
                     data = df_completo, family = binomial())
  
  formula_completa <- as.formula(
    paste("outcome_bin ~", paste(vars_limpias, collapse = " + "))
  )
  
  modelo_completo <- glm(formula_completa,
                         data = df_completo, family = binomial())
  
  # Stepwise backward por AIC
  modelo_final <- step(modelo_completo, direction = "backward", trace = 0)
  
  cat("\n--- Modelo final ---\n")
  print(summary(modelo_final))
  
  # 4.4 Validación
  
  # VIF
  vif_vals <- tryCatch(car::vif(modelo_final), error = function(e) NULL)
  if (!is.null(vif_vals)) {
    cat("VIF:", paste(names(vif_vals), round(vif_vals, 2),
                      sep = " = ", collapse = " | "), "\n")
    if (any(vif_vals > 5)) cat("⚠️  VIF > 5 en alguna variable\n") else
      cat("✅ Sin colinealidad relevante\n")
  }
  
  # Hosmer-Lemeshow
  hl <- ResourceSelection::hoslem.test(
    x = df_completo$outcome_bin,
    y = fitted(modelo_final),
    g = 10
  )
  cat(sprintf("Hosmer-Lemeshow p = %.3f %s\n",
              hl$p.value,
              ifelse(hl$p.value > 0.05, "✅ Buen ajuste", "⚠️  Revisar ajuste")))
  
  # R² Nagelkerke
  n        <- nrow(df_completo)
  ll_nulo  <- as.numeric(logLik(modelo_nulo))
  ll_final <- as.numeric(logLik(modelo_final))
  r2_cox   <- 1 - exp((2 / n) * (ll_nulo - ll_final))
  r2_nagel <- r2_cox / (1 - exp((2 / n) * ll_nulo))
  cat(sprintf("R² Nagelkerke = %.3f\n", r2_nagel))
  cat(sprintf("AIC nulo = %.1f | AIC final = %.1f\n",
              AIC(modelo_nulo), AIC(modelo_final)))
  
  # 4.5 Tabla Word (gtsummary)
  tbl <- modelo_final %>%
    tbl_regression(
      exponentiate = TRUE,
      pvalue_fun   = ~ style_pvalue(.x, digits = 3),
      estimate_fun = ~ style_ratio(.x, digits = 2)
    ) %>%
    bold_p(t = 0.05) %>%
    bold_labels() %>%
    add_glance_table(include = c(nobs, AIC, logLik)) %>%
    modify_caption(sprintf(
      "**Tabla.** Predictores independientes de %s en DM2", etiqueta_outcome
    )) %>%
    modify_header(estimate ~ "**OR ajustado**", ci ~ "**IC 95%**")
  
  ft <- tbl %>%
    as_flex_table() %>%
    add_footer_lines(
      values = as_paragraph(
        as_b("OR"), ": Odds Ratio ajustado. ",
        as_b("IC 95%"), ": Intervalo de confianza al 95%. ",
        "Selección por stepwise backward (AIC). ",
        as_b("p"), " en negrita: p < 0.05. ",
        sprintf("R² Nagelkerke = %.3f | Hosmer-Lemeshow p = %.3f.",
                r2_nagel, hl$p.value)
      )
    ) %>%
    fontsize(size = 9, part = "footer") %>%
    italic(part = "footer") %>%
    bold(part = "header") %>%
    autofit()
  
  ruta_tabla <- paste0("tablas/Tabla_Regresion_", outcome_var, ".docx")
  save_as_docx(ft, path = ruta_tabla)
  cat(sprintf("✅ Tabla guardada: %s\n", ruta_tabla))
  
  # 4.6 Forest plot
  coef_df <- broom::tidy(modelo_final,
                         exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      # Etiquetas legibles para los términos
      term = case_when(
        term == "Sexo_bin"    ~ "Sexo masculino",
        term == "HTA_bin"     ~ "Hipertensión Arterial",
        term == "TAS"         ~ "TA Sistólica (mmHg)",
        term == "TAD"         ~ "TA Diastólica (mmHg)",
        term == "Edad"        ~ "Edad (años)",
        term == "IMC"         ~ "IMC (kg/m²)",
        term == "Glucosa"     ~ "Glucosa (mg/dL)",
        term == "Hb"          ~ "Hemoglobina (g/dL)",
        term == "Acido_Urico" ~ "Ácido Úrico (mg/dL)",
        term == "Colesterol"  ~ "Colesterol (mg/dL)",
        term == "Trigliceridos" ~ "Triglicéridos (mg/dL)",
        term == "TyG"         ~ "Índice TyG",
        TRUE                  ~ term   # Si hay otro, lo deja como está
      ),
      sig  = p.value < 0.05,
      term = reorder(term, estimate)
    )
  
  g_forest <- ggplot(coef_df, aes(x = estimate, y = term, color = sig)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                  width = 0.25, linewidth = 0.7) +
    geom_point(size = 3.5) +
    scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "#E15759"),
                       labels = c("p ≥ 0.05", "p < 0.05")) +
    scale_x_log10() +
    labs(
      title    = paste("Forest plot:", etiqueta_outcome),
      subtitle = "Regresión logística binaria — OR ajustados con IC 95%",
      x        = "Odds Ratio ajustado (escala log)",
      y        = NULL,
      color    = "Significancia",
      caption  = sprintf(
        "DM2: Diabetes mellitus tipo 2.\nR² Nagelkerke = %.3f | Hosmer-Lemeshow p = %.3f",
        r2_nagel, hl$p.value)
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(color = "gray40", size = 10),
      plot.caption  = element_text(color = "gray50", size = 8),
      legend.position = "bottom"
    )
  
  ruta_forest <- paste0("graficas/ForestPlot_", outcome_var, ".png")
  ggsave(ruta_forest, plot = g_forest, width = 8, height = 5, dpi = 300)
  cat(sprintf("✅ Forest plot guardado: %s\n", ruta_forest))
  
  # 4.7 Curva ROC
  prob    <- predict(modelo_final, type = "response")
  roc_obj <- pROC::roc(df_completo$outcome_bin, prob,
                       ci = TRUE, quiet = TRUE)
  auc_v   <- round(as.numeric(pROC::auc(roc_obj)), 3)
  auc_ic  <- round(as.numeric(pROC::ci(roc_obj))[c(1, 3)], 3)
  
  coords_opt <- pROC::coords(roc_obj, "best", best.method = "youden",
                             ret = c("threshold", "sensitivity", "specificity"))
  
  cat(sprintf("AUC = %.3f (IC 95%%: %.3f–%.3f)\n", auc_v, auc_ic[1], auc_ic[2]))
  cat(sprintf("Punto óptimo (Youden): corte = %.3f | Sens = %.1f%% | Esp = %.1f%%\n",
              coords_opt$threshold,
              coords_opt$sensitivity * 100,
              coords_opt$specificity * 100))
  
  ruta_roc <- paste0("graficas/ROC_", outcome_var, ".png")
  png(ruta_roc, width = 1800, height = 1800, res = 300)
  pROC::plot.roc(
    roc_obj,
    col             = "#E15759",
    lwd             = 2.5,
    print.auc       = TRUE,
    print.auc.x     = 0.4,
    print.auc.y     = 0.15,
    auc.polygon     = TRUE,
    auc.polygon.col = "#E1575920",
    grid            = TRUE,
    grid.col        = "gray90",
    main  = paste("Curva ROC —", etiqueta_outcome),
    xlab  = "1 - Especificidad",
    ylab  = "Sensibilidad"
  )
  points(1 - coords_opt$specificity, coords_opt$sensitivity,
         pch = 19, col = "#4E79A7", cex = 1.5)
  legend("bottomright",
         legend = c(
           sprintf("AUC = %.3f (IC 95%%: %.3f–%.3f)", auc_v, auc_ic[1], auc_ic[2]),
           sprintf("Corte óptimo = %.3f", coords_opt$threshold),
           sprintf("Sens = %.1f%% | Esp = %.1f%%",
                   coords_opt$sensitivity * 100, coords_opt$specificity * 100)
         ),
         col = c("#E15759", "#4E79A7", "white"),
         pch = c(NA, 19, NA),
         lty = c(1, NA, NA),
         lwd = c(2, NA, NA),
         bty = "n", cex = 0.8)
  dev.off()
  cat(sprintf("✅ Curva ROC guardada: %s\n", ruta_roc))
  
  # 4.8 Retornar resultados para uso posterior
  return(list(
    modelo   = modelo_final,
    r2_nagel = r2_nagel,
    hl       = hl,
    auc      = auc_v,
    auc_ic   = auc_ic,
    roc      = roc_obj,
    coef_df  = coef_df
  ))
}

# --------------------------------------------------------------------------
# 5. CORRER PARA TODAS LAS COMPLICACIONES
# --------------------------------------------------------------------------

res_ERC        <- correr_regresion("ERC",         "Enfermedad Renal Crónica",   predictores_base)
res_HTA        <- correr_regresion("HTA",         "Hipertensión Arterial",      predictores_base)
res_Dialisis   <- correr_regresion("Dialisis",    "Diálisis",                   predictores_base)
res_Retinopatia<- correr_regresion("Retinopatia", "Retinopatía Diabética",      predictores_base)
res_IC         <- correr_regresion("IC",          "Insuficiencia Cardíaca",      predictores_base)
res_Amputacion <- correr_regresion("Amputacion",  "Amputación",                 predictores_base)

# --------------------------------------------------------------------------
# 6. TABLA RESUMEN DE TODOS LOS MODELOS
# --------------------------------------------------------------------------

resumen_modelos <- bind_rows(
  lapply(list(
    list(nombre = "ERC",          res = res_ERC,         etiq = "Enfermedad Renal Crónica"),
    list(nombre = "HTA",          res = res_HTA,         etiq = "Hipertensión Arterial"),
    list(nombre = "Dialisis",     res = res_Dialisis,    etiq = "Diálisis"),
    list(nombre = "Retinopatia",  res = res_Retinopatia, etiq = "Retinopatía Diabética"),
    list(nombre = "IC",           res = res_IC,          etiq = "Insuficiencia Cardíaca"),
    list(nombre = "Amputacion",   res = res_Amputacion,  etiq = "Amputación")
  ), function(x) {
    if (is.null(x$res)) return(NULL)
    data.frame(
      Complicación     = x$etiq,
      `R² Nagelkerke`  = round(x$res$r2_nagel, 3),
      `HL p`           = round(x$res$hl$p.value, 3),
      AUC              = x$res$auc,
      `IC95 AUC`       = paste0(x$res$auc_ic[1], "–", x$res$auc_ic[2]),
      check.names      = FALSE
    )
  })
)

cat("\n=== RESUMEN DE TODOS LOS MODELOS ===\n")
print(resumen_modelos)

# Guardar tabla resumen en Word
ft_resumen <- resumen_modelos %>%
  flextable() %>%
  set_caption("Tabla 8. Resumen de modelos de regresión logística por complicación") %>%
  add_footer_lines(
    "R² Nagelkerke: proporción de varianza explicada. HL p: Hosmer-Lemeshow (p > 0.05 = buen ajuste). AUC: área bajo la curva ROC con IC 95%."
  ) %>%
  fontsize(size = 9, part = "footer") %>%
  italic(part = "footer") %>%
  bold(part = "header") %>%
  autofit()

save_as_docx(ft_resumen, path = "tablas/Tabla_8_Resumen_Modelos.docx")
cat("✅ Tabla resumen guardada: tablas/Tabla_8_Resumen_Modelos.docx\n")