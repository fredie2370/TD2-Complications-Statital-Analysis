# =============================================================================
# 03_bivariado.R — Análisis bivariado completo
# =============================================================================
# Protocolo: Sección VI.viii.c (bivariado) + VI.viii.d (correlación)
#
# BLOQUE 3A: Chi² / Fisher exacto + OR crudo para TODAS las combinaciones
#            de variables categóricas × complicaciones (no solo 15 pares)
# BLOQUE 3B: t de Student (normales) o Mann-Whitney (no normales) para
#            variables continuas vs CADA UNA de las 7 complicaciones
# BLOQUE 3C: Spearman (variables no normales) + Pearson (variables normales)
#            con ajuste Holm
# =============================================================================

source("config.R")
source("00_setup.R")
df_clean <- readRDS(file.path(DIR_DATA, "df_clean_tesis.rds"))

# Etiquetas (igual que Bloque 2)
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

# Definición de variables predictoras y outcomes
vars_continuas <- c("Edad", "IMC", "TAS", "TAD", "Glucosa", "Hb",
                    "Colesterol", "Trigliceridos", "BUN", "Creatinina",
                    "Urea", "Acido_Urico", "TFG_CKD_EPI", "TyG")
vars_categoricas_exp <- c("Sexo", "IMC_cat")   # Exposiciones categóricas
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
  cat("  \u2705 Guardado:", ruta, "\n")
}

# =============================================================================
# BLOQUE 3A — CHI² / FISHER EXACTO + OR CRUDO
# =============================================================================
# FIX: Cubre sistemáticamente TODAS las combinaciones vars_categoricas × outcomes.
# FIX: Fisher SIN simulate.p.value para tablas 2×2 (así sí devuelve OR + IC).
# FIX: Niveles de los factores verificados antes de calcular OR manual.
# =============================================================================
cat("\n=== BLOQUE 3A: CHI\u00b2 / FISHER ===\n")
set.seed(SEMILLA)  # Reproducibilidad para simulate.p.value en tablas >2x2

calcular_chi_or <- function(df, exposicion, outcome) {
  x <- df[[exposicion]]
  y <- df[[outcome]]

  # Forzar niveles explícitos para que la tabla tenga orden consistente
  if (is.factor(x) && all(levels(x) %in% c("No","S\u00ed"))) {
    x <- factor(x, levels = c("No","S\u00ed"))
  }
  y <- factor(y, levels = c("No","S\u00ed"))

  tbl <- table(x, y)
  if (any(dim(tbl) < 2)) return(NULL)  # No hay variación suficiente

  # Decidir test según frecuencias esperadas (correct=FALSE solo para decidir)
  esperadas  <- tryCatch(chisq.test(tbl, correct = FALSE)$expected,
                         error = function(e) matrix(0))
  es_2x2     <- all(dim(tbl) == c(2, 2))
  usar_fisher <- any(esperadas < 5)

  if (usar_fisher) {
    # FIX: Para tablas 2×2, NO usar simulate (desactiva OR y conf.int)
    test_res <- if (es_2x2) {
      fisher.test(tbl)
    } else {
      fisher.test(tbl, simulate.p.value = TRUE, B = 2000)
    }
    p_val    <- test_res$p.value
    test_nom <- "Fisher exacto"
    or       <- if (es_2x2 && !is.null(test_res$estimate))
      round(test_res$estimate, 2) else NA
    ic_inf   <- if (es_2x2 && !is.null(test_res$conf.int))
      round(test_res$conf.int[1], 2) else NA
    ic_sup   <- if (es_2x2 && !is.null(test_res$conf.int))
      round(test_res$conf.int[2], 2) else NA
  } else {
    test_res <- chisq.test(tbl, correct = TRUE)
    p_val    <- test_res$p.value
    test_nom <- "Chi\u00b2"
    if (es_2x2) {
      # FIX: Orden verificado — niveles son c("No","Sí") para ambas variables
      # Tabla: filas = x (exposición), columnas = y (outcome)
      #   [1,1] = No/No, [1,2] = No/Sí, [2,1] = Sí/No, [2,2] = Sí/Sí
      a <- tbl["S\u00ed","S\u00ed"]  # Expuesto y con outcome
      b <- tbl["S\u00ed","No"]       # Expuesto sin outcome
      c <- tbl["No","S\u00ed"]       # No expuesto con outcome
      d <- tbl["No","No"]            # No expuesto sin outcome
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
    Exposicion = exposicion,
    Outcome    = outcome,
    Test       = test_nom,
    OR         = or,
    IC95       = ifelse(!is.na(or), paste0(ic_inf, " \u2013 ", ic_sup), "\u2014"),
    p_valor    = round(p_val, 4),
    Sig        = ifelse(!is.na(p_val) & p_val < ALPHA, "*", "")
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

# Añadir asociaciones entre complicaciones (pares clínicamente relevantes)
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

# Guardar tabla en Word
ft_chi <- resultados_chi %>%
  rename(`Exposición` = Exposicion, Desenlace = Outcome,
         `OR crudo` = OR, `IC 95%` = IC95, p = p_valor, ` ` = Sig) %>%
  flextable() %>%
  bold(~ p < ALPHA) %>%
  set_caption("Tabla 4. Asociaciones entre variables categ\u00f3ricas (Chi\u00b2 / Fisher exacto)")
guardar_ft(ft_chi, "Tabla_4_Chi2_Fisher",
  "OR: Odds Ratio crudo. IC 95% por m\u00e9todo Wald. Fisher exacto cuando frecuencia esperada < 5. * p < 0.05.")

# =============================================================================
# BLOQUE 3B — t de STUDENT / MANN-WHITNEY para las 7 complicaciones
# =============================================================================
# FIX: Se aplica t de Student para variables normales (protocolo lo exige).
# FIX: Se generan tablas para TODAS las complicaciones (no solo 4).
# =============================================================================
cat("\n=== BLOQUE 3B: t / MANN-WHITNEY ===\n")

# Recuperar resultados de normalidad del Bloque 2
# (si se corre de forma independiente, recalcular)
vars_normales <- c()
for (v in vars_continuas) {
  datos <- na.omit(df_clean[[v]])
  if (length(datos) >= 3) {
    p <- shapiro.test(datos)$p.value
    if (p > ALPHA) vars_normales <- c(vars_normales, v)
  }
}
vars_no_normales <- setdiff(vars_continuas, vars_normales)

cat("  Variables normales (t de Student):", paste(vars_normales, collapse = ", "), "\n")
cat("  Variables no normales (Mann-Whitney):", paste(vars_no_normales, collapse = ", "), "\n")

# FIX: Función auxiliar que aplica el test correcto según normalidad
crear_tabla_bivariada <- function(outcome_var, nombre_archivo) {
  cat(sprintf("  Generando tabla para: %s\n", outcome_var))
  etiq_out <- etiquetas[[outcome_var]]

  tbl <- df_clean %>%
    select(all_of(c(outcome_var, vars_continuas))) %>%
    tbl_summary(
      by        = all_of(outcome_var),
      statistic = list(all_continuous() ~ "{median} ({p25} - {p75})"),
      digits    = all_continuous() ~ 2,
      label     = etiquetas,
      missing   = "ifany",
      missing_text = "Sin registro"
    ) %>%
    add_p(
      test = c(
        # t de Student para variables normales
        setNames(rep(list("t.test"), length(vars_normales)), vars_normales),
        # Mann-Whitney para variables no normales
        setNames(rep(list("wilcox.test"), length(vars_no_normales)), vars_no_normales)
      ),
      pvalue_fun = ~ style_pvalue(.x, digits = 3)
    ) %>%
    add_overall() %>% bold_labels() %>% bold_p(t = ALPHA) %>%
    modify_caption(paste0("**Tabla.** Variables cl\u00ednicas y de laboratorio seg\u00fan ",
                          etiq_out))

  ft <- tbl %>% as_flex_table() %>%
    add_footer_lines(
      paste0("Variables normales (Shapiro-Wilk p>0.05): ",
             paste(vars_normales, collapse = ", "),
             " — comparadas con prueba t de Student. ",
             "Resto comparado con prueba U de Mann-Whitney. ",
             "p en negrita: p < 0.05. RIC: rango intercuartílico.")
    ) %>%
    fontsize(size = 9, part = "footer") %>% italic(part = "footer") %>%
    bold(part = "header") %>% autofit()

  ruta <- file.path(DIR_TABLAS, paste0(nombre_archivo, ".docx"))
  save_as_docx(ft, path = ruta)
  cat("    \u2705", ruta, "\n")
  return(tbl)
}

# FIX: Generar para TODAS las complicaciones (antes solo eran 4)
tbl_ERC         <- crear_tabla_bivariada("ERC",         "Tabla_5a_Bivariado_ERC")
tbl_HTA         <- crear_tabla_bivariada("HTA",         "Tabla_5b_Bivariado_HTA")
tbl_Dialisis    <- crear_tabla_bivariada("Dialisis",    "Tabla_5c_Bivariado_Dialisis")
tbl_Retinopatia <- crear_tabla_bivariada("Retinopatia", "Tabla_5d_Bivariado_Retinopatia")
tbl_IC          <- crear_tabla_bivariada("IC",          "Tabla_5e_Bivariado_IC")
tbl_EPOC        <- crear_tabla_bivariada("EPOC",        "Tabla_5f_Bivariado_EPOC")
tbl_Amputacion  <- crear_tabla_bivariada("Amputacion",  "Tabla_5g_Bivariado_Amputacion")
tbl_ComplicGen  <- crear_tabla_bivariada("Tiene_complicacion",
                                          "Tabla_5h_Bivariado_CualquierComplicacion")

# =============================================================================
# BLOQUE 3C — CORRELACIONES (Pearson + Spearman según distribución)
# =============================================================================
# FIX: Pearson para variables normales, Spearman para no normales.
# =============================================================================
cat("\n=== BLOQUE 3C: CORRELACIONES ===\n")

vars_corr  <- c("Edad", "IMC", "Glucosa", "TFG_CKD_EPI", "Creatinina",
                "BUN", "Urea", "Colesterol", "Trigliceridos", "Acido_Urico",
                "Hb", "TyG")
etiq_corr  <- c("Edad","IMC","Glucosa","TFG","Creatinina","BUN","Urea",
                "Colesterol","Triglic\u00e9ridos","\u00c1c. \u00darico","Hb","\u00cdndice TyG")

# Spearman para todas (estrategia conservadora con n=233 y mayoría no-normal)
# Se notifica en pie de tabla cuáles se aplica Pearson por ser normales
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
         `r Spearman`=r,`p ajustado`=p_adj), 10))

ft_corr <- corr_spearman %>%
  rename(`Variable 1`=var1,`Variable 2`=var2,
         `r Spearman`=r,`p ajustado`=p_adj,` `=Sig) %>%
  flextable() %>%
  bold(~ `p ajustado` < ALPHA) %>%
  set_caption("Tabla 6. Correlaciones de Spearman entre variables cuantitativas") %>%
  add_footer_lines(paste0(
    "r: coeficiente de correlaci\u00f3n de Spearman. ",
    "p ajustado por m\u00e9todo de Holm. ",
    "* p<0.05  ** p<0.01  *** p<0.001. ",
    "Variables con distribuci\u00f3n normal (Shapiro-Wilk p>0.05): ",
    paste(vars_normales, collapse = ", "), "."
  )) %>%
  fontsize(size = 9, part = "footer") %>% italic(part = "footer") %>%
  bold(part = "header") %>% autofit()

guardar_ft(ft_corr, "Tabla_6_Correlaciones_Spearman", NULL)

# =============================================================================
# GRÁFICAS DEL BLOQUE 3
# =============================================================================

# --- Gráfica 5: TFG vs ERC ---
g5 <- df_clean %>% filter(!is.na(ERC)) %>%
  ggplot(aes(x = ERC, y = TFG_CKD_EPI, fill = ERC)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.x = 1.5,
                     label.y = max(df_clean$TFG_CKD_EPI, na.rm=TRUE) * 0.95) +
  scale_fill_manual(values = c("No" = "#4E79A7", "S\u00ed" = "#E15759")) +
  labs(title = "TFG seg\u00fan ERC",
       x = "Enfermedad Renal Cr\u00f3nica",
       y = "TFG CKD-EPI (mL/min/1.73m\u00b2)",
       caption = "Prueba U de Mann-Whitney. ns: p\u22650.05; * p<0.05; ** p<0.01; *** p<0.001") +
  theme_classic(base_size = 12) + theme(legend.position = "none",
                                         plot.title = element_text(face = "bold"))
ggsave(file.path(DIR_GRAFICAS, "Grafica5_TFG_vs_ERC.png"),
       g5, width = 6, height = 5, dpi = 300)

# --- Gráfica 6: Panel de laboratorio vs ERC ---
vars_box  <- c("Glucosa", "Creatinina", "Hb", "TFG_CKD_EPI")
labs_box  <- c("Glucosa (mg/dL)", "Creatinina (mg/dL)",
               "Hemoglobina (g/dL)", "TFG (mL/min/1.73m\u00b2)")
plots_box <- lapply(seq_along(vars_box), function(i) {
  v <- vars_box[i]
  test_usar <- if (v %in% vars_normales) "t.test" else "wilcox.test"
  df_clean %>% filter(!is.na(ERC), !is.na(.data[[v]])) %>%
    ggplot(aes(x = ERC, y = .data[[v]], fill = ERC)) +
    geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 1) +
    geom_jitter(width = 0.15, alpha = 0.2, size = 0.8) +
    stat_compare_means(method = test_usar, label = "p.signif", label.x = 1.5) +
    scale_fill_manual(values = c("No"="#4E79A7","S\u00ed"="#E15759")) +
    labs(x = "ERC", y = labs_box[i]) +
    theme_classic(base_size = 10) + theme(legend.position = "none")
})
g6 <- ggpubr::ggarrange(plotlist = plots_box, ncol = 2, nrow = 2)
g6 <- ggpubr::annotate_figure(g6,
  top    = ggpubr::text_grob("Variables de laboratorio seg\u00fan ERC", face="bold", size=13),
  bottom = ggpubr::text_grob("ERC: Enfermedad Renal Cr\u00f3nica.", color="gray50", size=8))
ggsave(file.path(DIR_GRAFICAS, "Grafica6_Labs_vs_ERC.png"),
       g6, width = 9, height = 8, dpi = 300)

# --- Gráfica 7: Mapa de calor de correlaciones ---
mat_corr <- df_clean %>% select(all_of(vars_corr)) %>%
  cor(method = "spearman", use = "pairwise.complete.obs")
colnames(mat_corr) <- rownames(mat_corr) <- etiq_corr

png(file.path(DIR_GRAFICAS, "Grafica7_Mapa_Correlaciones.png"),
    width = 2400, height = 2000, res = 300)
corrplot(mat_corr, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.cex = 0.85, addCoef.col = "black",
         number.cex = 0.65,
         col = colorRampPalette(c("#E15759","white","#4E79A7"))(200),
         title = "Correlaciones Spearman", mar = c(0,0,2,0), diag = FALSE)
dev.off()

# --- Gráfica 8: Dispersión TFG vs Creatinina ---
r_val <- round(cor(df_clean$Creatinina, df_clean$TFG_CKD_EPI,
                   method = "spearman", use = "complete.obs"), 3)
g8 <- df_clean %>% filter(!is.na(TFG_CKD_EPI), !is.na(Creatinina)) %>%
  ggplot(aes(x = Creatinina, y = TFG_CKD_EPI, color = ERC)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "gray30",
              linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("No"="#4E79A7","S\u00ed"="#E15759"),
                     na.value = "gray70") +
  labs(title = "Relaci\u00f3n entre Creatinina y TFG (CKD-EPI)",
       x = "Creatinina (mg/dL)", y = "TFG CKD-EPI (mL/min/1.73m\u00b2)",
       color = "ERC", caption = paste0("r Spearman = ", r_val)) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(DIR_GRAFICAS, "Grafica8_TFG_vs_Creatinina.png"),
       g8, width = 7, height = 5, dpi = 300)

cat("\n=== BLOQUE 3 COMPLETADO ===\n")
cat("Tablas: 4, 5a\u20135h, 6\n")
cat("Gr\u00e1ficas: 5\u20138\n")
