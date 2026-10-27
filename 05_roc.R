# =============================================================================
# 05_roc.R — Curvas ROC de biomarcadores individuales (AUC, IC 95% DeLong, Youden)
# Requiere: 01_limpieza.R (genera df_clean_tesis.rds)
# Genera: Tabla 9 (.docx), Gráficas 11–16, Figure_3 (.svg)
# =============================================================================

# --------------------------------------------------------------------------
# 2. CARGAR BASE Y CONFIGURACIÓN
# --------------------------------------------------------------------------

source("config.R")   # Carga rutas, constantes y ALPHA
source("00_setup.R") # Carga librerías y crea carpetas

df_clean <- readRDS(file.path(DIR_DATA, "df_clean_tesis.rds"))

# (Las carpetas de salida las crea 00_setup.R según config.R)

# --------------------------------------------------------------------------
# 3. DEFINIR BIOMARCADORES Y OUTCOMES A EVALUAR
# --------------------------------------------------------------------------
# Biomarcadores seleccionados por plausibilidad fisiopatológica
# y por haber mostrado asociación en el análisis bivariado (Bloque 3)

biomarcadores <- list(
  list(var = "Acido_Urico",  etiq = "Uric acid (mg/dL)"),
  list(var = "TyG",          etiq = "TyG index"),
  list(var = "Glucosa",      etiq = "Fasting glucose (mg/dL)"),
  list(var = "Hb",           etiq = "Hemoglobin (g/dL)"),
  list(var = "TFG_CKD_EPI",  etiq = "eGFR (mL/min/1.73m²)"),
  list(var = "Creatinina",   etiq = "Creatinine (mg/dL)"),
  list(var = "TAS",          etiq = "Systolic blood pressure  (mmHg)"),
  list(var = "IMC",          etiq = "BMI (kg/m²)")
)

# Outcomes principales
outcomes <- list(
  list(var = "ERC",         etiq = "Chronic kidney disease"),
#  list(var = "HTA",         etiq = "Hipertensión Arterial"),
  list(var = "Dialisis",    etiq = "Dialysis"),
  list(var = "Retinopatia", etiq = "Diabetic retinopathy"),
  list(var = "Amputacion",  etiq = "Prior amputation")
)

# --------------------------------------------------------------------------
# 4. FUNCIÓN: CALCULAR ROC + YOUDEN PARA UN BIOMARCADOR/OUTCOME
# --------------------------------------------------------------------------

calcular_roc_biomarcador <- function(df, biovar, bioetiq, outvar, outetiq) {
  
  # Preparar datos
  datos <- df %>%
    mutate(outcome_bin = as.integer(.data[[outvar]] == "Yes")) %>%
    select(outcome_bin, predictor = all_of(biovar)) %>%
    filter(!is.na(outcome_bin), !is.na(predictor))
  
  n_eventos <- sum(datos$outcome_bin)
  
  # Requiere mínimo 10 eventos
  if (n_eventos < 10) {
    cat(sprintf("  ⚠️  %s ~ %s: solo %d eventos, omitiendo.\n",
                bioetiq, outetiq, n_eventos))
    return(NULL)
  }
  
  # Calcular ROC
  roc_obj <- tryCatch(
    pROC::roc(datos$outcome_bin, datos$predictor,
              ci = TRUE, quiet = TRUE, direction = "auto"),
    error = function(e) NULL
  )
  
  if (is.null(roc_obj)) return(NULL)
  
  auc_v  <- round(as.numeric(pROC::auc(roc_obj)), 3)
  auc_ic <- round(as.numeric(pROC::ci(roc_obj))[c(1, 3)], 3)
  
  # Punto de corte óptimo (Youden)
  coords_opt <- pROC::coords(roc_obj, "best", best.method = "youden",
                             ret = c("threshold", "sensitivity",
                                     "specificity", "ppv", "npv"))
  
  # Interpretación del AUC
  interp <- case_when(
    auc_v >= 0.90 ~ "Outstanding",
    auc_v >= 0.80 ~ "Excellent",
    auc_v >= 0.70 ~ "Acceptable",
    auc_v >= 0.60 ~ "Unsatisfactory",
    TRUE          ~ "Without discrimination"
  )
  
  cat(sprintf("  %-35s → AUC = %.3f (%.3f–%.3f) [%s]\n",
              paste0(bioetiq, " ~ ", outetiq),
              auc_v, auc_ic[1], auc_ic[2], interp))
  
  list(
    Biomarcador    = bioetiq,
    Outcome        = outetiq,
    n              = nrow(datos),
    n_eventos      = n_eventos,
    AUC            = auc_v,
    IC95_inf       = auc_ic[1],
    IC95_sup       = auc_ic[2],
    Interpretacion = interp,
    Corte_optimo   = round(coords_opt$threshold, 3),
    Sensibilidad   = round(coords_opt$sensitivity * 100, 1),
    Especificidad  = round(coords_opt$specificity * 100, 1),
    VPP            = round(coords_opt$ppv * 100, 1),
    VPN            = round(coords_opt$npv * 100, 1),
    roc_obj        = roc_obj
  )
}

# --------------------------------------------------------------------------
# 5. CALCULAR TODAS LAS COMBINACIONES
# --------------------------------------------------------------------------

cat("=== BLOQUE 5: CURVAS ROC DE BIOMARCADORES ===\n\n")

resultados_roc <- list()

for (out in outcomes) {
  cat(sprintf("\n--- Outcome: %s ---\n", out$etiq))
  for (bio in biomarcadores) {
    res <- calcular_roc_biomarcador(
      df_clean, bio$var, bio$etiq, out$var, out$etiq
    )
    if (!is.null(res)) {
      resultados_roc[[length(resultados_roc) + 1]] <- res
    }
  }
}

# --------------------------------------------------------------------------
# 6. TABLA RESUMEN DE TODOS LOS AUC (sin la columna roc_obj)
# --------------------------------------------------------------------------

tabla_auc <- bind_rows(lapply(resultados_roc, function(x) {
  data.frame(
    Biomarcador    = x$Biomarcador,
    Outcome        = x$Outcome,
    n              = x$n,
    AUC            = x$AUC,
    IC95           = paste0(x$IC95_inf, " – ", x$IC95_sup),
    Interpretación = x$Interpretacion,
    `Corte óptimo` = x$Corte_optimo,
    `Sens (%)`     = x$Sensibilidad,
    `Esp (%)`      = x$Especificidad,
    `VPP (%)`      = x$VPP,
    `VPN (%)`      = x$VPN,
    check.names    = FALSE
  )
}))

cat("\n=== RESUMEN AUC POR BIOMARCADOR Y OUTCOME ===\n")
print(tabla_auc %>% select(Biomarcador, Outcome, AUC, IC95, Interpretación))

# Guardar tabla completa en Word
ft_auc <- tabla_auc %>%
  flextable() %>%
  set_caption(
    "Tabla 9. Capacidad discriminativa de biomarcadores individuales (Curvas ROC)"
  ) %>%
  add_footer_lines(
    values = paste0(
      "AUC: Área bajo la curva ROC con IC 95% (método DeLong). ",
      "Corte óptimo determinado por el Índice de Youden (J = Sensibilidad + Especificidad - 1). ",
      "Interpretación: Sobresaliente ≥0.90 | Excellente 0.80–0.89 | ",
      "Aceptable 0.70–0.79 | Pobre 0.60–0.69. ",
      "Sens: Sensibilidad. Esp: Especificidad. VPP: Valor predictivo positivo. ",
      "VPN: Valor predictivo negativo."
    )
  ) %>%
  fontsize(size = 9, part = "footer") %>%
  italic(part = "footer") %>%
  bold(part = "header") %>%
  # Colorear filas según interpretación
  bg(i = ~ AUC >= 0.90, bg = "#d4edda") %>%   # verde: sobresaliente
  bg(i = ~ AUC >= 0.80 & AUC < 0.90, bg = "#cce5ff") %>%  # azul: excelente
  bg(i = ~ AUC >= 0.70 & AUC < 0.80, bg = "#fff3cd") %>%  # amarillo: aceptable
  bg(i = ~ AUC < 0.70, bg = "#f8d7da") %>%    # rojo: pobre
  autofit()

ruta_tabla9 <- file.path(DIR_TABLAS, "Tabla_9_ROC_Biomarcadores.docx")
save_as_docx(ft_auc, path = ruta_tabla9)
cat("✅ Tabla guardada:", ruta_tabla9, "\n")

# --------------------------------------------------------------------------
# 7. GRÁFICA 11 — ROC COMPARATIVA: BIOMARCADORES PARA ERC
#    (todos los biomarcadores en una sola gráfica para ERC)
# --------------------------------------------------------------------------

cat("\n=== GENERANDO GRÁFICAS ===\n")

graficar_roc_comparativa <- function(outcome_var, etiqueta_outcome,
                                     nombre_archivo) {
  
  # Filtrar resultados del outcome indicado
  rocs_outcome <- Filter(function(x) x$Outcome == etiqueta_outcome,
                         resultados_roc)
  
  if (length(rocs_outcome) == 0) return(NULL)
  
  # Paleta de colores
  colores <- c("#B07AA1","#4E79A7","#F28E2B","#59A14F",
               "#E15759","#EDC948","#a18262","#FF9DA7")
  #colores <- c("#4E79A7","","#F28E2B","#59A14F",
  #             "#B07AA1","#EDC948","#a18262","#FF9DA7")
  
  # Abrir PNG
  png(file.path(DIR_GRAFICAS, paste0(nombre_archivo, ".png")),
      width = 2200, height = 1800, res = 300)
  
  # Primera curva (para inicializar el plot)
  plot(rocs_outcome[[1]]$roc_obj,
       col  = colores[1], lwd = 2,
       main = paste0("ROC curves — ", etiqueta_outcome, "\n",
                     "Individual biomarker comparison"),
       xlab = "1 - Specificity",
       ylab = "Sensitivity",
       grid = TRUE, 
       legacy.axes = TRUE ,grid.col = "gray90")
  
  # Curvas adicionales
  if (length(rocs_outcome) > 1) {
    for (i in 2:length(rocs_outcome)) {
      lines(rocs_outcome[[i]]$roc_obj,
            col = colores[i], lwd = 2)
    }
  }
  
  # Línea de no discriminación
  abline(a = 0, b = 1, lty = 2, col = "gray60")
  
  # Leyenda con AUC
  leyenda <- sapply(rocs_outcome, function(x) {
    sprintf("%s (AUC=%.3f)", x$Biomarcador, x$AUC)
  })
  
  legend("bottomright",
         legend = leyenda,
         col    = colores[seq_along(rocs_outcome)],
         lwd    = 2,
         bty    = "n",
         cex    = 0.75)
  
  dev.off()
  cat(sprintf("✅ Gráfica guardada: %s\n", file.path(DIR_GRAFICAS, paste0(nombre_archivo, ".png"))))
}

# ROC comparativa para cada outcome
graficar_roc_comparativa("ERC",         "Chronic kidney disease",
                         "Grafica11_ROC_Comparativa_ERC")
#graficar_roc_comparativa("HTA",         "Arterial hypertension",
#                         "Grafica12_ROC_Comparativa_HTA")
graficar_roc_comparativa("Dialisis",    "Dialysis",
                         "Grafica13_ROC_Comparativa_Dialisis")
graficar_roc_comparativa("Retinopatia", "Diabetic retinopathy",
                         "Grafica14_ROC_Comparativa_Retinopatia")
graficar_roc_comparativa("Amputacion",  "Prior amputation",
                         "Grafica15_ROC_Comparativa_Amputacion")

# --------------------------------------------------------------------------
# 8. GRÁFICA 16 — MAPA DE AUC: BIOMARCADORES × OUTCOMES (heatmap)
# --------------------------------------------------------------------------

# Preparar matriz de AUC para el heatmap
auc_wide <- tabla_auc %>%
  select(Biomarcador, Outcome, AUC) %>%
  pivot_wider(names_from = Outcome, values_from = AUC)

auc_long <- tabla_auc %>%
  select(Biomarcador, Outcome, AUC, Interpretación)

g16 <- ggplot(auc_long, aes(x = Outcome, y = Biomarcador, fill = AUC)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", AUC)),
            size = 3.2, color = "black") +
  scale_fill_gradientn(
    colors = c("#f8d7da", "#fff3cd", "#cce5ff", "#d4edda"),
    values  = scales::rescale(c(0.5, 0.70, 0.80, 0.90, 1.0)),
    limits  = c(0.5, 1.0),
    name    = "AUC"
  ) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  labs(
    title    = "Capacidad discriminativa de biomarcadores por complicación",
    subtitle = "Área bajo la curva ROC (AUC) — valores más altos indican mejor discriminación",
    x        = "Complicación",
    y        = "Biomarcador",
    caption  = "Verde: Sobresaliente (≥0.90) | Azul: Excellente (0.80–0.89) | Amarillo: Aceptable (0.70–0.79) | Rojo: Pobre (<0.70)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 9),
    plot.caption  = element_text(color = "gray50", size = 8),
    axis.text.x   = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )

ggsave(file.path(DIR_GRAFICAS, "Grafica16_Heatmap_AUC.png"),
       plot = g16, width = 10, height = 6, dpi = 300)
cat("✅ Gráfica guardada:", file.path(DIR_GRAFICAS, "Grafica16_Heatmap_AUC.png"), "\n")

# --------------------------------------------------------------------------
# 9. RESUMEN FINAL
# --------------------------------------------------------------------------

cat("\n=== RESUMEN BLOQUE 5 ===\n")
cat(sprintf("Combinaciones evaluadas: %d\n", length(resultados_roc)))
cat(sprintf("Biomarcadores: %d | Outcomes: %d\n",
            length(biomarcadores), length(outcomes)))

# Top 5 combinaciones con mayor AUC
cat("\nTop 5 combinaciones con mayor AUC:\n")
top5 <- tabla_auc %>%
  arrange(desc(AUC)) %>%
  select(Biomarcador, Outcome, AUC, Interpretación) %>%
  head(9)
print(top5)

cat("\nArchivos generados:\n")
cat("  ", ruta_tabla9, "\n")
cat("  ", DIR_GRAFICAS, "/Grafica11 a Grafica15 — ROC comparativas por outcome\n", sep = "")
cat("  ", DIR_GRAFICAS, "/Grafica16_Heatmap_AUC.png — mapa de calor AUC\n", sep = "")
cat("\n=== BLOQUE 5 COMPLETADO ===\n")







tabla_heatmap <- tabla_auc %>%
  mutate(
    # A. Creamos el texto combinado para la celda: AUC arriba, Corte abajo
    etiqueta_celda = sprintf("AUC = %.3f\n(Optimal cut-off point: %.1f)", AUC, `Corte óptimo`),
    
    # B. Categorizamos el AUC según los criterios de tu tesis
    Interpretacion = case_when(
      AUC >= 0.90 ~ "Outstanding",
      AUC >= 0.80 & AUC < 0.90 ~ "Excellent",
      AUC >= 0.70 & AUC < 0.80 ~ "Acceptable",
      AUC >= 0.60 & AUC < 0.70 ~ "Unsatisfactory",
      AUC < 0.60 ~ "Without discrimination"
    ),
    
    # C. Fijamos el orden del factor para que la leyenda aparezca jerarquizada
    Interpretacion = factor(Interpretacion, 
                            levels = c("Outstanding", "Excellent", "Acceptable", "Unsatisfactory", "Without discrimination"))
  )

g_heatmap_nuevo <- ggplot(tabla_heatmap, aes(x = Outcome, y = Biomarcador)) +
  # Usamos Interpretacion en lugar del AUC continuo para el relleno
  geom_tile(aes(fill = Interpretacion), color = "white", linewidth = 1) +
  
  # Imprimimos la nueva etiqueta doble
  geom_text(aes(label = etiqueta_celda), size = 2, lineheight = 1.2, color = "grey30") +
  
  # Asignamos colores semánticos: verdes/azules para lo bueno, grises para el ruido
  scale_fill_manual(
    values = c(
      "Outstanding"      = "#d4edda", # Verde fuerte
      "Excellent"          = "#cce5ff", # Verde suave
      "Acceptable"          = "#fff3cd", # Naranja suave
      "Unsatisfactory"              = "#f8d7da", # Rojo coral
      "Without discrimination" = "#E0E0E0"  # Gris neutro (evita que el ojo se distraiga)
    ),# colors = c("#f8d7da", "#fff3cd", "#cce5ff", "#d4edda"),
    name = "Diagnosis capacity\n(AUC interpretation)"
  ) +
  
  labs(
  #  title    = "Capacidad discriminativa de biomarcadores por complicación",
  #  subtitle = "Área bajo la curva ROC y punto de corte óptimo (Índice de Youden)",
    x = "Complication or comorbidity",
    y = "Biomarker"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, face = "bold", color = "grey20"),
    axis.text.y = element_text(face = "bold", color = "grey20"),
    plot.title  = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid = element_blank(),
    legend.position = "right"
  )

print(g_heatmap_nuevo)

ggsave(
  filename = file.path(DIR_GRAFICAS, "Figure_3.svg"),
  plot     = g_heatmap_nuevo,
  width    = 240,
  height   = 130,
  units    = "mm",
  dpi = 600,
  device = svglite
)

