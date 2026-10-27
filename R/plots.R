# =============================================================================
# R/plots.R — Funciones de visualización reutilizables (app Shiny y reporte Rmd)
# =============================================================================
# Cada función recibe el data.frame limpio (salida de 01_limpieza.R) y devuelve
# un objeto ggplot o un data.frame. Mismas definiciones que los scripts 02–05
# (Clopper-Pearson, DeLong, Youden, regresión backward-AIC), pero sin escribir archivos.
# Requiere: ggplot2, dplyr, tidyr, pROC, car, ResourceSelection (los dos últimos solo para regresión).
# =============================================================================

suppressPackageStartupMessages({ library(ggplot2); library(dplyr); library(tidyr) })

col_f <- "#C0504D"; col_m <- "#2F5597"; col_pos <- "#B22222"; col_neg <- "#4D7EA8"
sex_cols <- c("Assigned female at birth" = col_f, "Assigned male at birth" = col_m)

theme_td2 <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
}

# --- Prevalencias con IC 95% Clopper-Pearson --------------------------------
prevalence_table <- function(df, vars = names(outcome_dict), conf = 0.95) {
  vars <- intersect(vars, names(df))
  bind_rows(lapply(vars, function(v) {
    x <- df[[v]]; n_obs <- sum(!is.na(x)); n_yes <- sum(x == "Yes", na.rm = TRUE)
    ci <- if (n_obs > 0) binom.test(n_yes, n_obs, conf.level = conf)$conf.int else c(NA, NA)
    data.frame(variable = v, label = unname(outcome_dict[v]), n = n_yes, N = n_obs,
               pct = 100 * n_yes / n_obs, lo = 100 * ci[1], hi = 100 * ci[2])
  }))
}

plot_prevalence <- function(df, vars = names(outcome_dict)) {
  p <- prevalence_table(df, vars)
  ggplot(p, aes(x = pct, y = reorder(label, pct))) +
    geom_col(fill = col_m, width = 0.65, alpha = 0.85) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.25, orientation = "y") +
    geom_text(aes(x = hi, label = sprintf("%.1f%% (n=%d)", pct, n)), hjust = -0.1, size = 3.6) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
    labs(x = "Prevalence (%) with 95% CI (Clopper-Pearson)", y = NULL,
         title = "Prevalence of complications and comorbidities") + theme_td2()
}

# --- Población ----------------------------------------------------------------
plot_age_pyramid <- function(df, binwidth = 5) {
  d <- df %>% filter(!is.na(Edad), !is.na(Sexo)) %>%
    mutate(grupo = cut(Edad, breaks = seq(floor(min(Edad) / binwidth) * binwidth,
                                          ceiling(max(Edad) / binwidth) * binwidth + binwidth, binwidth),
                       right = FALSE)) %>%
    count(grupo, Sexo) %>%
    mutate(n = ifelse(Sexo == "Assigned female at birth", -n, n))
  lim <- max(abs(d$n))
  ggplot(d, aes(x = n, y = grupo, fill = Sexo)) +
    geom_col(width = 0.9) +
    scale_x_continuous(labels = abs, limits = c(-lim, lim) * 1.05) +
    scale_fill_manual(values = sex_cols, name = NULL) +
    labs(x = "Patients (n)", y = "Age group (years)", title = "Age and sex distribution") + theme_td2()
}

plot_bmi_by_sex <- function(df) {
  d <- df %>% filter(!is.na(IMC_cat), !is.na(Sexo)) %>% count(IMC_cat, Sexo)
  ggplot(d, aes(IMC_cat, n, fill = Sexo)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    geom_text(aes(label = n), position = position_dodge(0.8), vjust = -0.3, size = 3.5) +
    scale_fill_manual(values = sex_cols, name = NULL) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, 12)) +
    labs(x = "BMI category", y = "Patients (n)", title = "BMI category by sex") + theme_td2()
}

plot_ckd_stages <- function(df) {
  d <- df %>% filter(!is.na(Estadio_ERC)) %>% count(Estadio_ERC) %>% mutate(pct = 100 * n / sum(n))
  ggplot(d, aes(Estadio_ERC, n)) +
    geom_col(fill = col_neg, width = 0.7) +
    geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)), vjust = -0.3, size = 3.6) +
    labs(x = "KDIGO stage (CKD-EPI 2009)", y = "Patients (n)", title = "eGFR stage distribution") + theme_td2()
}

# --- Complicaciones ------------------------------------------------------------
plot_complications_by_sex <- function(df, vars = c("ERC", "Dialisis", "Retinopatia", "IC", "Amputacion", "HTA", "EPOC")) {
  d <- df %>% filter(!is.na(Sexo)) %>%
    select(Sexo, all_of(vars)) %>%
    pivot_longer(-Sexo, names_to = "var", values_to = "val") %>%
    group_by(Sexo, var) %>% summarise(pct = 100 * mean(val == "Yes", na.rm = TRUE), n = sum(val == "Yes", na.rm = TRUE), .groups = "drop") %>%
    mutate(label = unname(outcome_dict[var]))
  ggplot(d, aes(pct, reorder(label, pct), fill = Sexo)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    scale_fill_manual(values = sex_cols, name = NULL) +
    labs(x = "% within sex", y = NULL, title = "Complications and comorbidities by sex") + theme_td2()
}

plot_burden <- function(df) {
  d <- df %>% count(N_Complicaciones) %>% mutate(pct = 100 * n / sum(n))
  ggplot(d, aes(factor(N_Complicaciones), n)) +
    geom_col(fill = col_pos, width = 0.65, alpha = 0.85) +
    geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)), vjust = -0.3, size = 3.6) +
    labs(x = "Number of complications per patient (CKD, dialysis, retinopathy, HF, amputation)",
         y = "Patients (n)", title = "Complication burden") + theme_td2()
}

# --- Laboratorio -----------------------------------------------------------------
plot_lab_by_outcome <- function(df, var, outcome) {
  d <- df %>% filter(!is.na(.data[[var]]), !is.na(.data[[outcome]]))
  lab <- ifelse(var %in% names(lab_dict), lab_dict[var], var)
  p <- tryCatch(wilcox.test(d[[var]] ~ d[[outcome]])$p.value, error = function(e) NA_real_)
  ggplot(d, aes(.data[[outcome]], .data[[var]], fill = .data[[outcome]])) +
    geom_violin(alpha = 0.35, colour = NA, trim = TRUE) +
    geom_boxplot(width = 0.18, outlier.shape = NA, fill = "white") +
    geom_jitter(width = 0.08, alpha = 0.35, size = 1.2) +
    scale_fill_manual(values = c(No = col_neg, Yes = col_pos), guide = "none") +
    labs(x = unname(outcome_dict[outcome]), y = lab,
         title = paste(lab, "by", tolower(unname(outcome_dict[outcome]))),
         subtitle = if (is.na(p)) NULL else sprintf("Mann-Whitney p = %s", format.pval(p, digits = 3, eps = 0.001))) +
    theme_td2()
}

corr_matrix <- function(df, vars = numeric_vars, method = "spearman") {
  vars <- intersect(vars, names(df))
  m <- df[vars]
  r <- cor(m, use = "pairwise.complete.obs", method = method)
  p <- outer(vars, vars, Vectorize(function(a, b)
    if (a == b) NA_real_ else suppressWarnings(cor.test(m[[a]], m[[b]], method = method, exact = FALSE)$p.value)))
  dimnames(p) <- dimnames(r)
  list(r = r, p = p)
}

plot_corr_heatmap <- function(df, vars = numeric_vars, method = "spearman") {
  cm <- corr_matrix(df, vars, method)
  d <- as.data.frame(as.table(cm$r)); names(d) <- c("a", "b", "r")
  d$p <- as.vector(cm$p)
  d$p_holm <- NA_real_
  up <- upper.tri(cm$p)
  d$p_holm[as.vector(up)] <- p.adjust(as.vector(cm$p)[as.vector(up)], "holm")
  d$star <- ifelse(!is.na(d$p_holm) & d$p_holm < 0.05, "*", "")
  ggplot(d, aes(a, b, fill = r)) + geom_tile(colour = "white") +
    geom_text(aes(label = paste0(sprintf("%.2f", r), star)), size = 2.9) +
    scale_fill_gradient2(low = col_neg, mid = "white", high = col_pos, limits = c(-1, 1), name = expression(rho)) +
    labs(x = NULL, y = NULL, title = paste(tools::toTitleCase(method), "correlation"),
         caption = "* Holm-adjusted p < 0.05 (upper triangle)") +
    theme_td2() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_scatter <- function(df, x, y, colour = NULL) {
  d <- df %>% filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
  rho <- suppressWarnings(cor.test(d[[x]], d[[y]], method = "spearman", exact = FALSE))
  g <- if (is.null(colour) || colour == "none") ggplot(d, aes(.data[[x]], .data[[y]])) + geom_point(alpha = 0.6, colour = col_m)
       else ggplot(d, aes(.data[[x]], .data[[y]], colour = .data[[colour]])) + geom_point(alpha = 0.7) +
              scale_colour_manual(values = c(No = col_neg, Yes = col_pos), na.value = "grey60", name = unname(outcome_dict[colour]))
  g + geom_smooth(aes(group = 1), method = "loess", formula = y ~ x, se = TRUE, colour = "black", linewidth = 0.7) +
    labs(x = unname(ifelse(x %in% names(lab_dict), lab_dict[x], x)), y = unname(ifelse(y %in% names(lab_dict), lab_dict[y], y)),
         subtitle = sprintf("Spearman rho = %.2f, p = %s", rho$estimate, format.pval(rho$p.value, digits = 3, eps = 0.001))) +
    theme_td2()
}

# --- ROC ---------------------------------------------------------------------------
roc_one <- function(df, biomarker, outcome) {
  d <- df[!is.na(df[[biomarker]]) & !is.na(df[[outcome]]), ]
  y <- as.integer(d[[outcome]] == "Yes")
  if (sum(y) < 5 || sum(1 - y) < 5) return(NULL)
  r <- pROC::roc(y, d[[biomarker]], quiet = TRUE, direction = "auto")
  ci <- as.numeric(pROC::ci.auc(r, method = "delong"))
  yc <- pROC::coords(r, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"), transpose = FALSE)[1, ]
  list(roc = r, row = data.frame(biomarker = biomarker, outcome = outcome, auc = ci[2], lo = ci[1], hi = ci[3],
        direction = r$direction, cutoff = yc$threshold, sens = yc$sensitivity, spec = yc$specificity,
        n = length(y), events = sum(y)))
}

roc_table <- function(df, biomarkers = roc_biomarkers, outcomes = roc_outcomes) {
  out <- list()
  for (o in outcomes) for (b in biomarkers) { r <- roc_one(df, b, o); if (!is.null(r)) out[[length(out) + 1]] <- r$row }
  res <- bind_rows(out)
  if (nrow(res)) res$interpretation <- cut(res$auc, c(0, .6, .7, .8, .9, 1.01), right = FALSE,
                                           labels = c("Poor", "Fair", "Acceptable", "Excellent", "Outstanding"))
  res
}

plot_roc_multi <- function(df, outcome, biomarkers = roc_biomarkers) {
  curves <- lapply(biomarkers, function(b) {
    r <- roc_one(df, b, outcome); if (is.null(r)) return(NULL)
    data.frame(biomarker = sprintf("%s (AUC %.2f)", ifelse(b %in% names(lab_dict), lab_dict[b], b), r$row$auc),
               spec = r$roc$specificities, sens = r$roc$sensitivities)
  })
  d <- bind_rows(curves)
  if (!nrow(d)) return(ggplot() + theme_void() + labs(title = "Not enough events for ROC analysis"))
  ggplot(d, aes(1 - spec, sens, colour = biomarker)) + geom_path(linewidth = 0.9) +
    geom_abline(linetype = 2, colour = "grey50") + coord_equal() +
    labs(x = "1 - Specificity", y = "Sensitivity", colour = NULL,
         title = paste("ROC curves:", unname(outcome_dict[outcome]))) + theme_td2() +
    guides(colour = guide_legend(ncol = 2))
}

plot_auc_heatmap <- function(df, biomarkers = roc_biomarkers, outcomes = roc_outcomes) {
  t <- roc_table(df, biomarkers, outcomes)
  if (!nrow(t)) return(ggplot() + theme_void())
  t$bm <- ifelse(t$biomarker %in% names(lab_dict), lab_dict[t$biomarker], t$biomarker)
  t$out <- unname(outcome_dict[t$outcome])
  ggplot(t, aes(out, bm, fill = auc)) + geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.2f", auc)), size = 3.6) +
    scale_fill_gradient2(low = "white", mid = "#FDE0DD", high = col_pos, midpoint = 0.7, limits = c(0.5, 1), name = "AUC") +
    labs(x = NULL, y = NULL, title = "AUC of individual biomarkers") + theme_td2()
}

# --- Regresión logística (misma lógica que 04_regresion.R) ----------------------
fit_logistic <- function(df, outcome, predictors = reg_predictors) {
  d <- df %>% mutate(outcome_bin = as.integer(.data[[outcome]] == "Yes"),
                     HTA_bin = as.integer(HTA == "Yes"),
                     Sexo_bin = as.integer(Sexo == "Assigned male at birth")) %>%
    filter(!is.na(outcome_bin))
  predictors <- setdiff(predictors, paste0(outcome, "_bin"))
  dc <- d %>% select(outcome_bin, all_of(predictors)) %>% na.omit()
  predictors <- predictors[sapply(predictors, function(v) length(unique(dc[[v]])) > 1)]
  ev <- sum(dc$outcome_bin)
  if (ev < 10) return(list(ok = FALSE, msg = sprintf("Only %d events (<10): logistic regression not recommended.", ev)))
  null <- glm(outcome_bin ~ 1, dc, family = binomial())
  full <- glm(reformulate(predictors, "outcome_bin"), dc, family = binomial())
  fin  <- step(full, direction = "backward", trace = 0)
  vif_v <- tryCatch(car::vif(fin), error = function(e) NULL)
  hl <- tryCatch(ResourceSelection::hoslem.test(dc$outcome_bin, fitted(fin), g = 10)$p.value, error = function(e) NA_real_)
  n <- nrow(dc); ln <- as.numeric(logLik(null)); lf <- as.numeric(logLik(fin))
  r2 <- (1 - exp((2 / n) * (ln - lf))) / (1 - exp((2 / n) * ln))
  r <- pROC::roc(dc$outcome_bin, fitted(fin), quiet = TRUE)
  ci <- as.numeric(pROC::ci.auc(r, method = "delong"))
  co <- broom::tidy(fin, exponentiate = TRUE, conf.int = TRUE) %>% filter(term != "(Intercept)")
  list(ok = TRUE, model = fin, coefs = co, vif = vif_v, hl_p = hl, nagelkerke = r2, auc = ci[2],
       auc_ci = c(ci[1], ci[3]), n = n, events = ev, roc = r)
}

term_labels <- c(Edad = "Age (per year)", Sexo_bin = "Male sex", TAS = "Systolic BP (per mmHg)", HTA_bin = "Hypertension",
                 Glucosa = "Glucose (per mg/dL)", Hb = "Haemoglobin (per g/dL)", Acido_Urico = "Uric acid (per mg/dL)", TyG = "TyG index (per unit)")

plot_forest <- function(fit, title = "") {
  if (!isTRUE(fit$ok)) return(ggplot() + theme_void() + labs(title = fit$msg))
  d <- fit$coefs; d$lab <- ifelse(d$term %in% names(term_labels), term_labels[d$term], d$term)
  ggplot(d, aes(estimate, reorder(lab, estimate))) +
    geom_vline(xintercept = 1, linetype = 2, colour = "grey50") +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), colour = col_m) +
    scale_x_log10() +
    labs(x = "Odds ratio (95% CI, log scale)", y = NULL, title = title,
         subtitle = sprintf("n = %d, events = %d | AUC %.2f (%.2f-%.2f) | Nagelkerke R² %.2f | HL p = %s",
                            fit$n, fit$events, fit$auc, fit$auc_ci[1], fit$auc_ci[2], fit$nagelkerke,
                            ifelse(is.na(fit$hl_p), "NA", sprintf("%.2f", fit$hl_p)))) + theme_td2()
}
