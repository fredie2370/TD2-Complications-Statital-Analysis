# =============================================================================
# app/app.R — Visualizador Shiny de resultados (T2D complications, Hospital cohort)
# =============================================================================
# Ejecutar desde la raíz del proyecto:   shiny::runApp("app")
# Datos: usa data/processed/df_clean_tesis.rds si existe (datos reales, local);
#        si no, usa los datos SINTÉTICOS de data/demo/ (la app lo indica con un aviso).
# La app muestra solo agregados/gráficas: NO tiene vista fila-por-fila de pacientes.
# =============================================================================
library(shiny); library(bslib); library(ggplot2); library(dplyr); library(DT)

root <- local({
  d <- normalizePath(getwd()); for (i in 1:3) { if (file.exists(file.path(d, "config.R"))) return(d); d <- dirname(d) }
  stop("Ejecuta shiny::runApp('app') desde la raíz del proyecto.")
})
source(file.path(root, "R", "load_data.R"), local = TRUE)
source(file.path(root, "R", "plots.R"), local = TRUE)
dat <- load_td2_data(root)
df  <- dat$data

outcome_choices <- function(v) setNames(v, unname(outcome_dict[v]))
lab_choices     <- setNames(numeric_vars, unname(lab_dict[numeric_vars]))

dl_png <- function(id_fun, name, w = 8, h = 5.5) downloadHandler(
  filename = function() paste0(name, ".png"),
  content  = function(file) ggsave(file, id_fun(), width = w, height = h, dpi = 300, bg = "white"))

ui <- page_navbar(
  title = "T2D complications explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2F5597"),
  header = if (dat$synthetic) div(class = "alert alert-warning m-2 mb-0",
    strong("Synthetic demo data. "), "These are randomly generated values, not patients; results have no clinical meaning. ",
    "Run 01_limpieza.R on the real dataset locally to use the app with real data.") else NULL,

  nav_panel("Overview",
    layout_columns(col_widths = c(3, 3, 3, 3),
      value_box("Patients", textOutput("n_pat"), theme = "primary"),
      value_box("Median age (IQR)", textOutput("age_med"), theme = "secondary"),
      value_box("Female", textOutput("pct_f"), theme = "info"),
      value_box("≥1 complication", textOutput("pct_comp"), theme = "danger")),
    card(card_header("Prevalence of complications and comorbidities"), plotOutput("p_prev", height = 420),
         downloadButton("dl_prev", "PNG")),
    card(card_header("Table"), DTOutput("t_prev"))),

  nav_panel("Population",
    layout_columns(col_widths = c(6, 6),
      card(plotOutput("p_pyr"), downloadButton("dl_pyr", "PNG")),
      card(plotOutput("p_bmi"), downloadButton("dl_bmi", "PNG"))),
    card(plotOutput("p_ckd", height = 380), downloadButton("dl_ckd", "PNG"))),

  nav_panel("Complications",
    layout_columns(col_widths = c(6, 6),
      card(plotOutput("p_cs"), downloadButton("dl_cs", "PNG")),
      card(plotOutput("p_burden"), downloadButton("dl_burden", "PNG")))),

  nav_panel("Labs & correlations",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("lab_var", "Laboratory / clinical variable", lab_choices, "Glucosa"),
        selectInput("lab_out", "Compare by", outcome_choices(names(outcome_dict)), "ERC"),
        hr(),
        selectInput("sx", "Scatter X", lab_choices, "TFG_CKD_EPI"),
        selectInput("sy", "Scatter Y", lab_choices, "Creatinina"),
        selectInput("sc", "Colour by", c("none", outcome_choices(names(outcome_dict))), "ERC")),
      navset_card_tab(
        nav_panel("Distribution by outcome", plotOutput("p_lab", height = 450), downloadButton("dl_lab", "PNG")),
        nav_panel("Scatter", plotOutput("p_sc", height = 450), downloadButton("dl_sc", "PNG")),
        nav_panel("Correlation matrix", plotOutput("p_cor", height = 550), downloadButton("dl_cor", "PNG"))))),

  nav_panel("Biomarker ROC",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("roc_out", "Complication", outcome_choices(roc_outcomes), "ERC"),
        checkboxGroupInput("roc_bm", "Biomarkers", setNames(roc_biomarkers, unname(lab_dict[roc_biomarkers])), roc_biomarkers),
        helpText("AUC with DeLong 95% CI; cut-off by Youden index. Direction chosen automatically (pROC).")),
      navset_card_tab(
        nav_panel("ROC curves", plotOutput("p_roc", height = 500), downloadButton("dl_roc", "PNG")),
        nav_panel("AUC heatmap", plotOutput("p_auc", height = 500), downloadButton("dl_auc", "PNG")),
        nav_panel("Table", DTOutput("t_roc"))))),

  nav_panel("Regression",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("reg_out", "Outcome", outcome_choices(reg_outcomes), "ERC"),
        checkboxGroupInput("reg_pred", "Candidate predictors", setNames(reg_predictors, unname(term_labels[reg_predictors])), reg_predictors),
        helpText("Binary logistic regression, backward selection by AIC (same as 04_regresion.R). Requires ≥10 events.")),
      card(plotOutput("p_forest", height = 400), downloadButton("dl_forest", "PNG")),
      card(card_header("Model diagnostics"), verbatimTextOutput("reg_diag")))),

  nav_panel("About",
    card(markdown(sprintf("
**Data source:** `%s`  \n**Mode:** %s  \n
Figures use the same definitions as scripts `02`–`05`: Clopper–Pearson CIs, Spearman correlation (Holm), DeLong AUC with Youden cut-off,
backward-AIC logistic regression (VIF, Hosmer–Lemeshow, Nagelkerke R²). Source: https://github.com/fredie2370/TD2-Complications-Statital-Analysis",
      basename(dat$path), if (dat$synthetic) "synthetic demo data" else "real data (local)"))))
)

server <- function(input, output, session) {
  output$n_pat    <- renderText(format(nrow(df), big.mark = ","))
  output$age_med  <- renderText({ q <- quantile(df$Edad, c(.25, .5, .75), na.rm = TRUE); sprintf("%.0f (%.0f–%.0f)", q[2], q[1], q[3]) })
  output$pct_f    <- renderText(sprintf("%.1f%%", 100 * mean(df$Sexo == "Assigned female at birth", na.rm = TRUE)))
  output$pct_comp <- renderText(sprintf("%.1f%%", 100 * mean(df$Tiene_complicacion == "Yes", na.rm = TRUE)))

  f_prev <- reactive(plot_prevalence(df));        output$p_prev <- renderPlot(f_prev());  output$dl_prev <- dl_png(f_prev, "prevalence")
  output$t_prev <- renderDT({ t <- prevalence_table(df) |> transmute(Condition = label, n, N, `%` = round(pct, 1), `95% CI` = sprintf("%.1f–%.1f", lo, hi))
    datatable(t, rownames = FALSE, options = list(dom = "t", pageLength = 10)) })
  f_pyr <- reactive(plot_age_pyramid(df));        output$p_pyr <- renderPlot(f_pyr());    output$dl_pyr <- dl_png(f_pyr, "age_sex_pyramid")
  f_bmi <- reactive(plot_bmi_by_sex(df));         output$p_bmi <- renderPlot(f_bmi());    output$dl_bmi <- dl_png(f_bmi, "bmi_by_sex")
  f_ckd <- reactive(plot_ckd_stages(df));         output$p_ckd <- renderPlot(f_ckd());    output$dl_ckd <- dl_png(f_ckd, "ckd_stages")
  f_cs  <- reactive(plot_complications_by_sex(df)); output$p_cs <- renderPlot(f_cs());    output$dl_cs <- dl_png(f_cs, "complications_by_sex")
  f_bur <- reactive(plot_burden(df));             output$p_burden <- renderPlot(f_bur()); output$dl_burden <- dl_png(f_bur, "burden")

  f_lab <- reactive(plot_lab_by_outcome(df, input$lab_var, input$lab_out)); output$p_lab <- renderPlot(f_lab()); output$dl_lab <- dl_png(f_lab, "lab_by_outcome")
  f_sc  <- reactive(plot_scatter(df, input$sx, input$sy, input$sc));          output$p_sc  <- renderPlot(f_sc());  output$dl_sc  <- dl_png(f_sc, "scatter")
  f_cor <- reactive(plot_corr_heatmap(df));                                    output$p_cor <- renderPlot(f_cor()); output$dl_cor <- dl_png(f_cor, "correlation", 9, 8)

  bm <- reactive({ req(length(input$roc_bm) > 0); input$roc_bm })
  f_roc <- reactive(plot_roc_multi(df, input$roc_out, bm()));                 output$p_roc <- renderPlot(f_roc()); output$dl_roc <- dl_png(f_roc, "roc_curves")
  f_auc <- reactive(plot_auc_heatmap(df, bm()));                               output$p_auc <- renderPlot(f_auc()); output$dl_auc <- dl_png(f_auc, "auc_heatmap")
  output$t_roc <- renderDT({
    t <- roc_table(df, bm(), roc_outcomes) |> transmute(Biomarker = unname(lab_dict[biomarker]), Complication = unname(outcome_dict[outcome]),
      AUC = round(auc, 3), `95% CI` = sprintf("%.3f–%.3f", lo, hi), Cutoff = round(cutoff, 2), Sensitivity = round(sens, 3), Specificity = round(spec, 3), Interpretation = interpretation)
    datatable(t, rownames = FALSE, filter = "top", options = list(pageLength = 15)) })

  fit <- reactive({ req(length(input$reg_pred) > 0); fit_logistic(df, input$reg_out, input$reg_pred) })
  f_for <- reactive(plot_forest(fit(), paste("Predictors of", tolower(unname(outcome_dict[input$reg_out])))))
  output$p_forest <- renderPlot(f_for()); output$dl_forest <- dl_png(f_for, "forest_plot", 8, 4.5)
  output$reg_diag <- renderText({ f <- fit(); if (!f$ok) return(f$msg)
    paste0(sprintf("n = %d | events = %d\nAUC = %.3f (%.3f–%.3f)\nNagelkerke R2 = %.3f\nHosmer–Lemeshow p = %.3f\n", f$n, f$events, f$auc, f$auc_ci[1], f$auc_ci[2], f$nagelkerke, f$hl_p),
           if (!is.null(f$vif)) paste("VIF:", paste(names(f$vif), round(f$vif, 2), sep = " = ", collapse = " | ")) else "VIF: n/a (single predictor)") })
}

shinyApp(ui, server)
