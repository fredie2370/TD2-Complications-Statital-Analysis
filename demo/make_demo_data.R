# =============================================================================
# demo/make_demo_data.R — Genera datos SINTÉTICOS para probar el pipeline y la app
# =============================================================================
# NO son datos de pacientes. Son números aleatorios con relaciones plausibles
# (edad, glucosa, creatinina -> complicaciones) con semilla fija. Sirven para:
#   - probar 01_limpieza.R y la app Shiny sin acceder a la base clínica,
# Los resultados NO tienen valor clínico ni científico.
#
# Uso (desde la raíz del proyecto):
#   Rscript demo/make_demo_data.R
# Salida:
#   data/demo/Datos_sintetico.xlsx   (mismo formato que Datos.xlsx; hoja DIABETICOS)
#   data/demo/df_clean_demo.rds      (resultado de correr 01_limpieza.R sobre ese Excel)
# =============================================================================

suppressPackageStartupMessages(library(openxlsx))
set.seed(2024)
n <- 228

sexo   <- sample(c("F", "M"), n, replace = TRUE, prob = c(0.56, 0.44))
edad   <- round(pmin(pmax(rnorm(n, 62, 12), 25), 95))
talla  <- round(ifelse(sexo == "F", rnorm(n, 1.58, 0.06), rnorm(n, 1.69, 0.07)), 2)
peso   <- round(pmax(rnorm(n, 76, 14), 40), 1)
tas    <- round(rnorm(n, 128 + 0.3 * (edad - 60), 18))
tad    <- round(rnorm(n, 76, 10))
glu    <- round(pmax(rlnorm(n, log(165), 0.40), 70))
tg     <- round(pmax(rlnorm(n, log(190), 0.45), 50))
colest <- round(rnorm(n, 185, 40))
hb_lat <- rnorm(n)
# latent renal risk
z <- 0.04 * (edad - 62) + 0.004 * (glu - 165) + 0.02 * (tas - 128) + rnorm(n, 0, 0.8)
cr <- round(pmax(exp(-0.15 + 0.55 * z + rnorm(n, 0, 0.25)), 0.4), 2)
hb <- round(pmax(13.5 - 1.4 * z + 0.6 * hb_lat, 6), 1)
bun <- round(pmax(14 + 12 * pmax(cr - 0.8, 0) + rnorm(n, 0, 4), 5), 1)
urea <- round(bun * 2.14, 1)
ua <- round(pmax(5.2 + 1.1 * z + rnorm(n, 0, 1.1), 2), 1)

sn <- function(p) ifelse(runif(n) < p, "S", "N")
lg <- function(x) plogis(x)
erc  <- sn(lg(-1.6 + 1.2 * z))
hta  <- sn(lg(0.2 + 0.03 * (edad - 62) + 0.5 * z))
dial <- ifelse(erc == "S", sn(0.10), "N")
ret  <- sn(lg(-2.4 + 0.5 * z + 0.004 * (glu - 165)))
ic   <- sn(lg(-3.0 + 0.04 * (edad - 62) + 0.4 * z))
epoc <- sn(0.06)
amp  <- sn(lg(-3.6 + 0.004 * (glu - 165) + 0.3 * z))

# Excel con la misma estructura que la base real: fila 1 = encabezado agrupador,
# fila 2 = nombres de columnas (SKIP_FILAS = 1).
d <- data.frame(
  Nombre = sprintf("SINTETICO-%03d", seq_len(n)),
  Sexo = sexo, Edad = edad, `Peso (Kg)` = peso, `Talla (m)` = talla,
  TA = paste0(tas, "-", tad), FC = round(rnorm(n, 78, 9)), FR = round(rnorm(n, 18, 2)),
  `HTDS-HA` = hta, `Retinopatía diabética` = ret, `insuficencia cardiaca` = ic,
  ERC = erc, Dialisis = dial, EPOC = epoc, `Amputación` = amp,
  Glucosa = glu, `Anemia (HB)` = hb, colesi = colest, Tg = tg,
  Bun = bun, Creatiniina = cr, urea = urea, `acido urico` = ua,
  check.names = FALSE, stringsAsFactors = FALSE
)

dir.create("data/demo", recursive = TRUE, showWarnings = FALSE)
xlsx <- "data/demo/Datos_sintetico.xlsx"
wb <- createWorkbook()
addWorksheet(wb, "DIABETICOS")
writeData(wb, "DIABETICOS", data.frame(x = "DATOS SINTÉTICOS (no son pacientes reales)"),
          startRow = 1, colNames = FALSE)
writeData(wb, "DIABETICOS", d, startRow = 2)
saveWorkbook(wb, xlsx, overwrite = TRUE)
message("Excel sintético: ", xlsx)

# Correr el 01_limpieza.R REAL sobre el Excel sintético, escribiendo en carpetas temporales
tmp <- tempfile("demo_out_"); dir.create(tmp)
Sys.setenv(TD2_DATA_FILE = xlsx,
           TD2_DIR_TABLES  = file.path(tmp, "tables"),
           TD2_DIR_FIGURES = file.path(tmp, "figures"),
           TD2_DIR_MODELS  = file.path(tmp, "models"),
           TD2_DIR_DATA    = file.path(tmp, "processed"))
local({ source("01_limpieza.R", local = FALSE) })
file.copy(file.path(tmp, "processed", "df_clean_tesis.rds"),
          "data/demo/df_clean_demo.rds", overwrite = TRUE)
message("RDS sintético: data/demo/df_clean_demo.rds")
