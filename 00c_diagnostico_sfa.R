# ==============================================================================
# ARCHIVO: 00c_diagnostico_sfa.R
# ACTIVIDAD GANTT: A1/A6 - Diagnóstico numérico previo del SFA en Base C
# PROPÓSITO:
#   - Evaluar colinealidad entre -ln(LE) y ln((1000-IMR)/LE) en Base C
#   - Verificar signos de coeficientes y asimetría de residuos (OLS)
#   - Probar convergencia de frontier::sfa() (ODF bi-producto vs 1 salida LE)
# ==============================================================================

paquetes <- c("tidyverse", "frontier", "plm", "moments")
faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(faltantes) > 0) install.packages(faltantes)

library(tidyverse)
library(frontier)
library(plm)
library(moments)

ruta_proyecto <- "C:/Users/tato_/OneDrive/Escritorio/Seminario/Proyecto_Multiverso_Salud"
if (dir.exists(ruta_proyecto)) setwd(ruta_proyecto)

base_C <- readRDS("Resultados/Base_C_Original.rds") %>%
  mutate(
    ln_HE       = log(HE),
    ln_HB       = log(HB),
    ln_PHY      = log(PHY),
    ln_NUR      = log(NUR),
    ln_y1       = log(LE),
    neg_ln_y1   = -log(LE),
    ln_y2_1000  = log(IMR_1000),
    ln_ratio    = log(IMR_1000 / LE)
  )

cat("==============================================================\n")
cat("DIAGNÓSTICO NUMÉRICO PREVIO DEL SFA EN BASE C REAL\n")
cat("==============================================================\n")
cat("Observaciones completas en Base C (2007-2017):", nrow(base_C), "\n")
cat("Países distintos en Base C                   :", n_distinct(base_C$Country), "\n\n")

# 1. Revisión de rangos logarítmicos y correlación (colinealidad advertida por el profesor)
cat("--- 1. VARIACIÓN LOGARÍTMICA Y CORRELACIÓN ---\n")
cat("Rango de ln(LE)          :", round(range(base_C$ln_y1), 4),
    " | Amplitud:", round(diff(range(base_C$ln_y1)), 4), "\n")
cat("Rango de ln(1000 - IMR)  :", round(range(base_C$ln_y2_1000), 4),
    " | Amplitud:", round(diff(range(base_C$ln_y2_1000)), 4), "\n")
cat("Correlación(-ln y1, ln(y2/y1)):",
    round(cor(base_C$neg_ln_y1, base_C$ln_ratio), 6), "\n\n")

# 2. Diagnóstico OLS previo: ODF bi-producto vs Producción 1 salida (LE)
ols_odf <- lm(neg_ln_y1 ~ ln_HE + ln_HB + ln_PHY + ln_NUR + ln_ratio, data = base_C)
ols_le  <- lm(ln_y1     ~ ln_HE + ln_HB + ln_PHY + ln_NUR, data = base_C)

cat("--- 2. REGRESIÓN OLS PREVIA: ODF BI-PRODUCTO (-ln y1) ---\n")
print(round(coef(summary(ols_odf)), 4))
cat("R2 ajustado ODF:", round(summary(ols_odf)$adj.r.squared, 4), "\n")
cat("Asimetría residuos OLS (ODF, esperada > 0 para +u):",
    round(skewness(residuals(ols_odf)), 4), "\n\n")

cat("--- 3. REGRESIÓN OLS PREVIA: FRONTERA 1 SALIDA (ln LE) ---\n")
print(round(coef(summary(ols_le)), 4))
cat("R2 ajustado LE :", round(summary(ols_le)$adj.r.squared, 4), "\n")
cat("Asimetría residuos OLS (LE, esperada < 0 para -u) :",
    round(skewness(residuals(ols_le)), 4), "\n\n")

# 3. Prueba de estimación SFA de panel (Battese & Coelli 1992)
pdata_C <- pdata.frame(base_C, index = c("iso3c", "Year"))

cat("--- 4. ESTIMACIÓN SFA PANEL (ODF BI-PRODUCTO, ineffDecrease = FALSE) ---\n")
sfa_odf <- tryCatch(
  sfa(neg_ln_y1 ~ ln_HE + ln_HB + ln_PHY + ln_NUR + ln_ratio,
      data = pdata_C, ineffDecrease = FALSE, timeEffect = TRUE),
  error = function(e) e
)
if (inherits(sfa_odf, "error")) {
  cat("ERROR en SFA ODF:", conditionMessage(sfa_odf), "\n")
} else {
  print(summary(sfa_odf))
}

cat("\n--- 5. ESTIMACIÓN SFA PANEL (1 SALIDA LE, ineffDecrease = TRUE) ---\n")
sfa_le <- tryCatch(
  sfa(ln_y1 ~ ln_HE + ln_HB + ln_PHY + ln_NUR,
      data = pdata_C, ineffDecrease = TRUE, timeEffect = TRUE),
  error = function(e) e
)
if (inherits(sfa_le, "error")) {
  cat("ERROR en SFA LE:", conditionMessage(sfa_le), "\n")
} else {
  print(summary(sfa_le))
}

cat("\n-> 00c_diagnostico_sfa.R completado.\n")