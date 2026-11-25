# ==============================================================================
# ARCHIVO: 00_descarga_api_banco_mundial.R
# PROPÓSITO: Descarga cruda desde la API oficial del Banco Mundial (2007-2022)
#            para los 23 países del estudio, resguardo sin modificar y
#            auditoría real de datos faltantes (NA).
# ==============================================================================

# 1. Instalar paquetes si faltan y cargar librerías
paquetes <- c("wbstats", "tidyverse", "writexl", "digest")
faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(faltantes) > 0) install.packages(faltantes)

library(wbstats)
library(tidyverse)
library(writexl)
library(digest)

# 2. Fijar carpeta del proyecto
ruta_proyecto <- "C:/Users/tato_/OneDrive/Escritorio/Seminario/Proyecto_Multiverso_Salud"
if (dir.exists(ruta_proyecto)) {
  setwd(ruta_proyecto)
} else {
  stop("No se encontró la carpeta del proyecto: ", ruta_proyecto)
}

# 3. Los 23 países de referencia (códigos ISO-3) y los 6 indicadores oficiales
paises_23 <- c(
  "ARG", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL",
  "CRI", "CUB", "DOM", "SLV", "GTM", "HND", "JAM", "MEX",
  "NIC", "PER", "LCA", "SUR", "TTO", "URY", "VEN"
)

indicadores <- c(
  HE  = "SH.XPD.CHEX.GD.ZS", # Gasto corriente en salud (% del PIB)
  HB  = "SH.MED.BEDS.ZS",     # Camas de hospital (por 1.000 hab.)
  PHY = "SH.MED.PHYS.ZS",     # Médicos (por 1.000 hab.)
  NUR = "SH.MED.NUMW.P3",     # Enfermeras y parteras (por 1.000 hab.)
  LE  = "SP.DYN.LE00.IN",     # Esperanza de vida al nacer (años)
  IMR = "SP.DYN.IMRT.IN"      # Mortalidad infantil (por 1.000 nacidos vivos)
)

# 4. Descarga cruda directa desde la API del Banco Mundial (2007-2022)
fecha_descarga <- Sys.Date()
cat("Consultando API del Banco Mundial (2007-2022)...\n")

wb_crudo <- wb_data(
  indicator  = indicadores,
  country    = paises_23,
  start_date = 2007,
  end_date   = 2022
)

# 5. Guardar la respuesta cruda SIN MODIFICAR (RDS y Excel)
archivo_rds  <- paste0("WorldBank_API_Crudo_2007_2022_", fecha_descarga, ".rds")
archivo_xlsx <- paste0("WorldBank_API_Crudo_2007_2022_", fecha_descarga, ".xlsx")

saveRDS(wb_crudo, archivo_rds)

# 6. Calcular matriz real de faltantes (NA) sobre la respuesta cruda
matriz_na <- wb_crudo %>%
  mutate(Year = as.integer(date)) %>%
  group_by(Year) %>%
  summarise(
    NA_HE     = sum(is.na(HE)),
    NA_HB     = sum(is.na(HB)),
    NA_PHY    = sum(is.na(PHY)),
    NA_NUR    = sum(is.na(NUR)),
    NA_LE     = sum(is.na(LE)),
    NA_IMR    = sum(is.na(IMR)),
    Completos = sum(complete.cases(HE, HB, PHY, NUR, LE, IMR)),
    .groups   = "drop"
  ) %>%
  arrange(Year)

# Exportar Excel con la respuesta cruda intacta en la Hoja 1 y la auditoría en la Hoja 2
write_xlsx(
  list(
    "Respuesta_API_Cruda" = wb_crudo,
    "Auditoria_NA_Anual"  = matriz_na
  ),
  path = archivo_xlsx
)

# 7. Mostrar en consola los datos de trazabilidad y la tabla real de NA
cat("\n==============================================================\n")
cat("TRAZABILIDAD DE DESCARGA - API BANCO MUNDIAL\n")
cat("==============================================================\n")
cat("Fecha de descarga :", as.character(fecha_descarga), "\n")
cat("Archivo RDS crudo :", archivo_rds, "\n")
cat("SHA-256 RDS       :", digest(file = archivo_rds, algo = "sha256"), "\n")
cat("Archivo XLSX      :", archivo_xlsx, "\n")
cat("SHA-256 XLSX      :", digest(file = archivo_xlsx, algo = "sha256"), "\n\n")

cat("MATRIZ REAL DE FALTANTES (NA) POR AÑO (23 PAÍSES, 2007-2022):\n")
print(as.data.frame(matriz_na), row.names = FALSE)