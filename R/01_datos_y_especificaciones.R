# ==============================================================================
# MÓDULO: R/01_datos_y_especificaciones.R
# PROPÓSITO: Funciones puras para verificar SHA-256, construir Bases A, B y C,
#            y generar la tabla parametrizada de especificaciones del multiverso.
# ==============================================================================

preparar_bases_multiverso <- function(archivo_base_A, archivo_wb_rds) {
  sha_A  <- digest::digest(file = archivo_base_A, algo = "sha256")
  sha_WB <- digest::digest(file = archivo_wb_rds, algo = "sha256")
  
  mapa_paises <- tibble::tibble(
    iso3c = c(
      "ARG", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL",
      "CRI", "CUB", "DOM", "SLV", "GTM", "HND", "JAM", "MEX",
      "NIC", "PER", "LCA", "SUR", "TTO", "URY", "VEN"
    ),
    Country = c(
      "Argentina", "Bahamas, The", "Barbados", "Belize", "Bolivia",
      "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba",
      "Dominican Republic", "El Salvador", "Guatemala", "Honduras",
      "Jamaica", "Mexico", "Nicaragua", "Peru", "St. Lucia",
      "Suriname", "Trinidad and Tobago", "Uruguay", "Venezuela, RB"
    )
  )
  
  # --- BASE A (Repositorio Peres 2024) ---
  base_A_raw <- readxl::read_excel(archivo_base_A, sheet = 1) %>%
    dplyr::select(Country, Year, HE, HB, PHY, NUR, LE, IMR) %>%
    dplyr::mutate(Year = as.integer(Year)) %>%
    dplyr::filter(Year %in% 2007:2017) %>%
    dplyr::left_join(mapa_paises, by = "Country") %>%
    dplyr::select(iso3c, Country, Year, HE, HB, PHY, NUR, LE, IMR) %>%
    dplyr::arrange(Year, Country)
  
  base_A <- base_A_raw %>%
    dplyr::filter(stats::complete.cases(HE, HB, PHY, NUR, LE, IMR))
  
  stopifnot(
    "Hay países no mapeados en Base A" = sum(is.na(base_A$iso3c)) == 0,
    "El año 2017 en Base A no tiene 23 países completos" = sum(base_A$Year == 2017) == 23
  )
  
  ceros_base_A <- base_A %>%
    tidyr::pivot_longer(cols = c(HE, HB, PHY, NUR, LE, IMR),
                        names_to = "Variable", values_to = "Valor") %>%
    dplyr::filter(Valor == 0) %>%
    dplyr::arrange(Year, Variable, Country)
  
  # --- BASE C (API Cruda Banco Mundial, escala original sin imputar) ---
  wb_crudo <- readRDS(archivo_wb_rds) %>%
    dplyr::mutate(Year = as.integer(date)) %>%
    dplyr::filter(Year %in% 2007:2017) %>%
    dplyr::inner_join(mapa_paises, by = "iso3c") %>%
    dplyr::select(iso3c, Country, Year, HE, HB, PHY, NUR, LE, IMR_raw = IMR) %>%
    dplyr::arrange(Year, Country)
  
  auditoria_na_wb <- wb_crudo %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
      Paises_Totales   = dplyr::n(),
      NA_HE            = sum(is.na(HE)),
      NA_HB            = sum(is.na(HB)),
      NA_PHY           = sum(is.na(PHY)),
      NA_NUR           = sum(is.na(NUR)),
      NA_LE            = sum(is.na(LE)),
      NA_IMR           = sum(is.na(IMR_raw)),
      Paises_Completos = sum(stats::complete.cases(HE, HB, PHY, NUR, LE, IMR_raw)),
      .groups = "drop"
    )
  
  base_C <- wb_crudo %>%
    dplyr::filter(stats::complete.cases(HE, HB, PHY, NUR, LE, IMR_raw)) %>%
    dplyr::mutate(
      IMR_1000 = 1000 - IMR_raw,
      IMR_max  = max(IMR_raw, na.rm = TRUE) - IMR_raw,
      IMR      = IMR_1000
    )
  
  # --- BASE B (Base C re-normalizada con Min-Max global) ---
  minmax_col <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) rep(0, length(x)) else (x - rng[1]) / diff(rng)
  }
  
  base_B <- base_C %>%
    dplyr::mutate(dplyr::across(c(HE, HB, PHY, NUR, LE, IMR_1000, IMR_max), minmax_col)) %>%
    dplyr::mutate(IMR = IMR_1000)
  
  ceros_base_B <- base_B %>%
    tidyr::pivot_longer(cols = c(HE, HB, PHY, NUR, LE, IMR_1000),
                        names_to = "Variable", values_to = "Valor") %>%
    dplyr::filter(Valor == 0) %>%
    dplyr::arrange(Year, Variable, Country)
  
  list(
    sha_A           = sha_A,
    sha_WB          = sha_WB,
    base_A          = base_A,
    base_B          = base_B,
    base_C          = base_C,
    auditoria_na_wb = auditoria_na_wb,
    ceros_base_A    = ceros_base_A,
    ceros_base_B    = ceros_base_B
  )
}

construir_tabla_especificaciones <- function(datos_prep) {
  anios_base_A <- unique(datos_prep$base_A$Year)
  anios_cero_A <- unique(datos_prep$ceros_base_A$Year)
  anios_cero_B <- unique(datos_prep$ceros_base_B$Year)
  
  bases <- c("Base_A", "Base_B", "Base_C")
  trans <- c("IMR_1000", "IMR_max")
  anios <- 2007:2017
  
  esp_dea <- tidyr::expand_grid(
    Familia = "DEA_Radial", Orientacion = c("io", "oo"), Regimen = c("crs", "vrs"),
    Parametro = "NA", Base = bases, Transformacion = trans, Year = anios
  )
  esp_sbm <- tidyr::expand_grid(
    Familia = "SBM", Orientacion = "no_radial", Regimen = c("crs", "vrs"),
    Parametro = "NA", Base = bases, Transformacion = trans, Year = anios
  )
  esp_fdh <- tidyr::expand_grid(
    Familia = "FDH", Orientacion = c("io", "oo"), Regimen = "NA",
    Parametro = "NA", Base = bases, Transformacion = trans, Year = anios
  )
  esp_ddf <- tidyr::expand_grid(
    Familia = "DDF", Orientacion = "direccional", Regimen = "vrs",
    Parametro = "dir_X_Y", Base = bases, Transformacion = trans, Year = anios
  )
  esp_orderm <- tidyr::expand_grid(
    Familia = "Orden_m", Orientacion = "io", Regimen = "NA",
    Parametro = c("m=3", "m=5", "m=8"), Base = bases, Transformacion = trans, Year = anios
  )
  esp_ordera <- tidyr::expand_grid(
    Familia = "Orden_alpha", Orientacion = "io", Regimen = "NA",
    Parametro = c("a=0.75", "a=0.85", "a=0.90"), Base = bases, Transformacion = trans, Year = anios
  )
  esp_boot <- tidyr::expand_grid(
    Familia = "Bootstrap", Orientacion = "io", Regimen = "vrs",
    Parametro = "B=2000", Base = bases, Transformacion = trans, Year = anios
  )
  esp_sfa <- tidyr::expand_grid(
    Familia = "SFA_Panel", Orientacion = "oo", Regimen = "log_lineal",
    Parametro = "BC92", Base = bases, Transformacion = trans, Year = NA_integer_
  )
  
  dplyr::bind_rows(
    esp_dea, esp_sbm, esp_fdh, esp_ddf, esp_orderm, esp_ordera, esp_boot, esp_sfa
  ) %>%
    dplyr::mutate(
      Universo_ID = sprintf("U%04d", dplyr::row_number()),
      Semilla     = 10000L + dplyr::row_number(),
      Estado_A_Priori = dplyr::case_when(
        Base == "Base_A" & !is.na(Year) & !(Year %in% anios_base_A) ~ "no_estimable",
        Base == "Base_A" & Transformacion == "IMR_max" ~ "no_estimable",
        Familia == "SBM" & Base == "Base_A" & (Year %in% anios_cero_A) ~ "no_estimable",
        Familia == "SBM" & Base == "Base_B" & (Year %in% anios_cero_B) ~ "no_estimable",
        Familia == "SFA_Panel" ~ "no_estimable",
        Base == "Base_B" & Transformacion == "IMR_max" ~ "copia_esperada",
        Familia == "DEA_Radial" & Regimen == "crs" & Orientacion == "oo" ~ "copia_esperada",
        TRUE ~ "estimable"
      ),
      Motivo_Estado = dplyr::case_when(
        Base == "Base_A" & !is.na(Year) & !(Year %in% anios_base_A) ~ "Anio ausente en DEA_dataset.xlsx (solo 2007, 2012 y 2017)",
        Base == "Base_A" & Transformacion == "IMR_max" ~ "Serie original ausente en repositorio (Base A)",
        Familia == "SBM" & Base %in% c("Base_A", "Base_B") & Estado_A_Priori == "no_estimable" ~ "Presencia de cero en insumo/salida (Min-Max)",
        Familia == "SFA_Panel" & Base %in% c("Base_A", "Base_B") ~ "Indefinicion de ln(0) en datos Min-Max",
        Familia == "SFA_Panel" & Transformacion == "IMR_max" ~ "Indefinicion de ln(0) en el maximo historico",
        Familia == "SFA_Panel" & Base == "Base_C" & Transformacion == "IMR_1000" ~ "Excluido por diagnostico previo (colinealidad r=0.9975 y no convergencia ML)",
        Base == "Base_B" & Transformacion == "IMR_max" ~ "Min-Max anula la traslacion lineal de IMR",
        Familia == "DEA_Radial" & Regimen == "crs" & Orientacion == "oo" ~ "Reciproco exacto de DEA CRS-IO",
        TRUE ~ "Candidato a estimacion y verificacion por huella"
      )
    ) %>%
    dplyr::select(Universo_ID, Familia, Orientacion, Regimen, Parametro, Base,
                  Transformacion, Year, Semilla, Estado_A_Priori, Motivo_Estado)
}