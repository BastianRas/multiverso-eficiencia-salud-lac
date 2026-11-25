# ==============================================================================
# MÓDULO: R/02_replicacion_y_pruebas.R
# PROPÓSITO: Replicación oficial de Peres et al. (2025) para 2017 y batería
#            de 9 pruebas automáticas (testthat) integradas al pipeline.
# ==============================================================================

ejecutar_y_validar_replicacion <- function(datos_prep, archivo_ref_repo) {
  datos_panel <- datos_prep$base_A
  datos_2017  <- datos_panel %>% dplyr::filter(Year == 2017)
  
  dea_2017 <- deaR::make_deadata(
    datadea = datos_2017,
    dmus    = "Country",
    inputs  = c("HE", "HB", "PHY", "NUR"),
    outputs = c("LE", "IMR")
  )
  
  res_vrs_io <- deaR::model_basic(dea_2017, orientation = "io", rts = "vrs")
  res_vrs_oo <- deaR::model_basic(dea_2017, orientation = "oo", rts = "vrs")
  
  ref_repo_2017 <- readr::read_csv(archivo_ref_repo, show_col_types = FALSE) %>%
    dplyr::select(Country, Ref_Repo_6dec)
  
  ref_articulo_2dec <- tibble::tibble(
    Country = c(
      "Argentina", "Bahamas, The", "Barbados", "Belize", "Bolivia",
      "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba",
      "Dominican Republic", "El Salvador", "Guatemala", "Honduras",
      "Jamaica", "Mexico", "Nicaragua", "Peru", "St. Lucia",
      "Suriname", "Trinidad and Tobago", "Uruguay", "Venezuela, RB"
    ),
    Ref_Articulo_2dec = c(
      0.99, 0.57, 0.74, 1.00, 0.64, 0.42, 1.00, 1.00, 1.00, 1.00,
      1.00, 0.90, 1.00, 1.00, 1.00, 1.00, 0.74, 1.00, 1.00, 0.88,
      0.37, 0.83, 1.00
    )
  )
  
  eff_io_vrs      <- unname(unlist(deaR::efficiencies(res_vrs_io)))
  eff_oo_vrs      <- unname(unlist(deaR::efficiencies(res_vrs_oo)))
  eff_oo_vrs_norm <- 1 / eff_oo_vrs
  
  tabla_validacion <- tibble::tibble(
    Country        = datos_2017$Country,
    DEA_IO_VRS     = eff_io_vrs,
    DEA_OO_VRS_INV = eff_oo_vrs_norm
  ) %>%
    dplyr::inner_join(ref_repo_2017, by = "Country") %>%
    dplyr::inner_join(ref_articulo_2dec, by = "Country") %>%
    dplyr::mutate(
      Dif_Articulo_2dec = abs(DEA_IO_VRS - Ref_Articulo_2dec),
      Cumple_2dec       = Dif_Articulo_2dec <= 0.005,
      Dif_Repo_6dec     = abs(round(DEA_IO_VRS, 6) - Ref_Repo_6dec),
      Cumple_6dec       = Dif_Repo_6dec <= 0.0001
    )
  
  # Batería de 9 pruebas automáticas (Anexo B)
  testthat::test_that("Integridad, copias CRS, ceros en el panel y propiedades OO", {
    testthat::expect_equal(nrow(tabla_validacion), 23)
    testthat::expect_true(all(tabla_validacion$Cumple_2dec))
    testthat::expect_true(all(tabla_validacion$Cumple_6dec))
    
    testthat::expect_equal(sum(is.na(tabla_validacion$DEA_OO_VRS_INV)), 0)
    testthat::expect_true(all(tabla_validacion$DEA_OO_VRS_INV >= 0 &
                                tabla_validacion$DEA_OO_VRS_INV <= 1 + 1e-6))
    frontera_io <- abs(tabla_validacion$DEA_IO_VRS - 1) <= 1e-6
    frontera_oo <- abs(tabla_validacion$DEA_OO_VRS_INV - 1) <= 1e-6
    testthat::expect_equal(frontera_oo, frontera_io)
    
    res_crs_io <- deaR::model_basic(dea_2017, orientation = "io", rts = "crs")
    res_crs_oo <- deaR::model_basic(dea_2017, orientation = "oo", rts = "crs")
    eff_crs_io <- round(unname(unlist(deaR::efficiencies(res_crs_io))), 6)
    eff_crs_oo <- round(1 / unname(unlist(deaR::efficiencies(res_crs_oo))), 6)
    rangos_io  <- rank(-eff_crs_io, ties.method = "average")
    rangos_oo  <- rank(-eff_crs_oo, ties.method = "average")
    testthat::expect_equal(rangos_io, rangos_oo)
    
    registro_ceros_panel <- datos_panel %>%
      tidyr::pivot_longer(
        cols = c(HE, HB, PHY, NUR, LE, IMR),
        names_to = "Variable",
        values_to = "Valor"
      ) %>%
      dplyr::filter(Valor == 0)
    testthat::expect_true(nrow(registro_ceros_panel) > 0)
    testthat::expect_true(all(c("Country", "Year", "Variable") %in%
                                names(registro_ceros_panel)))
  })
  
  tabla_validacion
}