# ==============================================================================
# MÓDULO: R/03_modelos_deterministas.R
# ACTIVIDADES GANTT: A3 (pruebas con datos de control) y A4 (métodos deterministas)
# ==============================================================================

probar_modelos_deterministas_control <- function() {
  # A y B son eficientes; C y D tienen el doble de insumos y la mitad de salidas
  df_ctrl <- tibble::tibble(
    Country = c("DMU_A", "DMU_B", "DMU_C", "DMU_D"),
    x1 = c(2, 4, 4, 8),
    x2 = c(4, 2, 8, 4),
    y1 = c(10, 10, 5, 5),
    y2 = c(10, 10, 5, 5)
  )
  
  dea_ctrl <- deaR::make_deadata(
    datadea = df_ctrl,
    dmus    = "Country",
    inputs  = c("x1", "x2"),
    outputs = c("y1", "y2")
  )
  
  testthat::test_that("Pruebas de control para DEA, SBM, FDH y DDF", {
    e_dea <- unname(unlist(deaR::efficiencies(
      deaR::model_basic(dea_ctrl, orientation = "io", rts = "vrs")
    )))
    testthat::expect_equal(round(e_dea, 4), c(1, 1, 0.5, 0.5))
    
    e_sbm <- unname(unlist(deaR::efficiencies(
      deaR::model_sbmeff(dea_ctrl, rts = "vrs")
    )))
    testthat::expect_equal(round(e_sbm[1:2], 4), c(1, 1))
    testthat::expect_true(all(e_sbm[3:4] < 1))
    
    e_fdh <- unname(unlist(deaR::efficiencies(
      deaR::model_fdh(dea_ctrl, orientation = "io")
    )))
    testthat::expect_equal(round(e_fdh, 4), c(1, 1, 0.5, 0.5))
    
    # model_dir() devuelve rho: 1 = frontera, < 1 = ineficiente
    e_ddf <- unname(unlist(deaR::efficiencies(
      deaR::model_dir(dea_ctrl, rts = "vrs")
    )))
    testthat::expect_equal(round(e_ddf[1:2], 4), c(1, 1))
    testthat::expect_true(all(e_ddf[3:4] < 1))
  })
  
  TRUE
}

ejecutar_deterministas_multiverso <- function(datos_prep, tabla_especificaciones) {
  familias_det <- c("DEA_Radial", "SBM", "FDH", "DDF")
  
  esp_det <- tabla_especificaciones %>%
    dplyr::filter(Familia %in% familias_det, Estado_A_Priori != "no_estimable")
  
  obtener_df_base <- function(nombre_base, anio, trans) {
    df_full <- switch(
      nombre_base,
      "Base_A" = datos_prep$base_A,
      "Base_B" = datos_prep$base_B,
      "Base_C" = datos_prep$base_C
    )
    df <- df_full %>% dplyr::filter(Year == anio)
    if (nombre_base != "Base_A") {
      df <- df %>% dplyr::mutate(IMR = .data[[trans]])
    }
    df
  }
  
  resultados_lista  <- vector("list", nrow(esp_det))
  resumen_universos <- vector("list", nrow(esp_det))
  
  for (i in seq_len(nrow(esp_det))) {
    u  <- esp_det[i, ]
    df <- obtener_df_base(u$Base, u$Year, u$Transformacion)
    
    if (nrow(df) < 3) {
      resumen_universos[[i]] <- u %>%
        dplyr::mutate(
          Estado_Final  = "no_estimable",
          N_Paises      = nrow(df),
          N_Frontera    = NA_integer_,
          Huella_Rangos = NA_character_,
          Mensaje       = "Menos de 3 países completos en el año"
        )
      next
    }
    
    dea_obj <- suppressWarnings(
      deaR::make_deadata(
        datadea = df,
        dmus    = "Country",
        inputs  = c("HE", "HB", "PHY", "NUR"),
        outputs = c("LE", "IMR")
      )
    )
    
    out <- tryCatch({
      if (u$Familia == "DEA_Radial") {
        mod      <- deaR::model_basic(dea_obj, orientation = u$Orientacion, rts = u$Regimen)
        raw      <- unname(unlist(deaR::efficiencies(mod)))
        puntaje  <- if (u$Orientacion == "oo") 1 / raw else raw
        frontera <- abs(puntaje - 1) <= 1e-6
        rango    <- rank(-round(puntaje, 6), ties.method = "average")
      } else if (u$Familia == "SBM") {
        mod      <- deaR::model_sbmeff(dea_obj, rts = u$Regimen)
        puntaje  <- unname(unlist(deaR::efficiencies(mod)))
        frontera <- abs(puntaje - 1) <= 1e-6
        rango    <- rank(-round(puntaje, 6), ties.method = "average")
      } else if (u$Familia == "FDH") {
        mod      <- deaR::model_fdh(dea_obj, orientation = u$Orientacion)
        raw      <- unname(unlist(deaR::efficiencies(mod)))
        puntaje  <- if (u$Orientacion == "oo") 1 / raw else raw
        frontera <- abs(puntaje - 1) <= 1e-6
        rango    <- rank(-round(puntaje, 6), ties.method = "average")
      } else if (u$Familia == "DDF") {
        mod      <- deaR::model_dir(dea_obj, rts = "vrs")
        puntaje  <- unname(unlist(deaR::efficiencies(mod))) # rho: 1 = frontera, < 1 = ineficiente
        frontera <- abs(puntaje - 1) <= 1e-6
        rango    <- rank(-round(puntaje, 6), ties.method = "average")
      }
      
      if (any(is.na(puntaje)) || any(!is.finite(puntaje))) {
        stop("Puntajes NA o no finitos")
      }
      
      list(ok = TRUE, puntaje = puntaje, frontera = frontera, rango = rango, msg = "OK")
    }, error = function(e) {
      list(ok = FALSE, msg = conditionMessage(e))
    })
    
    if (!out$ok) {
      resumen_universos[[i]] <- u %>%
        dplyr::mutate(
          Estado_Final  = "no_estimable",
          N_Paises      = nrow(df),
          N_Frontera    = NA_integer_,
          Huella_Rangos = NA_character_,
          Mensaje       = out$msg
        )
    } else {
      huella <- digest::digest(
        paste(df$iso3c, round(out$rango, 4), collapse = "|"),
        algo = "md5"
      )
      resumen_universos[[i]] <- u %>%
        dplyr::mutate(
          Estado_Final  = "estimado",
          N_Paises      = nrow(df),
          N_Frontera    = sum(out$frontera),
          Huella_Rangos = huella,
          Mensaje       = "OK"
        )
      
      resultados_lista[[i]] <- tibble::tibble(
        Universo_ID    = u$Universo_ID,
        Familia        = u$Familia,
        Orientacion    = u$Orientacion,
        Regimen        = u$Regimen,
        Base           = u$Base,
        Transformacion = u$Transformacion,
        Year           = u$Year,
        iso3c          = df$iso3c,
        Country        = df$Country,
        Puntaje        = out$puntaje,
        Rango          = out$rango,
        En_Frontera    = out$frontera
      )
    }
  }
  
  tabla_estados <- dplyr::bind_rows(resumen_universos) %>%
    dplyr::group_by(Base, Year, Huella_Rangos) %>%
    dplyr::mutate(
      Es_Copia     = !is.na(Huella_Rangos) & dplyr::row_number() > 1,
      Estado_Final = dplyr::if_else(Es_Copia, "copia_de_otro", Estado_Final)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Es_Copia)
  
  tabla_larga <- dplyr::bind_rows(resultados_lista)
  
  list(
    estados_deterministas    = tabla_estados,
    resultados_deterministas = tabla_larga
  )
}