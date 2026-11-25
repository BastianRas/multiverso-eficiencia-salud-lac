# ==============================================================================
# MÓDULO: R/04_modelos_robustos.R
# ACTIVIDAD GANTT: A5 - Pruebas de control e integración de Orden-m y Orden-alfa
# ==============================================================================

probar_modelos_robustos_control <- function() {
  df_ctrl <- tibble::tibble(
    Country = c("DMU_A", "DMU_B", "DMU_C", "DMU_D"),
    x1 = c(2, 4, 4, 8),
    x2 = c(4, 2, 8, 4),
    y1 = c(10, 10, 5, 5),
    y2 = c(10, 10, 5, 5)
  )
  
  mat_x <- as.matrix(df_ctrl[, c("x1", "x2")])
  mat_y <- as.matrix(df_ctrl[, c("y1", "y2")])
  
  testthat::test_that("Pruebas de control para Orden-m y Orden-alfa (IO)", {
    set.seed(12345)
    om <- frontiles::ordermscore.boot(xobs = mat_x, yobs = mat_y, m = 3, B = 200)
    e_om <- as.numeric(om$input.mean)
    testthat::expect_equal(length(e_om), 4)
    testthat::expect_true(all(is.finite(e_om)))
    testthat::expect_true(min(e_om[1:2]) > max(e_om[3:4]))
    
    oa <- frontiles::alphascore(xobs = mat_x, yobs = mat_y, alpha = 0.85)
    e_oa <- as.numeric(oa$input)
    testthat::expect_equal(length(e_oa), 4)
    testthat::expect_true(all(is.finite(e_oa)))
    testthat::expect_true(min(e_oa[1:2]) > max(e_oa[3:4]))
  })
  
  TRUE
}

ejecutar_robustos_multiverso <- function(datos_prep, tabla_especificaciones) {
  familias_rob <- c("Orden_m", "Orden_alpha")
  
  esp_rob <- tabla_especificaciones %>%
    dplyr::filter(Familia %in% familias_rob, Estado_A_Priori != "no_estimable")
  
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
  
  resultados_lista  <- vector("list", nrow(esp_rob))
  resumen_universos <- vector("list", nrow(esp_rob))
  
  for (i in seq_len(nrow(esp_rob))) {
    u  <- esp_rob[i, ]
    df <- obtener_df_base(u$Base, u$Year, u$Transformacion)
    
    if (nrow(df) < 3) {
      resumen_universos[[i]] <- u %>%
        dplyr::mutate(
          Estado_Final  = "no_estimable",
          N_Paises      = nrow(df),
          N_Exteriores  = NA_integer_,
          Huella_Rangos = NA_character_,
          Mensaje       = "Menos de 3 países completos en el año"
        )
      next
    }
    
    mat_x <- as.matrix(df[, c("HE", "HB", "PHY", "NUR")])
    mat_y <- as.matrix(df[, c("LE", "IMR")])
    
    out <- tryCatch({
      if (u$Familia == "Orden_m") {
        m_val <- as.numeric(sub("m=", "", u$Parametro))
        set.seed(u$Semilla)
        mod <- frontiles::ordermscore.boot(xobs = mat_x, yobs = mat_y, m = m_val, B = 200)
        puntaje <- as.numeric(mod$input.mean)
      } else if (u$Familia == "Orden_alpha") {
        a_val <- as.numeric(sub("a=", "", u$Parametro))
        mod <- frontiles::alphascore(xobs = mat_x, yobs = mat_y, alpha = a_val)
        puntaje <- as.numeric(mod$input)
      }
      
      if (any(is.na(puntaje)) || any(!is.finite(puntaje))) {
        stop("Puntajes NA o no finitos")
      }
      
      # En fronteras parciales IO: puntaje > 1 indica unidad exterior (supereficiente)
      exterior <- puntaje > (1 + 1e-6)
      rango    <- rank(-round(puntaje, 6), ties.method = "average")
      
      list(ok = TRUE, puntaje = puntaje, exterior = exterior, rango = rango, msg = "OK")
    }, error = function(e) {
      list(ok = FALSE, msg = conditionMessage(e))
    })
    
    if (!out$ok) {
      resumen_universos[[i]] <- u %>%
        dplyr::mutate(
          Estado_Final  = "no_estimable",
          N_Paises      = nrow(df),
          N_Exteriores  = NA_integer_,
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
          N_Exteriores  = sum(out$exterior),
          Huella_Rangos = huella,
          Mensaje       = "OK"
        )
      
      resultados_lista[[i]] <- tibble::tibble(
        Universo_ID    = u$Universo_ID,
        Familia        = u$Familia,
        Orientacion    = u$Orientacion,
        Regimen        = u$Regimen,
        Parametro      = u$Parametro,
        Base           = u$Base,
        Transformacion = u$Transformacion,
        Year           = u$Year,
        iso3c          = df$iso3c,
        Country        = df$Country,
        Puntaje        = out$puntaje,
        Rango          = out$rango,
        Es_Exterior    = out$exterior
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
  
  # Control de ruido de Monte Carlo en 2017 (Base A, m = 5: B = 200 vs B = 500)
  df_17 <- datos_prep$base_A %>% dplyr::filter(Year == 2017)
  mx_17 <- as.matrix(df_17[, c("HE", "HB", "PHY", "NUR")])
  my_17 <- as.matrix(df_17[, c("LE", "IMR")])
  
  set.seed(20261)
  om_b200 <- as.numeric(frontiles::ordermscore.boot(mx_17, my_17, m = 5, B = 200)$input.mean)
  set.seed(99999)
  om_b500 <- as.numeric(frontiles::ordermscore.boot(mx_17, my_17, m = 5, B = 500)$input.mean)
  
  control_montecarlo <- tibble::tibble(
    Base               = "Base_A",
    Year               = 2017L,
    Parametro          = "m=5 (B=200 vs B=500, distinta semilla)",
    Tau_Kendall_Rangos = stats::cor(om_b200, om_b500, method = "kendall"),
    Dif_Max_Puntaje    = max(abs(om_b200 - om_b500))
  )
  
  list(
    estados_robustos    = tabla_estados,
    resultados_robustos = tabla_larga,
    control_montecarlo  = control_montecarlo
  )
}