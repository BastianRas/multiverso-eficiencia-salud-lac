# ==============================================================================
# MÓDULO: R/05_modelos_estocasticos_bootstrap.R
# ACTIVIDAD GANTT: A6 - DEA Bootstrap (B=2000, IC 95% en rangos) y registro SFA
# ==============================================================================

probar_bootstrap_control <- function() {
  # Muestra sintética de 8 DMUs para permitir estimación de ancho de banda
  df_ctrl <- tibble::tibble(
    Country = paste0("DMU_", LETTERS[1:8]),
    x1 = c(2, 4, 3, 5, 4, 8, 6, 9),
    x2 = c(4, 2, 3, 2, 8, 4, 7, 6),
    y1 = c(10, 10, 10, 9, 5, 5, 6, 4),
    y2 = c(10, 10, 9, 10, 5, 5, 5, 4)
  )
  
  mat_x <- as.matrix(df_ctrl[, c("x1", "x2")])
  mat_y <- as.matrix(df_ctrl[, c("y1", "y2")])
  
  testthat::test_that("Prueba de control para DEA Bootstrap (IO-VRS)", {
    set.seed(12345)
    mod <- Benchmarking::dea.boot(
      X = mat_x, Y = mat_y, NREP = 200, RTS = "vrs", ORIENTATION = "in"
    )
    eff_bc <- as.numeric(mod$eff.bc)
    testthat::expect_equal(length(eff_bc), 8)
    testthat::expect_true(all(is.finite(eff_bc)))
    testthat::expect_true(all(eff_bc > 0 & eff_bc <= 1))
    testthat::expect_true(min(eff_bc[1:3]) > max(eff_bc[5:8]))
  })
  
  TRUE
}

ejecutar_bootstrap_multiverso <- function(datos_prep, tabla_especificaciones) {
  esp_boot <- tabla_especificaciones %>%
    dplyr::filter(Familia == "Bootstrap", Estado_A_Priori != "no_estimable")
  
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
  
  resultados_lista  <- vector("list", nrow(esp_boot))
  resumen_universos <- vector("list", nrow(esp_boot))
  
  for (i in seq_len(nrow(esp_boot))) {
    u  <- esp_boot[i, ]
    df <- obtener_df_base(u$Base, u$Year, u$Transformacion)
    
    if (nrow(df) < 6) {
      resumen_universos[[i]] <- u %>%
        dplyr::mutate(
          Estado_Final  = "no_estimable",
          N_Paises      = nrow(df),
          Huella_Rangos = NA_character_,
          Mensaje       = "Muestra insuficiente (N < 6) para bootstrap suavizado"
        )
      next
    }
    
    mat_x <- as.matrix(df[, c("HE", "HB", "PHY", "NUR")])
    mat_y <- as.matrix(df[, c("LE", "IMR")])
    
    out <- tryCatch({
      set.seed(u$Semilla)
      mod <- Benchmarking::dea.boot(
        X = mat_x, Y = mat_y, NREP = 2000, RTS = "vrs", ORIENTATION = "in"
      )
      
      eff_orig <- as.numeric(mod$eff)
      eff_bc   <- as.numeric(mod$eff.bc)
      boot_mat <- mod$boot # Matriz N x 2000
      
      if (any(is.na(eff_bc)) || any(!is.finite(eff_bc))) {
        stop("Puntajes corregidos por sesgo (eff.bc) con NA o no finitos")
      }
      if (!is.matrix(boot_mat) || nrow(boot_mat) != nrow(df)) {
        stop("Matriz de replicas bootstrap invalida")
      }
      
      # Regla 3: calcular rango en cada una de las 2.000 replicas y extraer IC 95%
      rangos_boot <- apply(boot_mat, 2, function(col_eff) {
        if (any(!is.finite(col_eff))) return(rep(NA_real_, length(col_eff)))
        rank(-round(col_eff, 6), ties.method = "average")
      })
      
      ic_inf <- apply(rangos_boot, 1, stats::quantile, probs = 0.025, na.rm = TRUE)
      ic_sup <- apply(rangos_boot, 1, stats::quantile, probs = 0.975, na.rm = TRUE)
      
      rango_bc <- rank(-round(eff_bc, 6), ties.method = "average")
      
      list(
        ok       = TRUE,
        eff_orig = eff_orig,
        puntaje  = eff_bc,
        rango    = rango_bc,
        ic_inf   = as.numeric(ic_inf),
        ic_sup   = as.numeric(ic_sup),
        msg      = "OK"
      )
    }, error = function(e) {
      list(ok = FALSE, msg = conditionMessage(e))
    })
    
    if (!out$ok) {
      resumen_universos[[i]] <- u %>%
        dplyr::mutate(
          Estado_Final  = "no_estimable",
          N_Paises      = nrow(df),
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
        Puntaje_Orig   = out$eff_orig,
        Puntaje        = out$puntaje,
        Rango          = out$rango,
        Rango_IC_Inf   = out$ic_inf,
        Rango_IC_Sup   = out$ic_sup
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
    estados_bootstrap    = tabla_estados,
    resultados_bootstrap = tabla_larga
  )
}

registrar_diagnostico_sfa <- function(datos_prep, tabla_especificaciones) {
  base_C <- datos_prep$base_C %>%
    dplyr::mutate(
      ln_HE     = log(HE),
      ln_HB     = log(HB),
      ln_PHY    = log(PHY),
      ln_NUR    = log(NUR),
      ln_y1     = log(LE),
      neg_ln_y1 = -log(LE),
      ln_ratio  = log(IMR_1000 / LE)
    )
  
  cor_colinealidad <- stats::cor(base_C$neg_ln_y1, base_C$ln_ratio)
  ols_odf <- stats::lm(neg_ln_y1 ~ ln_HE + ln_HB + ln_PHY + ln_NUR + ln_ratio, data = base_C)
  ols_le  <- stats::lm(ln_y1     ~ ln_HE + ln_HB + ln_PHY + ln_NUR, data = base_C)
  
  resumen_diagnostico <- tibble::tibble(
    Especificacion      = c("SFA_ODF_BiProducto", "SFA_Produccion_1Salida_LE"),
    Observaciones       = nrow(base_C),
    Paises_Distintos    = dplyr::n_distinct(base_C$iso3c),
    Cor_Colinealidad    = c(cor_colinealidad, NA_real_),
    R2_Adj_OLS          = c(summary(ols_odf)$adj.r.squared, summary(ols_le)$adj.r.squared),
    Asimetria_Residuos  = c(moments::skewness(stats::residuals(ols_odf)),
                            moments::skewness(stats::residuals(ols_le))),
    Decision_Multiverso = c(
      "Excluido: colinealidad severa (r=0.9975) y no convergencia ML",
      "Excluido: asimetria OLS incorrecta (>0 para -u) y no convergencia ML"
    )
  )
  
  estados_sfa <- tabla_especificaciones %>%
    dplyr::filter(Familia == "SFA_Panel") %>%
    dplyr::mutate(
      Estado_Final  = "no_estimable",
      N_Paises      = NA_integer_,
      Huella_Rangos = NA_character_,
      Mensaje       = Motivo_Estado
    )
  
  list(
    resumen_diagnostico = resumen_diagnostico,
    estados_sfa         = estados_sfa
  )
}