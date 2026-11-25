# ==============================================================================
# MÓDULO: R/06_analisis_multiverso.R
# ACTIVIDADES GANTT: A7 y A8 - Consolidación, Concordancia (Base A y Base C),
#                    Regla 3 (con 10 no eficientes y desplazamiento bruto),
#                    Estabilidad 2017, Descomposición de Varianza y Min-Max
# ==============================================================================

consolidar_y_analizar_multiverso <- function(
    datos_prep,
    tabla_especificaciones,
    res_deterministas,
    res_robustos,
    res_bootstrap,
    diagnostico_sfa,
    validacion_replicacion_2017
) {
  # ----------------------------------------------------------------------------
  # 1. CONSOLIDACIÓN DE ESTADOS Y TABLA LARGA (CON MARCA DE CERO EN INSUMO)
  # ----------------------------------------------------------------------------
  estados_ejecutados <- dplyr::bind_rows(
    res_deterministas$estados_deterministas %>%
      dplyr::select(Universo_ID, Estado_Final, N_Paises, Huella_Rangos, Mensaje),
    res_robustos$estados_robustos %>%
      dplyr::select(Universo_ID, Estado_Final, N_Paises, Huella_Rangos, Mensaje),
    res_bootstrap$estados_bootstrap %>%
      dplyr::select(Universo_ID, Estado_Final, N_Paises, Huella_Rangos, Mensaje),
    diagnostico_sfa$estados_sfa %>%
      dplyr::select(Universo_ID, Estado_Final, N_Paises, Huella_Rangos, Mensaje)
  )
  
  tabla_estados_maestra <- tabla_especificaciones %>%
    dplyr::left_join(estados_ejecutados, by = "Universo_ID") %>%
    dplyr::mutate(
      Estado_Final = dplyr::coalesce(Estado_Final, Estado_A_Priori),
      Mensaje      = dplyr::coalesce(Mensaje, Motivo_Estado)
    )
  
  ids_estimados <- tabla_estados_maestra %>%
    dplyr::filter(Estado_Final == "estimado") %>%
    dplyr::pull(Universo_ID)
  
  # Identificar países con cero exacto en algún insumo (HE, HB, PHY, NUR)
  ceros_insumos <- dplyr::bind_rows(
    datos_prep$ceros_base_A %>% dplyr::mutate(Base = "Base_A"),
    datos_prep$ceros_base_B %>% dplyr::mutate(Base = "Base_B")
  ) %>%
    dplyr::filter(Variable %in% c("HE", "HB", "PHY", "NUR")) %>%
    dplyr::distinct(Base, Year, iso3c) %>%
    dplyr::mutate(Tiene_Cero_Insumo = TRUE)
  
  df_det <- res_deterministas$resultados_deterministas %>%
    dplyr::mutate(Parametro = "NA", Es_Exterior = NA)
  df_rob <- res_robustos$resultados_robustos %>%
    dplyr::mutate(En_Frontera = NA)
  df_boot <- res_bootstrap$resultados_bootstrap %>%
    dplyr::mutate(En_Frontera = NA, Es_Exterior = NA) %>%
    dplyr::select(-Puntaje_Orig, -Rango_IC_Inf, -Rango_IC_Sup)
  
  tabla_larga <- dplyr::bind_rows(df_det, df_rob, df_boot) %>%
    dplyr::filter(Universo_ID %in% ids_estimados) %>%
    dplyr::left_join(ceros_insumos, by = c("Base", "Year", "iso3c")) %>%
    dplyr::mutate(
      Tiene_Cero_Insumo        = dplyr::coalesce(Tiene_Cero_Insumo, FALSE),
      Frontera_Por_Cero_Insumo = dplyr::coalesce(En_Frontera, FALSE) & Tiene_Cero_Insumo
    )
  
  # Exportar tabla larga consolidada a CSV (Sección 1.4 del informe)
  if (!dir.exists("Resultados")) dir.create("Resultados")
  readr::write_csv(tabla_larga, "Resultados/06b_Tabla_Larga_Multiverso.csv")
  
  # Función auxiliar para concordancia entre familias (Tau-b y W descriptiva)
  calcular_concordancia <- function(df_sub, nombre_base, anio) {
    rangos_fam <- df_sub %>%
      dplyr::group_by(Familia, iso3c, Country) %>%
      dplyr::summarise(Rango_Fam = mean(Rango, na.rm = TRUE), .groups = "drop")
    
    mat_fam <- rangos_fam %>%
      tidyr::pivot_wider(names_from = Familia, values_from = Rango_Fam) %>%
      dplyr::arrange(iso3c)
    
    fams <- setdiff(names(mat_fam), c("iso3c", "Country"))
    pares <- utils::combn(fams, 2, simplify = FALSE)
    
    pares_df <- purrr::map_dfr(pares, function(p) {
      v1 <- mat_fam[[p[1]]]
      v2 <- mat_fam[[p[2]]]
      n  <- length(v1)
      pares_tot <- n * (n - 1) / 2
      sin_empate <- 0L
      for (a in 1:(n - 1)) {
        for (b in (a + 1):n) {
          if (!is.na(v1[a]) && !is.na(v1[b]) && !is.na(v2[a]) && !is.na(v2[b])) {
            if (v1[a] != v1[b] && v2[a] != v2[b]) sin_empate <- sin_empate + 1L
          }
        }
      }
      tibble::tibble(
        Base             = nombre_base,
        Year             = as.integer(anio),
        Familia_1        = p[1],
        Familia_2        = p[2],
        Tau_b_Kendall    = stats::cor(v1, v2, method = "kendall", use = "complete.obs"),
        Pares_Sin_Empate = sin_empate,
        Pares_Totales    = as.integer(pares_tot)
      )
    })
    
    M_ranks   <- as.matrix(mat_fam[, fams])
    k_fam     <- ncol(M_ranks)
    n_paises  <- nrow(M_ranks)
    M_ranks_f <- apply(M_ranks, 2, rank, ties.method = "average")
    R_i       <- rowSums(M_ranks_f)
    S         <- sum((R_i - mean(R_i))^2)
    W_desc    <- (12 * S) / (k_fam^2 * (n_paises^3 - n_paises))
    
    global_df <- tibble::tibble(
      Base           = nombre_base,
      Year           = as.integer(anio),
      N_Paises       = n_paises,
      N_Familias     = k_fam,
      Tau_b_Promedio = mean(pares_df$Tau_b_Kendall, na.rm = TRUE),
      Tau_b_Min      = min(pares_df$Tau_b_Kendall, na.rm = TRUE),
      Tau_b_Max      = max(pares_df$Tau_b_Kendall, na.rm = TRUE),
      W_Kendall_Desc = W_desc
    )
    
    list(pares = pares_df, global = global_df, rangos_fam = rangos_fam)
  }
  
  # ----------------------------------------------------------------------------
  # 2. CONCORDANCIA EN BASE A (2017, 23 PAÍSES) Y BASE C (2017, 15 PAÍSES)
  # ----------------------------------------------------------------------------
  conc_A <- calcular_concordancia(
    tabla_larga %>% dplyr::filter(Base == "Base_A", Year == 2017),
    "Base_A", 2017
  )
  conc_C <- calcular_concordancia(
    tabla_larga %>% dplyr::filter(Base == "Base_C", Year == 2017, Transformacion == "IMR_1000"),
    "Base_C", 2017
  )
  
  resumen_concordancia_global <- dplyr::bind_rows(conc_A$global, conc_C$global)
  concordancia_pares_2017     <- conc_A$pares
  concordancia_pares_2017_C   <- conc_C$pares
  
  # ----------------------------------------------------------------------------
  # 3. REGLA 3: IC 95% BOOTSTRAP (TOTAL Y SOLO 10 NO EFICIENTES) + DESPLAZ. BRUTO
  # ----------------------------------------------------------------------------
  eficientes_ref_2017 <- validacion_replicacion_2017 %>%
    dplyr::filter(abs(DEA_IO_VRS - 1) <= 1e-6) %>%
    dplyr::pull(Country)
  
  rango_ref_2017 <- validacion_replicacion_2017 %>%
    dplyr::mutate(Rango_Ref_2017 = rank(-round(DEA_IO_VRS, 6), ties.method = "average")) %>%
    dplyr::select(Country, Rango_Ref_2017)
  
  ic_boot <- res_bootstrap$resultados_bootstrap %>%
    dplyr::select(Base, Year, Transformacion, iso3c, Rango_IC_Inf, Rango_IC_Sup) %>%
    dplyr::distinct()
  
  eval_regla3 <- tabla_larga %>%
    dplyr::filter(Familia != "Bootstrap") %>%
    dplyr::inner_join(ic_boot, by = c("Base", "Year", "Transformacion", "iso3c")) %>%
    dplyr::mutate(
      Fuera_IC       = (Rango < Rango_IC_Inf) | (Rango > Rango_IC_Sup),
      Grupo_Ref_2017 = dplyr::if_else(Country %in% eficientes_ref_2017,
                                      "Eficientes_13", "No_Eficientes_10")
    )
  
  regla3_por_familia <- eval_regla3 %>%
    dplyr::group_by(Base, Year, Familia) %>%
    dplyr::summarise(
      Total_Eval    = dplyr::n(),
      N_Fuera_IC    = sum(Fuera_IC, na.rm = TRUE),
      Prop_Fuera_IC = mean(Fuera_IC, na.rm = TRUE),
      Supera_5pct   = Prop_Fuera_IC > 0.05,
      .groups = "drop"
    )
  
  regla3_resumen_2017_A <- dplyr::bind_rows(
    regla3_por_familia %>%
      dplyr::filter(Base == "Base_A", Year == 2017) %>%
      dplyr::summarise(
        Subconjunto           = "Todos los paises (N=23)",
        N_Familias_Evaluadas  = dplyr::n(),
        Prop_Media_Igual_Peso = mean(Prop_Fuera_IC, na.rm = TRUE),
        Familias_Sobre_5pct   = sum(Supera_5pct),
        Veredicto_Hipotesis   = dplyr::if_else(
          Prop_Media_Igual_Peso > 0.05,
          "RESPALDADA (supera umbral del 5%)",
          "NO RESPALDADA (dentro del 5%)"
        )
      ),
    eval_regla3 %>%
      dplyr::filter(Base == "Base_A", Year == 2017, Grupo_Ref_2017 == "No_Eficientes_10") %>%
      dplyr::group_by(Familia) %>%
      dplyr::summarise(Prop_Fuera_IC = mean(Fuera_IC, na.rm = TRUE), .groups = "drop") %>%
      dplyr::summarise(
        Subconjunto           = "Solo 10 no eficientes (Regla 3 estricta)",
        N_Familias_Evaluadas  = dplyr::n(),
        Prop_Media_Igual_Peso = mean(Prop_Fuera_IC, na.rm = TRUE),
        Familias_Sobre_5pct   = sum(Prop_Fuera_IC > 0.05),
        Veredicto_Hipotesis   = dplyr::if_else(
          Prop_Media_Igual_Peso > 0.05,
          "RESPALDADA (supera umbral del 5%)",
          "NO RESPALDADA (dentro del 5%)"
        )
      )
  )
  
  # Desplazamiento bruto en puestos respecto al DEA IO-VRS 2017 (sin filtro bootstrap)
  desplazamiento_bruto_2017 <- tabla_larga %>%
    dplyr::filter(Base == "Base_A", Year == 2017) %>%
    dplyr::left_join(rango_ref_2017, by = "Country") %>%
    dplyr::mutate(Desplaz_Abs = abs(Rango - Rango_Ref_2017)) %>%
    dplyr::group_by(Familia) %>%
    dplyr::summarise(
      Desplaz_Medio   = mean(Desplaz_Abs, na.rm = TRUE),
      Desplaz_Mediano = stats::median(Desplaz_Abs, na.rm = TRUE),
      Desplaz_Maximo  = max(Desplaz_Abs, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ----------------------------------------------------------------------------
  # 4. ESTABILIDAD DE CONCLUSIONES CONCRETAS EN 2017 (BASE A)
  # ----------------------------------------------------------------------------
  peores5_ref_2017 <- validacion_replicacion_2017 %>%
    dplyr::arrange(DEA_IO_VRS) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::pull(Country)
  
  df_2017_A <- tabla_larga %>% dplyr::filter(Base == "Base_A", Year == 2017)
  
  estabilidad_frontera_2017 <- df_2017_A %>%
    dplyr::filter(!is.na(En_Frontera)) %>%
    dplyr::group_by(Universo_ID, Familia, Orientacion, Regimen) %>%
    dplyr::summarise(
      N_Frontera_Univ = sum(En_Frontera),
      Retenidos_de_13 = sum(En_Frontera & (Country %in% eficientes_ref_2017)),
      Jaccard_13      = sum(En_Frontera & (Country %in% eficientes_ref_2017)) /
        length(union(Country[En_Frontera], eficientes_ref_2017)),
      .groups = "drop"
    )
  
  curva_chile_2017 <- df_2017_A %>%
    dplyr::filter(Country == "Chile") %>%
    dplyr::select(Universo_ID, Familia, Orientacion, Regimen, Parametro, Puntaje, Rango, En_Frontera) %>%
    dplyr::arrange(Rango)
  
  estabilidad_peores5_2017 <- df_2017_A %>%
    dplyr::group_by(Universo_ID, Familia, Orientacion, Regimen, Parametro) %>%
    dplyr::arrange(dplyr::desc(Rango)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::summarise(
      Coinciden_Peores5 = sum(Country %in% peores5_ref_2017),
      Paises_Peores5    = paste(Country, collapse = ", "),
      .groups = "drop"
    )
  
  # ----------------------------------------------------------------------------
  # 5. SENSIBILIDAD Y DESCOMPOSICIÓN DE VARIANZA POR PAÍS (REGLA 5, BASE A 2017)
  # ----------------------------------------------------------------------------
  # Descomposición con igual peso por familia: Var_Total = Var_Entre_Familias + Var_Intra_Familia
  var_pais_2017 <- df_2017_A %>%
    dplyr::group_by(iso3c, Country, Familia) %>%
    dplyr::summarise(
      Media_Fam = mean(Rango, na.rm = TRUE),
      Var_Intra = if (dplyr::n() > 1) mean((Rango - mean(Rango))^2) else 0,
      .groups = "drop"
    ) %>%
    dplyr::group_by(iso3c, Country) %>%
    dplyr::summarise(
      Rango_Promedio        = mean(Media_Fam),
      Rango_DE              = stats::sd(Media_Fam),
      Rango_Min             = min(Media_Fam),
      Rango_Max             = max(Media_Fam),
      Amplitud_Rango        = max(Media_Fam) - min(Media_Fam),
      Var_Entre_Familias    = mean((Media_Fam - mean(Media_Fam))^2),
      Var_Intra_Familia     = mean(Var_Intra),
      Var_Total_EqPeso      = Var_Entre_Familias + Var_Intra_Familia,
      Pct_Var_Entre_Familia = dplyr::if_else(
        Var_Total_EqPeso > 0,
        100 * Var_Entre_Familias / Var_Total_EqPeso,
        0
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(Amplitud_Rango))
  
  # ----------------------------------------------------------------------------
  # 6. DESACOPLAMIENTO DE VERSIÓN (A vs B) Y ESCALA MIN-MAX (B vs C) EN 2017
  # ----------------------------------------------------------------------------
  paises_15_2017 <- tabla_larga %>%
    dplyr::filter(Base == "Base_C", Year == 2017) %>%
    dplyr::pull(iso3c) %>%
    unique()
  
  comp_bases_2017 <- tabla_larga %>%
    dplyr::filter(Year == 2017, Transformacion == "IMR_1000", iso3c %in% paises_15_2017) %>%
    dplyr::group_by(Base, Familia, Orientacion, Regimen, Parametro, iso3c, Country) %>%
    dplyr::summarise(Rango = mean(Rango), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Base, values_from = Rango)
  
  efecto_version_y_escala <- comp_bases_2017 %>%
    dplyr::group_by(Familia, Orientacion, Regimen, Parametro) %>%
    dplyr::summarise(
      N_Paises_Comunes   = sum(!is.na(Base_A) & !is.na(Base_B) & !is.na(Base_C)),
      Tau_b_Version_AvsB = if (sum(!is.na(Base_A) & !is.na(Base_B)) >= 3)
        stats::cor(Base_A, Base_B, method = "kendall", use = "complete.obs") else NA_real_,
      Tau_b_Escala_BvsC  = if (sum(!is.na(Base_B) & !is.na(Base_C)) >= 3)
        stats::cor(Base_B, Base_C, method = "kendall", use = "complete.obs") else NA_real_,
      Desplaz_Max_BvsC   = if (sum(!is.na(Base_B) & !is.na(Base_C)) >= 1)
        max(abs(Base_B - Base_C), na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
  
  # ----------------------------------------------------------------------------
  # 7. EXPORTAR CONSOLIDADO MAESTRO A EXCEL EN Resultados/
  # ----------------------------------------------------------------------------
  writexl::write_xlsx(
    list(
      "01_Estados_1062"        = tabla_estados_maestra,
      "02_Concordancia_Global" = resumen_concordancia_global,
      "03_Concordancia_BaseA"  = concordancia_pares_2017,
      "04_Concordancia_BaseC"  = concordancia_pares_2017_C,
      "05_Regla3_Resumen"      = regla3_resumen_2017_A,
      "06_Desplaz_Bruto_2017"  = desplazamiento_bruto_2017,
      "07_Frontera_13_2017"    = estabilidad_frontera_2017,
      "08_Curva_Chile_2017"    = curva_chile_2017,
      "09_Peores5_2017"        = estabilidad_peores5_2017,
      "10_Sensibilidad_Pais"   = var_pais_2017,
      "11_Efecto_Ver_Escala"   = efecto_version_y_escala
    ),
    path = "Resultados/06_Consolidado_Multiverso_Final.xlsx"
  )
  
  list(
    tabla_estados_maestra       = tabla_estados_maestra,
    resumen_concordancia_global = resumen_concordancia_global,
    concordancia_pares_2017     = concordancia_pares_2017,
    concordancia_pares_2017_C   = concordancia_pares_2017_C,
    regla3_resumen_2017_A       = regla3_resumen_2017_A,
    desplazamiento_bruto_2017   = desplazamiento_bruto_2017,
    estabilidad_frontera_2017   = estabilidad_frontera_2017,
    curva_chile_2017            = curva_chile_2017,
    estabilidad_peores5_2017    = estabilidad_peores5_2017,
    sensibilidad_pais_2017      = var_pais_2017,
    efecto_version_y_escala     = efecto_version_y_escala
  )
}