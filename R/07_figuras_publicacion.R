# ==============================================================================
# MÓDULO: R/07_figuras_publicacion.R
# ACTIVIDAD GANTT: A9 - Generación automática de figuras en alta resolución
# SALIDA: Resultados/Figuras/*.png (300 dpi)
# ==============================================================================

generar_figuras_multiverso <- function(analisis_multiverso_final) {
  dir_fig <- "Resultados/Figuras"
  if (!dir.exists(dir_fig)) dir.create(dir_fig, recursive = TRUE)
  
  tema_pub <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle    = ggplot2::element_text(size = 10, color = "gray30"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    )
  
  # ----------------------------------------------------------------------------
  # FIGURA 1: Mapa de calor de concordancia (Tau-b de Kendall entre familias, 2017)
  # ----------------------------------------------------------------------------
  pares <- analisis_multiverso_final$concordancia_pares_2017
  fams  <- sort(unique(c(pares$Familia_1, pares$Familia_2)))
  
  mat_sym <- dplyr::bind_rows(
    pares %>% dplyr::select(F1 = Familia_1, F2 = Familia_2, Tau_b = Tau_b_Kendall),
    pares %>% dplyr::select(F1 = Familia_2, F2 = Familia_1, Tau_b = Tau_b_Kendall),
    tibble::tibble(F1 = fams, F2 = fams, Tau_b = 1)
  ) %>%
    dplyr::mutate(
      F1 = factor(F1, levels = fams),
      F2 = factor(F2, levels = rev(fams)),
      Etiqueta = as.character(round(Tau_b, 2))
    )
  
  g1 <- ggplot2::ggplot(mat_sym, ggplot2::aes(x = F1, y = F2, fill = Tau_b)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = Etiqueta), size = 4, fontface = "bold") +
    ggplot2::scale_fill_gradient2(
      low = "#d73027", mid = "#fee08b", high = "#1a9850",
      midpoint = 0.5, limits = c(0, 1), name = "Tau-b de Kendall"
    ) +
    ggplot2::labs(
      title    = "Concordancia entre familias metodológicas (Base A, 2017)",
      subtitle = "Coeficiente Tau-b de Kendall sobre rangos promedio por familia (23 países)",
      x = NULL, y = NULL
    ) +
    tema_pub
  
  ruta_f1 <- file.path(dir_fig, "Fig1_Mapa_Calor_Concordancia_2017.png")
  ggplot2::ggsave(ruta_f1, g1, width = 7, height = 5.5, dpi = 300, bg = "white")
  
  # ----------------------------------------------------------------------------
  # FIGURA 2: Curva de especificación para Chile (Base A, 2017)
  # ----------------------------------------------------------------------------
  df_chile <- analisis_multiverso_final$curva_chile_2017 %>%
    dplyr::mutate(
      Especificacion = paste0(
        Familia, " (",
        ifelse(Orientacion == "NA", "", Orientacion),
        ifelse(Regimen == "NA", "", paste0("-", Regimen)),
        ifelse(Parametro == "NA", "", paste0(" ", Parametro)),
        ")"
      ),
      Especificacion = gsub("\\(-", "(", Especificacion),
      Especificacion = gsub("\\( ", "(", Especificacion),
      Especificacion = stats::reorder(Especificacion, Rango)
    )
  
  g2 <- ggplot2::ggplot(df_chile, ggplot2::aes(x = Especificacion, y = Rango, color = Familia)) +
    ggplot2::geom_hline(yintercept = 7, linetype = "dashed", color = "gray40") +
    ggplot2::geom_segment(
      ggplot2::aes(xend = Especificacion, y = 1, yend = Rango),
      color = "gray80", linewidth = 0.7
    ) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(Rango, 1)),
      hjust = -0.45, size = 3.5, show.legend = FALSE
    ) +
    ggplot2::scale_y_reverse(limits = c(23, 1), breaks = c(1, 5, 7, 10, 15, 20, 23)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = "Curva de especificación: Posición relativa de Chile (2017, Base A)",
      subtitle = "Línea punteada = posición de referencia DEA VRS (rango 7,0 empatado en frontera)",
      x = NULL, y = "Posición en el ranking (1 = mejor, 23 = peor)"
    ) +
    tema_pub
  
  ruta_f2 <- file.path(dir_fig, "Fig2_Curva_Especificacion_Chile_2017.png")
  ggplot2::ggsave(ruta_f2, g2, width = 8.5, height = 5.5, dpi = 300, bg = "white")
  
  # ----------------------------------------------------------------------------
  # FIGURA 3: Sensibilidad posicional por país (Base A, 2017)
  # ----------------------------------------------------------------------------
  df_sens <- analisis_multiverso_final$sensibilidad_pais_2017 %>%
    dplyr::mutate(Country = stats::reorder(Country, Amplitud_Rango))
  
  g3 <- ggplot2::ggplot(df_sens, ggplot2::aes(y = Country)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = Rango_Min, xend = Rango_Max, yend = Country),
      color = "#4575b4", linewidth = 1.6, alpha = 0.75
    ) +
    ggplot2::geom_point(ggplot2::aes(x = Rango_Min), color = "#1a9850", size = 2.6) +
    ggplot2::geom_point(ggplot2::aes(x = Rango_Max), color = "#d73027", size = 2.6) +
    ggplot2::geom_point(ggplot2::aes(x = Rango_Promedio), color = "black", shape = 18, size = 3) +
    ggplot2::scale_x_continuous(breaks = seq(1, 23, by = 2), limits = c(1, 23)) +
    ggplot2::labs(
      title    = "Sensibilidad posicional por país frente al método (Base A, 2017)",
      subtitle = "Verde = mejor rango | Rombo negro = promedio | Rojo = peor rango (ordenado por amplitud)",
      x = "Posición en el ranking (1 = mejor, 23 = peor)", y = NULL
    ) +
    tema_pub
  
  ruta_f3 <- file.path(dir_fig, "Fig3_Sensibilidad_Posicional_Paises_2017.png")
  ggplot2::ggsave(ruta_f3, g3, width = 8.5, height = 7, dpi = 300, bg = "white")
  
  # ----------------------------------------------------------------------------
  # FIGURA 4: Desacoplamiento de versión (A vs B) y escala Min-Max (B vs C) en 2017
  # ----------------------------------------------------------------------------
  df_ve <- analisis_multiverso_final$efecto_version_y_escala %>%
    dplyr::filter(!is.na(Tau_b_Version_AvsB) | !is.na(Tau_b_Escala_BvsC)) %>%
    dplyr::mutate(
      Modelo = paste0(
        Familia, " ",
        ifelse(Orientacion == "NA", "", Orientacion),
        ifelse(Regimen == "NA", "", paste0("-", Regimen)),
        ifelse(Parametro == "NA", "", paste0(" ", Parametro))
      )
    ) %>%
    tidyr::pivot_longer(
      cols = c(Tau_b_Version_AvsB, Tau_b_Escala_BvsC),
      names_to = "Efecto",
      values_to = "Tau_b"
    ) %>%
    dplyr::filter(!is.na(Tau_b)) %>%
    dplyr::mutate(
      Efecto = dplyr::recode(
        Efecto,
        "Tau_b_Version_AvsB" = "Efecto Versión (Base A vs. Base B)",
        "Tau_b_Escala_BvsC"  = "Efecto Escala Min-Max (Base B vs. Base C)"
      )
    )
  
  g4 <- ggplot2::ggplot(df_ve, ggplot2::aes(x = stats::reorder(Modelo, Tau_b), y = Tau_b, fill = Efecto)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.65) +
    ggplot2::geom_hline(yintercept = 0.7, linetype = "dashed", color = "gray40") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(min(0, min(df_ve$Tau_b) - 0.05), 1.05)) +
    ggplot2::scale_fill_manual(values = c("#2b83ba", "#fdae61")) +
    ggplot2::labs(
      title    = "Desacoplamiento del efecto versión vs. efecto escala Min-Max (2017)",
      subtitle = "Tau-b de Kendall sobre la intersección de 15 países completos (1,0 = sin cambio)",
      x = NULL, y = "Concordancia (Tau-b de Kendall)", fill = NULL
    ) +
    tema_pub
  
  ruta_f4 <- file.path(dir_fig, "Fig4_Efecto_Version_y_Escala_2017.png")
  ggplot2::ggsave(ruta_f4, g4, width = 8.5, height = 5.5, dpi = 300, bg = "white")
  
  c(ruta_f1, ruta_f2, ruta_f3, ruta_f4)
}