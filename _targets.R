# ==============================================================================
# ARCHIVO: _targets.R
# ACTIVIDADES GANTT: A2 a A9 - Pipeline completo + Figuras de publicación
# EJECUCIÓN: targets::tar_make(callr_function = NULL)
# ==============================================================================

library(targets)

tar_option_set(
  packages = c(
    "tidyverse", "readxl", "writexl", "digest",
    "deaR", "frontiles", "Benchmarking", "frontier", "plm", "moments",
    "testthat"
  )
)

tar_source("R")

list(
  # 1. Archivos de entrada rastreados
  tar_target(archivo_base_A, "DEA_dataset.xlsx", format = "file"),
  tar_target(archivo_wb_rds, "WorldBank_API_Crudo_2007_2022_2026-11-25.rds", format = "file"),
  tar_target(archivo_ref_repo, "referencia_repo_2017.csv", format = "file"),
  
  # 2. Preparación de Bases A, B y C + auditorías SHA-256, NA y ceros
  tar_target(
    datos_prep,
    preparar_bases_multiverso(archivo_base_A, archivo_wb_rds)
  ),
  
  # 3. Tabla parametrizada de especificaciones del multiverso (1.062 nominales)
  tar_target(
    tabla_especificaciones,
    construir_tabla_especificaciones(datos_prep)
  ),
  
  # 4. Replicación 2017 y batería de 9 pruebas automáticas (testthat)
  tar_target(
    validacion_replicacion_2017,
    ejecutar_y_validar_replicacion(datos_prep, archivo_ref_repo)
  ),
  
  # 5. Actividad A3 y A4: Pruebas de control y 4 familias deterministas
  tar_target(
    test_deterministas_control,
    probar_modelos_deterministas_control()
  ),
  tar_target(
    res_deterministas,
    ejecutar_deterministas_multiverso(datos_prep, tabla_especificaciones)
  ),
  
  # 6. Actividad A5: Pruebas de control y fronteras robustas (Orden-m y Orden-alfa)
  tar_target(
    test_robustos_control,
    probar_modelos_robustos_control()
  ),
  tar_target(
    res_robustos,
    ejecutar_robustos_multiverso(datos_prep, tabla_especificaciones)
  ),
  
  # 7. Actividad A6: DEA Bootstrap (B=2000, IC 95% rangos) y registro formal SFA
  tar_target(
    test_bootstrap_control,
    probar_bootstrap_control()
  ),
  tar_target(
    res_bootstrap,
    ejecutar_bootstrap_multiverso(datos_prep, tabla_especificaciones)
  ),
  tar_target(
    diagnostico_sfa,
    registrar_diagnostico_sfa(datos_prep, tabla_especificaciones)
  ),
  
  # 8. Actividades A7 y A8: Consolidación, Concordancia, Regla 3 y Estabilidad 2017
  tar_target(
    analisis_multiverso_final,
    consolidar_y_analizar_multiverso(
      datos_prep,
      tabla_especificaciones,
      res_deterministas,
      res_robustos,
      res_bootstrap,
      diagnostico_sfa,
      validacion_replicacion_2017
    )
  ),
  
  # 9. Actividad A9: Generación automática de las 4 figuras en Resultados/Figuras/
  tar_target(
    figuras_multiverso,
    generar_figuras_multiverso(analisis_multiverso_final),
    format = "file"
  )
)
