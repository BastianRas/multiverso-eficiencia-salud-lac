# Análisis Multiverso de la Eficiencia de los Sistemas de Salud en LAC (2007–2017)

Repositorio reproducible en **R** (`targets` + `renv`) para evaluar la sensibilidad metodológica de los rankings de eficiencia sanitaria en 23 países de América Latina y el Caribe, tomando como referencia el estudio de Peres et al. (2025).

## Estructura del repositorio

- `_targets.R`: Orquestador principal del pipeline (15 nodos encadenados).
- `renv.lock`: Versiones exactas de todos los paquetes de R utilizados.
- `R/01_datos_y_especificaciones.R`: Verificación SHA-256, construcción de Bases A, B y C y tabla de 1.062 universos nominales.
- `R/02_replicacion_y_pruebas.R`: Replicación oficial de Peres et al. (2025) para 2017 y batería de 9 pruebas automáticas (`testthat`).
- `R/03_modelos_deterministas.R`: Pruebas de control y estimación de DEA Radial, SBM, FDH y DDF.
- `R/04_modelos_robustos.R`: Pruebas de control, fronteras parciales de orden-$m$ y orden-$\alpha$, y control de ruido Monte Carlo.
- `R/05_modelos_estocasticos_bootstrap.R`: DEA Bootstrap ($B = 2000$ con IC 95% en rangos) y registro formal del diagnóstico SFA en Base C.
- `R/06_analisis_multiverso.R`: Consolidado maestro, tabla larga con marca de ceros en insumos, concordancia en Bases A y C ($\tau_b$ y $W$), Regla 3, curva de Chile, descomposición de varianza por país y efecto versión/escala.
- `R/07_figuras_publicacion.R`: Generación automática de 4 figuras en 300 dpi.
- `00_descarga_api_banco_mundial.R`: Script auditable de descarga cruda desde la API del Banco Mundial.
- `00c_diagnostico_sfa.R`: Script independiente de diagnóstico numérico previo para SFA.
- `DEA_dataset.xlsx`, `referencia_repo_2017.csv`, `WorldBank_API_Crudo_2007_2022_2026-11-25.rds`: Datos fuente verificados por SHA-256.
- `Resultados/`: Consolidado Excel (`06_Consolidado_Multiverso_Final.xlsx`), tabla larga (`06b_Tabla_Larga_Multiverso.csv`) y figuras (`Resultados/Figuras/`).

## Cómo reproducir el análisis completo

1. Clonar el repositorio:
```bash
git clone [https://github.com/BastianRas/multiverso-eficiencia-salud-lac.git](https://github.com/BastianRas/multiverso-eficiencia-salud-lac.git)
```
2. Abrir RStudio en la carpeta del repositorio y restaurar las dependencias (solo la primera vez):
```r
install.packages("renv") # Si no está instalado
renv::restore()
```
3. Ejecutar el flujo completo de punta a punta:
```r
targets::tar_make(callr_function = NULL)
```
*(En carpetas locales fuera de OneDrive también puede ejecutarse con `targets::tar_make()`).*

## Salidas generadas automáticamente
- `Resultados/06_Consolidado_Multiverso_Final.xlsx`: Libro Excel con las 11 hojas analíticas del multiverso.
- `Resultados/06b_Tabla_Larga_Multiverso.csv`: Tabla consolidada en formato largo (universo × país × año) con marca de frontera por cero en insumo.
- `Resultados/Figuras/`: 4 gráficos PNG en alta resolución (300 dpi).
