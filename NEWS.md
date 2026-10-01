# InkAgro 0.1.0

* Primera versión pública.
* `inkagro()` reemplaza a `inkagro_auto()`, `inkagro_detect()`,
  `inkagro_analyze()`, `inkagro_supuestos()` e `inkagro_plot()`.
* El diseño se declara (o se infiere de los argumentos) y se verifica contra
  la estructura de los datos; ya no se adivina por el nombre de las columnas.
* Diseños: DCA, DBCA, factorial de dos factores (en DCA o DBCA) y parcelas
  divididas balanceadas.
* Los valores atípicos se marcan; solo se excluyen con `outliers = "excluir"`.
* Resultados como objeto `inkagro` con métodos `print()`, `summary()`,
  `plot()` y `as.data.frame()`.
* `inkagro_clean()` ya no imputa por defecto.
