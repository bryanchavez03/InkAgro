# InkAgro 0.1.0

* Nueva `preparar()`: ordena tablas de campo hechas a mano (títulos, filas de
  promedio o total, notas al pie, celdas combinadas, bloques en columnas,
  encabezados en dos filas, unidades en el encabezado, coma decimal) y anota
  cada cambio. Lo dudoso no se corrige: queda en `revisar`. `exportar()` la
  guarda en Excel (hojas datos, cambios y revisar) y CSV, e `inkagro()` acepta
  el resultado directamente.
* `inkagro_clean()` deja de exportarse: modificaba datos sin avisar.
* Con parcelas perdidas (DBCA o factorial desbalanceado), las medias y la
  comparación con Tukey o LSD se calculan ajustadas por el modelo (medias de
  mínimos cuadrados, iguales a las de 'emmeans'), en lugar de promedios
  simples sesgados por las parcelas que faltan.
* Los avisos muestran la fila del archivo original (la del Excel cuando los
  datos vienen de `preparar()`).
* El gráfico de interacción en escala de grises ya no da avisos de 'ggplot2'.

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
* Factores cuantitativos (dosis): descomposición polinomial (lineal,
  cuadrático, cúbico) con el error correcto, ecuación, R² y máximo técnico.
* `plot()`: estilo de artículo en escala de grises y tipos `"puntos"`
  (intervalo de confianza), `"interaccion"`, `"regresion"` y `"residuos"`.
* `graficos()`: menú en la consola que dibuja el gráfico elegido e imprime
  la línea de código para repetirlo.
* `informe()`: documento de Word con tablas y figuras numeradas.
* Si el archivo no existe, se busca en Descargas, Escritorio y Documentos.
* `asistente()`: reconoce variables medidas, factores, bloques y localidades
  y pregunta en lenguaje de campo; imprime el código para repetir el
  análisis. `leer_datos()` exporta el lector de archivos.
* `prueba = "scottknott"`: grupos de medias que no se superponen (validado
  contra el paquete 'ScottKnott'); el asistente la ofrece con 10 o más
  tratamientos.
* `plot(tipo = "dendrograma")`: árbol de las divisiones de Scott-Knott.
