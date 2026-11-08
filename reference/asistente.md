# Asistente para analizar un experimento sin saber R

Lee el archivo, reconoce qué columnas son variables medidas, factores,
bloques o localidades, y hace preguntas en lenguaje de campo (qué
mediste, qué comparaste, cómo se sembró). Con las respuestas ejecuta
\[inkagro()\], muestra el reporte, ofrece crear el informe en Word e
imprime las líneas de código para repetir el análisis sin preguntas.
Después del análisis ofrece un menú de gráficos.

## Usage

``` r
asistente(datos)
```

## Arguments

- datos:

  Ruta a un archivo de datos o un \`data.frame\`. Si se omite, se abre
  una ventana para elegir el archivo.

## Value

El resultado de \[inkagro()\], de forma invisible, o \`NULL\` si se
cancela. Guárdalo con \`res \<- asistente()\` para seguir usándolo.

## Details

Ejecuta esta función en una línea sola: si se envía junto con otras
líneas del script, R toma la siguiente línea como respuesta.

## Examples

``` r
if (interactive()) {
  res <- asistente()
}
```
