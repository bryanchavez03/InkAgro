# Informe en Word de un análisis InkAgro

Genera un documento de Word con el resumen del experimento, los avisos,
la tabla de análisis de varianza, las conclusiones, los supuestos, las
comparaciones de medias, los valores atípicos y los gráficos, con tablas
y figuras numeradas listas para una tesis o un artículo.

## Usage

``` r
informe(
  x,
  archivo = "informe_inkagro.docx",
  titulo = NULL,
  fuente = "Times New Roman",
  tamano = 12,
  tablas = c("articulo", "cuadros"),
  graficos = TRUE
)
```

## Arguments

- x:

  Objeto devuelto por \[inkagro()\].

- archivo:

  Ruta del archivo \`.docx\` a crear.

- titulo:

  Título del informe. Por defecto se arma con la variable respuesta.

- fuente:

  Fuente del documento. Por defecto \`"Times New Roman"\`.

- tamano:

  Tamaño de letra en puntos. Por defecto 12.

- tablas:

  \`"articulo"\` (solo líneas horizontales, estilo de revista) o
  \`"cuadros"\` (todas las celdas con borde).

- graficos:

  Si es \`TRUE\` (por defecto), incluye las figuras.

## Value

La ruta del archivo creado, de forma invisible.

## Details

Requiere los paquetes 'officer' y 'flextable'.

## Examples

``` r
# \donttest{
if (requireNamespace("officer", quietly = TRUE) &&
    requireNamespace("flextable", quietly = TRUE)) {
  res <- inkagro(npk, "yield", "N", factor_b = "P", bloque = "block",
                 diseno = "factorial")
  archivo <- informe(res, file.path(tempdir(), "informe.docx"))
}
# }
```
