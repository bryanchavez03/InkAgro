# Elegir gráficos con un menú

Muestra en la consola un menú con los gráficos disponibles para este
análisis, dibuja el elegido e imprime la línea de código equivalente
para copiarla en el script. Así quien no conoce los argumentos de
\[plot()\]\[plot.inkagro\] puede explorar, y el análisis sigue siendo
reproducible.

## Usage

``` r
graficos(x)
```

## Arguments

- x:

  Objeto devuelto por \[inkagro()\].

## Value

El gráfico elegido (objeto \`ggplot\`), de forma invisible.

## Details

Ejecuta esta función en una línea sola: si se envía junto con otras
líneas, R toma la siguiente línea del script como respuesta al menú.

## Examples

``` r
if (interactive()) {
  res <- inkagro(PlantGrowth, "weight", "group", diseno = "dca")
  graficos(res)
}
```
