# Tablas de un análisis InkAgro

Extrae como \`data.frame\` la tabla de análisis de varianza, las
comparaciones de medias o los valores atípicos, para exportarlas (por
ejemplo con \`write.csv()\`) o seguir trabajando con ellas.

## Usage

``` r
# S3 method for class 'inkagro'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  tabla = c("anova", "medias", "atipicos"),
  ...
)
```

## Arguments

- x:

  Objeto devuelto por \[inkagro()\].

- row.names, optional:

  No se usan; existen por compatibilidad con \[as.data.frame()\].

- tabla:

  \`"anova"\` (por defecto), \`"medias"\` o \`"atipicos"\`.

- ...:

  No se usa.

## Value

Un \`data.frame\`.

## Examples

``` r
res <- inkagro(PlantGrowth, "weight", "group", diseno = "dca")
as.data.frame(res)
#>   fuente gl       sc        cm        F          p sig
#> 1  group  2  3.76634 1.8831700 4.846088 0.01590996   *
#> 2  Error 27 10.49209 0.3885959       NA         NA    
#> 3  Total 29 14.25843        NA       NA         NA    
as.data.frame(res, tabla = "medias")
#>   comparacion nivel  n media        ee grupo
#> 1       group  trt2 10 5.526 0.1971284     a
#> 2       group  ctrl 10 5.032 0.1971284    ab
#> 3       group  trt1 10 4.661 0.1971284     b
```
