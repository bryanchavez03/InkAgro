# Leer un archivo de datos

Lee archivos \`.csv\` (con coma o punto y coma), \`.txt\`, \`.tsv\`,
\`.xlsx\`, \`.xls\`, \`.rds\`, \`.sav\` y \`.dta\` y devuelve un
\`data.frame\`. Es el mismo lector que usa \[inkagro()\]; si el archivo
no existe, sugiere dónde está.

## Usage

``` r
leer_datos(archivo)
```

## Arguments

- archivo:

  Ruta al archivo.

## Value

Un \`data.frame\`.

## Examples

``` r
f <- tempfile(fileext = ".csv")
write.csv2(PlantGrowth, f, row.names = FALSE)
head(leer_datos(f))
#>   weight group
#> 1   4.17  ctrl
#> 2   5.58  ctrl
#> 3   5.18  ctrl
#> 4   6.11  ctrl
#> 5   4.50  ctrl
#> 6   4.61  ctrl
unlink(f)
```
