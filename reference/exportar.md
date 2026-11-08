# Guardar una tabla preparada en Excel y CSV

Escribe un Excel con tres hojas (\`datos\`, \`cambios\` y \`revisar\`)
para que se pueda verificar cada corrección, y un CSV con solo los
datos, listo para cualquier programa.

## Usage

``` r
exportar(x, archivo = "datos_ordenados.xlsx", csv = TRUE)
```

## Arguments

- x:

  Objeto devuelto por \[preparar()\].

- archivo:

  Ruta del Excel a crear (\`.xlsx\`).

- csv:

  Si es \`TRUE\` (por defecto), también guarda un \`.csv\` con el mismo
  nombre.

## Value

Las rutas creadas, de forma invisible.

## Details

Requiere el paquete 'writexl' para el Excel.

## Examples

``` r
# \donttest{
if (requireNamespace("writexl", quietly = TRUE)) {
  crudo <- data.frame(Tratamiento = c("T1", "T2", "Total"),
                      I = c("4,5", "3,2", "7,7"), II = c("4,8", "3,0", "7,8"))
  p <- preparar(crudo)
  exportar(p, file.path(tempdir(), "datos_ordenados.xlsx"))
}
#> Guardado: /tmp/Rtmp9keI24/datos_ordenados.xlsx y /tmp/Rtmp9keI24/datos_ordenados.csv
# }
```
