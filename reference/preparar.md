# Ordenar una tabla de campo antes del análisis

Lee un Excel, CSV o \`data.frame\` armado "a mano" y lo deja como tabla
larga y limpia: una fila por parcela y una columna por variable. Corrige
la \*\*estructura\*\* de la tabla y anota cada cambio; lo que no puede
decidir con seguridad (valores que no son números, posibles comas mal
puestas) no lo cambia, sino que lo deja en la lista de cosas por
revisar.

## Usage

``` r
preparar(datos, hoja = NULL, variable = NULL)
```

## Arguments

- datos:

  Ruta a un archivo \`.xlsx\`, \`.xls\`, \`.csv\`, \`.txt\` o \`.tsv\`,
  o un \`data.frame\`.

- hoja:

  Hoja del Excel (nombre o número). Por defecto, la primera; si hay
  varias, se avisa en \`revisar\`.

- variable:

  Nombre para la variable medida cuando la tabla está en formato ancho y
  las columnas de bloques no tienen un encabezado encima (por ejemplo
  \`variable = "rendimiento"\`). Si es \`NULL\` se llama \`valor\`.

## Value

Un objeto de clase \`inkagro_preparado\`: una lista con \`datos\` (la
tabla ordenada), \`cambios\` (cada corrección hecha), \`revisar\` (lo
que conviene verificar a mano), \`unidades\` (unidades leídas de los
encabezados) y \`origen\`. Se puede pasar directamente a \[inkagro()\] y
guardar con \[exportar()\].

## Qué corrige

- Filas de título o vacías antes del encabezado.

- Encabezados en dos filas (por ejemplo "Rendimiento" sobre "I II III").

- Unidades en el encabezado: \`Rendimiento (t/ha)\` pasa a llamarse
  \`rendimiento\` y la unidad se guarda en \`unidades\`.

- Filas de resumen: Total, Promedio, Media, Suma, CV, desviación,
  máximo, mínimo, y notas al pie como "Fuente: ...".

- Celdas combinadas en columnas de tratamiento o bloque (el nombre
  escrito una vez y vacío debajo): se rellena hacia abajo.

- Formato ancho: bloques o repeticiones como columnas (\`I\`, \`II\`,
  \`III\`, \`R1\`, \`R2\`, \`Bloque 1\`...) se pasan a formato largo con
  una columna \`bloque\`.

- Coma decimal y números guardados como texto.

- Etiquetas que solo difieren en mayúsculas o espacios (\`T1\`, \`t1\`,
  \`T 1\`).

## Qué nunca hace

No rellena datos faltantes de la variable medida, no borra valores
atípicos y no junta nombres de variedades parecidos. Esos casos van a
\`revisar\`.

## Examples

``` r
# Tabla en formato ancho, con fila de promedio y coma decimal
crudo <- data.frame(
  Variedad = c("INIA 1", "inia 1", "Canchan", "Promedio"),
  I   = c("4,5", "4,1", "3,2", "3,9"),
  II  = c("4,8", "4,4", "3,0", "4,1"),
  III = c("4,6", "4,0", "s/d", "4,3"),
  check.names = FALSE)
p <- preparar(crudo, variable = "rendimiento")
p
#> 
#> InkAgro | Tabla preparada desde un data.frame
#> ------------------------------------------------------------------
#> 6 filas x 4 columnas: variedad, iii, bloque, rendimiento
#> 
#> Cambios hechos (4)
#>   - Fila de resumen: Se quitó la fila 'Promedio' (no es una parcela).
#>   - Coma decimal: Se leyó la coma como decimal en 6 valor(es) (columnas
#>     'i', 'ii').
#>   - Formato ancho a largo: Las columnas 'i', 'ii' eran bloques o
#>     repeticiones; se pasó a una fila por parcela con la columna 'bloque'.
#>   - Etiquetas unificadas: 'inia 1' -> 'INIA 1'
#> 
#> Por revisar (0)
#>   Nada.
#> 
#> Primeras filas:
#>  variedad iii bloque rendimiento
#>    INIA 1 4,6      I         4.5
#>    INIA 1 4,6     II         4.8
#>    INIA 1 4,0      I         4.1
#>    INIA 1 4,0     II         4.4
#>   Canchan s/d      I         3.2
#>   Canchan s/d     II         3.0
#> 
#> Guárdala con exportar(x, "datos_ordenados.xlsx") o analízala con inkagro(x, ...).
#> 
p$datos
#>   variedad iii bloque rendimiento
#> 1   INIA 1 4,6      I         4.5
#> 2   INIA 1 4,6     II         4.8
#> 3   INIA 1 4,0      I         4.1
#> 4   INIA 1 4,0     II         4.4
#> 5  Canchan s/d      I         3.2
#> 6  Canchan s/d     II         3.0
```
