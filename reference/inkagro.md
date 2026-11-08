# Analiza un experimento agrícola de principio a fin

Lee los datos, verifica que su estructura corresponda al diseño
experimental, ajusta el análisis de varianza, evalúa los supuestos,
identifica valores atípicos y compara medias. Devuelve un objeto
\`inkagro\` que se imprime como un reporte en español y que se puede
graficar con \[plot()\]\[plot.inkagro\] o convertir a tablas con
\[as.data.frame()\]\[as.data.frame.inkagro\].

## Usage

``` r
inkagro(
  datos,
  respuesta,
  tratamiento,
  diseno = NULL,
  bloque = NULL,
  factor_b = NULL,
  prueba = c("tukey", "duncan", "lsd", "snk", "scottknott"),
  alfa = 0.05,
  outliers = c("marcar", "excluir")
)
```

## Arguments

- datos:

  Un \`data.frame\`, la ruta a un archivo \`.csv\`, \`.txt\`, \`.tsv\`,
  \`.xlsx\`, \`.xls\`, \`.rds\`, \`.sav\` o \`.dta\`, o una tabla
  ordenada con \[preparar()\]. Si el Excel tiene títulos, filas de
  promedio o los bloques en columnas, pásalo primero por \[preparar()\].

- respuesta:

  Nombre de la columna con la variable medida (por ejemplo,
  rendimiento). No distingue mayúsculas.

- tratamiento:

  Nombre de la columna de tratamientos. En factoriales es el primer
  factor; en parcelas divididas, el de la parcela principal.

- diseno:

  Uno de \`"dca"\`, \`"dbca"\`, \`"factorial"\` o
  \`"parcelas_divididas"\` (se aceptan alias como \`"rcbd"\`, \`"crd"\`,
  \`"split_plot"\`). Si es \`NULL\` se infiere de los argumentos
  indicados.

- bloque:

  Nombre de la columna de bloques o repeticiones, si las hay.

- factor_b:

  Nombre de la columna del segundo factor (factorial) o de la subparcela
  (parcelas divididas).

- prueba:

  Prueba de comparación de medias: \`"tukey"\` (por defecto),
  \`"duncan"\`, \`"lsd"\` o \`"snk"\` (calculadas con el paquete
  'agricolae'), o \`"scottknott"\`, que forma grupos que no se
  superponen y conviene cuando hay muchos tratamientos (Scott y Knott,
  1974).

- alfa:

  Nivel de significancia. Por defecto 0.05.

- outliers:

  Qué hacer con los valores atípicos (residuo estandarizado mayor que 3
  en valor absoluto): \`"marcar"\` (por defecto) los reporta sin
  quitarlos; \`"excluir"\` repite el análisis sin ellos y reporta cuáles
  se quitaron. Nunca se eliminan datos en silencio.

## Value

Un objeto de clase \`inkagro\`: una lista con el diseño, la tabla de
análisis de varianza (\`anova\`), las comparaciones de medias
(\`medias\`), el coeficiente de variación (\`cv\`), la evaluación de
supuestos (\`supuestos\`), los valores atípicos (\`atipicos\`), los
avisos generados (\`avisos\`), el modelo ajustado (\`modelo\`, de clase
\`lm\`) y los datos usados (\`datos\`).

## Diseños soportados

- \`"dca"\`:

  Completamente al azar. Requiere \`tratamiento\`.

- \`"dbca"\`:

  Bloques completos al azar. Requiere \`tratamiento\` y \`bloque\`.

- \`"factorial"\`:

  Factorial de dos factores (\`tratamiento\` y \`factor_b\`), en DCA o,
  si se indica \`bloque\`, en DBCA.

- \`"parcelas_divididas"\`:

  Parcelas divididas en bloques: \`tratamiento\` es el factor de la
  parcela principal y \`factor_b\` el de la subparcela. Requiere datos
  balanceados.

## Por qué se pide el diseño

El diseño depende de cómo se aleatorizó en campo, y eso no siempre se
puede deducir de los datos: un factorial en bloques y unas parcelas
divididas producen tablas idénticas, pero se analizan con errores
distintos. Por eso InkAgro no adivina: si \`diseno\` no se indica, lo
infiere solo a partir de los argumentos dados (y lo avisa), y en todos
los casos comprueba que los datos sean coherentes con el diseño. Si no
lo son (por ejemplo, varias localidades mezcladas, bloques que no
contienen los tratamientos o un factorial incompleto), se detiene con un
mensaje que explica el problema en lugar de producir un análisis
incorrecto.

## References

Pimentel-Gomes, F. (1985). \*Curso de estatística experimental\*. Nobel,
São Paulo. (Clasificación del coeficiente de variación.)

Scott, A. J. y Knott, M. (1974). A cluster analysis method for grouping
means in the analysis of variance. \*Biometrics\* 30, 507-512.

de Mendiburu, F. (2023). \*agricolae: Statistical Procedures for
Agricultural Research\*. R package.

## Examples

``` r
# Completamente al azar: peso seco de plantas con dos tratamientos y un control
res <- inkagro(PlantGrowth, respuesta = "weight", tratamiento = "group",
               diseno = "dca")
res
#> 
#> InkAgro | Diseño completamente al azar (DCA)
#> ------------------------------------------------------------------
#> Respuesta: weight | group: 3 niveles | n = 30
#> 
#> Análisis de varianza (suma de cuadrados tipo I)
#>  Fuente GL     SC    CM    F     p  
#>   group  2  3.766 1.883 4.85 0.016 *
#>   Error 27 10.492 0.389             
#>   Total 29 14.258                   
#> Signif.: *** p<0.001  ** p<0.01  * p<0.05  ns no significativo
#> 
#> CV = 12.3% (medio) | Media general = 5.073
#> 
#> Conclusiones (alfa = 0.05)
#>   - Hay diferencias significativas entre los niveles de 'group' (F =
#>     4.85; p = 0.016).
#> 
#> Supuestos (sobre los residuos)
#>   Normalidad (Shapiro-Wilk): W = 0.966, p = 0.438 -> se cumple
#>   Homogeneidad de varianzas (Levene): F = 1.119, p = 0.341 -> se cumple
#> 
#> Comparación de medias (Tukey, alfa = 0.05)
#> 
#>   group
#>  Nivel n  Media EE    Grupo
#>  trt2  10 5.526 0.197 a    
#>  ctrl  10 5.032 0.197 ab   
#>  trt1  10 4.661 0.197 b    
#>   Letras distintas indican diferencias significativas. EE = error estándar de la media.
#> 
#> Valores atípicos (|residuo estandarizado| > 3): ninguno.
#> 

# Factorial 2 x 2 en bloques (nitrógeno x fósforo)
inkagro(npk, respuesta = "yield", tratamiento = "N", factor_b = "P",
        bloque = "block", diseno = "factorial")
#> 
#> InkAgro | Factorial en bloques completos al azar
#> ------------------------------------------------------------------
#> Respuesta: yield | N: 2 niveles | P: 2 niveles | Bloques: 6 | n = 24
#> 
#> Análisis de varianza (suma de cuadrados tipo I)
#>           Fuente GL      SC      CM    F     p   
#>  Bloques (block)  5 343.295  68.659 3.28 0.034  *
#>  N                1 189.282 189.282 9.04 0.009 **
#>  P                1   8.402   8.402 0.40 0.536 ns
#>  N x P            1  21.282  21.282 1.02 0.329 ns
#>  Error           15 314.105  20.940              
#>  Total           23 876.365                      
#> Signif.: *** p<0.001  ** p<0.01  * p<0.05  ns no significativo
#> 
#> CV = 8.3% (bajo) | Media general = 54.875
#> 
#> Conclusiones (alfa = 0.05)
#>   - Hay diferencias significativas entre los niveles de 'N' (F = 9.04; p
#>     = 0.009).
#>   - No se detectaron diferencias significativas entre los niveles de 'P'
#>     (F = 0.40; p = 0.536).
#>   - No se detectó interacción significativa N x P (F = 1.02; p = 0.329).
#> 
#> Supuestos (sobre los residuos)
#>   Normalidad (Shapiro-Wilk): W = 0.962, p = 0.489 -> se cumple
#>   Homogeneidad de varianzas (Levene): F = 0.331, p = 0.803 -> se cumple
#> 
#> Comparación de medias (Tukey, alfa = 0.05)
#> 
#>   N
#>  Nivel n  Media  EE    Grupo
#>  1     12 57.683 1.321 a    
#>  0     12 52.067 1.321 b    
#> 
#>   P  [efecto no significativo en el ANOVA]
#>  Nivel n  Media  EE    Grupo
#>  0     12 55.467 1.321 a    
#>  1     12 54.283 1.321 a    
#>   Letras distintas indican diferencias significativas. EE = error estándar de la media.
#> 
#> Valores atípicos (|residuo estandarizado| > 3): ninguno.
#> 

# Parcelas divididas: variedad en la parcela principal, dosis de N en la
# subparcela (Yates, 1935)
if (requireNamespace("nlme", quietly = TRUE)) {
  avena <- as.data.frame(nlme::Oats)
  inkagro(avena, respuesta = "yield", tratamiento = "Variety",
          factor_b = "nitro", bloque = "Block",
          diseno = "parcelas_divididas")
}
#> 
#> InkAgro | Parcelas divididas en bloques completos al azar
#> ------------------------------------------------------------------
#> Respuesta: yield | Variety: 3 niveles | nitro: 4 niveles | Bloques: 6 | n = 72
#> 
#> Análisis de varianza (suma de cuadrados tipo I)
#>           Fuente GL        SC       CM     F      p    
#>  Bloques (Block)  5 15875.278 3175.056  5.28  0.012   *
#>  Variety          2  1786.361  893.181  1.49  0.272  ns
#>  Error (a)       10  6013.306  601.331                 
#>  nitro            3 20020.500 6673.500 37.69 <0.001 ***
#>  Variety x nitro  6   321.750   53.625  0.30  0.932  ns
#>  Error (b)       45  7968.750  177.083                 
#>  Total           71 51985.944                          
#> Signif.: *** p<0.001  ** p<0.01  * p<0.05  ns no significativo
#> 
#> CV(a) = 23.6% (alto) | CV(b) = 12.8% (medio) | Media general = 103.972
#> 
#> Conclusiones (alfa = 0.05)
#>   - No se detectaron diferencias significativas entre los niveles de
#>     'Variety' (F = 1.49; p = 0.272).
#>   - Hay diferencias significativas entre los niveles de 'nitro' (F =
#>     37.69; p < 0.001).
#>   - No se detectó interacción significativa Variety x nitro (F = 0.30; p
#>     = 0.932).
#>   - Respuesta lineal a 'nitro': y = 81.87 + 73.67x (R² = 0.976).
#> 
#> Regresión para 'nitro' (factor cuantitativo)
#>  Componente GL        SC      F      p    
#>  Lineal      1 19536.400 110.32 <0.001 ***
#>  Cuadrático  1   480.500   2.71  0.106  ns
#>  Cúbico      1     3.600   0.02  0.887  ns
#> 
#> Supuestos (sobre los residuos)
#>   Normalidad (Shapiro-Wilk): W = 0.990, p = 0.836 -> se cumple
#>   Homogeneidad de varianzas (Levene): F = 1.005, p = 0.453 -> se cumple
#> 
#> Comparación de medias (Tukey, alfa = 0.05)
#> 
#>   Variety  [efecto no significativo en el ANOVA]
#>  Nivel       n  Media   EE    Grupo
#>  Marvellous  24 109.792 5.006 a    
#>  Golden Rain 24 104.500 5.006 a    
#>  Victory     24 97.625  5.006 a    
#> 
#>   nitro
#>  Nivel n  Media   EE    Grupo
#>  0.6   18 123.389 3.137 a    
#>  0.4   18 114.222 3.137 a    
#>  0.2   18 98.889  3.137 b    
#>  0     18 79.389  3.137 c    
#>   Letras distintas indican diferencias significativas. EE = error estándar de la media.
#> 
#> Valores atípicos (|residuo estandarizado| > 3): ninguno.
#> 
```
