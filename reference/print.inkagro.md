# Reporte de un análisis InkAgro

Imprime el reporte del análisis: diseño, avisos, análisis de varianza,
coeficiente de variación, conclusiones, supuestos, comparación de medias
y valores atípicos.

## Usage

``` r
# S3 method for class 'inkagro'
print(x, max_niveles = 20, ...)

# S3 method for class 'inkagro'
summary(object, ...)
```

## Arguments

- x, object:

  Objeto devuelto por \[inkagro()\].

- max_niveles:

  Número máximo de niveles a mostrar en cada tabla de medias. Las tablas
  completas se obtienen con \[as.data.frame.inkagro()\].

- ...:

  No se usa.

## Value

\`x\`, de forma invisible.

## Examples

``` r
res <- inkagro(PlantGrowth, "weight", "group", diseno = "dca")
print(res)
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
summary(res)
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
```
