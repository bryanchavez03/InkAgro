# InkAgro

Análisis de experimentos agrícolas en una sola línea, con reporte en español.

```r
# install.packages("remotes")
remotes::install_github("bryanchavez03/InkAgro")

library(InkAgro)
res <- inkagro("mi_ensayo.xlsx", respuesta = "rendimiento",
               tratamiento = "variedad", bloque = "bloque",
               diseno = "dbca")
res                                   # reporte completo
plot(res)                             # medias, error estándar y letras
as.data.frame(res, tabla = "medias")  # tabla para la tesis
```

InkAgro verifica que los datos correspondan al diseño declarado (DCA, DBCA,
factorial o parcelas divididas) y se detiene con una explicación si no es
así: varias localidades mezcladas, bloques que no contienen los
tratamientos, factoriales incompletos. Luego ajusta el análisis de varianza,
evalúa los supuestos, marca los valores atípicos (no los borra) y compara
medias con Tukey, Duncan, LSD o SNK (vía 'agricolae').
