# Supuestos del analisis de varianza y deteccion de valores atipicos.

.ink_supuestos <- function(modelo, d, codigo) {
  r <- stats::residuals(modelo)
  grupos <- if (codigo %in% c("factorial", "pd")) {
    interaction(d$A, d$B, drop = TRUE)
  } else {
    d$A
  }

  normalidad <- list(prueba = "Shapiro-Wilk", estadistico = NA_real_,
                     p = NA_real_, nota = "")
  if (length(r) < 3L || length(r) > 5000L) {
    normalidad$nota <- "no aplicable (se requieren entre 3 y 5000 observaciones)"
  } else if (stats::sd(r) < sqrt(.Machine$double.eps)) {
    normalidad$nota <- "no aplicable (residuos sin variaci\u00f3n)"
  } else {
    s <- stats::shapiro.test(r)
    normalidad$estadistico <- unname(s$statistic)
    normalidad$p <- s$p.value
  }

  homogeneidad <- list(prueba = "Levene", estadistico = NA_real_,
                       p = NA_real_, nota = "")
  if (any(table(grupos) < 2L)) {
    homogeneidad$nota <- "no aplicable (hay grupos con una sola observaci\u00f3n)"
  } else {
    lv <- suppressWarnings(car::leveneTest(r, grupos, center = stats::median))
    homogeneidad$estadistico <- lv[["F value"]][1L]
    homogeneidad$p <- lv[["Pr(>F)"]][1L]
  }

  list(normalidad = normalidad, homogeneidad = homogeneidad)
}

.ink_atipicos <- function(modelo, d, umbral = 3) {
  rs <- suppressWarnings(stats::rstandard(modelo))
  i <- which(is.finite(rs) & abs(rs) > umbral)
  data.frame(
    fila        = d$.fila[i],
    tratamiento = as.character(d$A[i]),
    valor       = d$y[i],
    residuo_est = round(unname(rs[i]), 2),
    stringsAsFactors = FALSE
  )
}
