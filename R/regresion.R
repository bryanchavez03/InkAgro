# Regresion para factores cuantitativos (dosis, densidades, dias).
# Descompone la suma de cuadrados del factor en contrastes polinomiales
# ortogonales (lineal, cuadratico, cubico) probados con el error que
# corresponde a ese factor, y ajusta la curva de mayor grado significativo
# (hasta cuadratico) sobre las medias.

.ink_es_cuantitativo <- function(f) {
  nlevels(f) >= 3L && !anyNA(suppressWarnings(as.numeric(levels(f))))
}

.ink_regresion <- function(y, f, err, alfa) {
  dosis  <- as.numeric(levels(f))
  k      <- length(dosis)
  n      <- as.integer(table(f))
  if (length(unique(n)) > 1L) {
    return(list(nota = "desbalanceado"))
  }
  medias <- as.numeric(tapply(y, f, mean))
  grados <- min(k - 1L, 3L)
  C  <- stats::contr.poly(k, scores = dosis)[, seq_len(grados), drop = FALSE]
  sc <- n[1L] * as.numeric(crossprod(C, medias))^2
  cm_e <- err[["cm"]]
  gl_e <- err[["gl"]]
  tabla <- data.frame(
    componente = c("Lineal", "Cuadr\u00e1tico", "C\u00fabico")[seq_len(grados)],
    gl = 1L, sc = sc, F = sc / cm_e,
    stringsAsFactors = FALSE)
  sc_total <- n[1L] * sum((medias - mean(medias))^2)
  if (k - 1L > grados) {
    gl_d <- k - 1L - grados
    tabla <- rbind(tabla, data.frame(
      componente = "Desviaci\u00f3n de la regresi\u00f3n", gl = gl_d,
      sc = sc_total - sum(sc), F = ((sc_total - sum(sc)) / gl_d) / cm_e))
  }
  tabla$p <- stats::pf(tabla$F, tabla$gl, gl_e, lower.tail = FALSE)
  tabla$sig <- .ink_signif(tabla$p)

  p_lin  <- tabla$p[1L]
  p_cuad <- if (grados >= 2L) tabla$p[2L] else NA_real_
  grado <- if (!is.na(p_cuad) && p_cuad < alfa) 2L else if (p_lin < alfa) 1L else 0L

  out <- list(tabla = tabla, grado = grado, dosis = dosis, medias = medias,
              ee = sqrt(cm_e / n), coef = NULL, r2 = NA_real_, optimo = NULL)
  if (grado > 0L) {
    ajuste <- stats::lm(medias ~ stats::poly(dosis, grado, raw = TRUE))
    out$coef <- unname(stats::coef(ajuste))
    out$r2 <- if (k > grado + 1L) summary(ajuste)$r.squared else 1
    if (grado == 2L && out$coef[3L] < 0) {
      x0 <- -out$coef[2L] / (2 * out$coef[3L])
      if (x0 > min(dosis) && x0 < max(dosis)) {
        out$optimo <- c(x = x0, y = sum(out$coef * x0^(0:2)))
      }
    }
  }
  out
}

.ink_ecuacion <- function(r, digitos = 3L) {
  if (is.null(r$coef)) return("")
  b <- r$coef
  termino <- function(v, x) {
    if (is.na(v)) return("")
    paste0(if (v < 0) " - " else " + ", formatC(abs(v), format = "fg", digits = digitos + 1L), x)
  }
  paste0("y = ", formatC(b[1L], format = "fg", digits = digitos + 1L),
         termino(b[2L], "x"),
         if (length(b) > 2L) termino(b[3L], "x\u00b2") else "")
}

.ink_texto_regresion <- function(nombre, r, alfa) {
  if (!is.null(r$nota)) {
    return(sprintf(paste0(
      "'%s' es cuantitativo, pero tiene distinto n\u00famero de repeticiones ",
      "por nivel; la regresi\u00f3n por contrastes requiere datos balanceados."),
      nombre))
  }
  if (r$grado == 0L) {
    return(sprintf("La respuesta a '%s' no muestra tendencia lineal ni cuadr\u00e1tica significativa.",
                   nombre))
  }
  base <- sprintf("Respuesta %s a '%s': %s (R\u00b2 = %s).",
                  if (r$grado == 1L) "lineal" else "cuadr\u00e1tica", nombre,
                  .ink_ecuacion(r), .ink_num(r$r2, 3L))
  if (!is.null(r$optimo)) {
    base <- paste0(base, sprintf(
      " M\u00e1ximo t\u00e9cnico estimado en x = %s (y = %s).",
      .ink_num(r$optimo[["x"]], 3L), .ink_num(r$optimo[["y"]], 2L)))
  } else if (r$grado == 2L) {
    base <- paste0(base, " El m\u00e1ximo no est\u00e1 dentro del rango evaluado.")
  }
  base
}
