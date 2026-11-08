# Medias ajustadas (medias de minimos cuadrados) y su comparacion, para
# disenos con parcelas perdidas. Con datos desbalanceados la media aritmetica
# de un tratamiento depende de que parcelas faltan (si falta una parcela de
# un bloque o de una dosis baja, la media sube o baja sin que el tratamiento
# sea distinto). La media ajustada promedia las predicciones del modelo sobre
# todos los bloques y niveles del otro factor con el mismo peso, igual que
# emmeans::emmeans(). Las comparaciones usan el error estandar de cada
# diferencia, calculado con la matriz de covarianzas del modelo.

.ink_comparar_ajustado <- function(modelo, d, termino, prueba, alfa) {
  beta <- stats::coef(modelo)
  if (anyNA(beta)) return(NULL)  # alguna combinacion no estimable

  factores <- intersect(c("blq", "A", "B"), names(d))
  rejilla <- expand.grid(lapply(d[factores], levels), KEEP.OUT.ATTRS = FALSE)
  for (f in factores) rejilla[[f]] <- factor(rejilla[[f]], levels = levels(d[[f]]))
  X <- stats::model.matrix(stats::delete.response(stats::terms(modelo)), rejilla,
                           contrasts.arg = modelo$contrasts)

  clave_de <- function(z) {
    if (termino == "AB") interaction(z$A, z$B, sep = " : ", lex.order = TRUE) else z[[termino]]
  }
  niveles <- levels(droplevels(clave_de(d)))
  clave <- as.character(clave_de(rejilla))

  L <- t(vapply(niveles, function(v) colMeans(X[clave == v, , drop = FALSE]),
                numeric(ncol(X))))
  medias <- as.numeric(L %*% beta)
  V <- L %*% stats::vcov(modelo) %*% t(L)
  ee <- sqrt(diag(V))
  gl <- modelo$df.residual
  k <- length(niveles)

  # Diferencias significativas entre cada par de medias
  signif <- matrix(FALSE, k, k)
  for (i in seq_len(k - 1L)) {
    for (j in (i + 1L):k) {
      t_ij <- (medias[i] - medias[j]) / sqrt(V[i, i] + V[j, j] - 2 * V[i, j])
      p <- if (prueba == "tukey") {
        stats::ptukey(abs(t_ij) * sqrt(2), nmeans = k, df = gl, lower.tail = FALSE)
      } else {
        2 * stats::pt(-abs(t_ij), df = gl)
      }
      signif[i, j] <- signif[j, i] <- p < alfa
    }
  }

  grupo <- .ink_letras_cld(medias, signif)
  n <- as.integer(table(factor(as.character(clave_de(d)), levels = niveles)))
  out <- data.frame(nivel = niveles, n = n, media = medias, ee = ee, grupo = grupo,
                    stringsAsFactors = FALSE)
  out <- out[order(-out$media), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "orden_niveles") <- niveles
  attr(out, "ajustadas") <- TRUE
  out
}

# Letras de comparacion (algoritmo "insertar y absorber", Piepho 2004):
# medias que comparten al menos una letra no difieren significativamente.
# La letra "a" corresponde a la media mas alta.
.ink_letras_cld <- function(medias, signif) {
  k <- length(medias)
  columnas <- list(rep(TRUE, k))
  pares <- which(upper.tri(signif) & signif, arr.ind = TRUE)
  for (r in seq_len(nrow(pares))) {
    i <- pares[r, 1L]
    j <- pares[r, 2L]
    nuevas <- list()
    for (col in columnas) {
      if (col[i] && col[j]) {
        c1 <- col
        c1[i] <- FALSE
        c2 <- col
        c2[j] <- FALSE
        nuevas <- c(nuevas, list(c1, c2))
      } else {
        nuevas <- c(nuevas, list(col))
      }
    }
    # Absorber: quitar columnas vacias, repetidas o contenidas en otra
    nuevas <- nuevas[vapply(nuevas, any, logical(1))]
    conservar <- rep(TRUE, length(nuevas))
    for (a in seq_along(nuevas)) {
      for (b in seq_along(nuevas)) {
        if (a == b || !conservar[b]) next
        if (all(nuevas[[a]] <= nuevas[[b]]) && (any(nuevas[[b]] > nuevas[[a]]) || a > b)) {
          conservar[a] <- FALSE
          break
        }
      }
    }
    columnas <- nuevas[conservar]
  }
  # Ordenar las letras segun la media mas alta de cada grupo
  orden <- order(-medias)
  primera <- vapply(columnas, function(col) min(match(which(col), orden)), numeric(1))
  columnas <- columnas[order(primera)]
  letras <- .ink_letras(seq_along(columnas))
  vapply(seq_len(k), function(i) {
    paste(letras[vapply(columnas, function(col) col[i], logical(1))], collapse = "")
  }, character(1))
}
