# Prueba de Scott-Knott y dendrograma de medias.
#
# Scott, A. J. y Knott, M. (1974). A cluster analysis method for grouping
# means in the analysis of variance. Biometrics 30, 507-512.
#
# Divide las medias ordenadas en dos grupos maximizando la suma de cuadrados
# entre grupos, prueba la division con el estadistico lambda (chi-cuadrado
# con k / (pi - 2) grados de libertad) y repite en cada grupo hasta que ya no
# es significativa. Los grupos resultantes no se superponen.

.ink_scott_knott <- function(y, g, gl, cm, alfa) {
  g <- droplevels(as.factor(g))
  medias <- tapply(y, g, mean)
  n <- as.integer(table(g))
  o <- order(medias, decreasing = TRUE)
  m <- as.numeric(medias)[o]
  ee2 <- (cm / n)[o]
  grupo <- integer(length(m))
  siguiente <- 0L

  # Devuelve el nodo del arbol: una division significativa (con su suma de
  # cuadrados entre grupos) o un grupo final.
  partir <- function(idx) {
    k <- length(idx)
    if (k == 1L) {
      siguiente <<- siguiente + 1L
      grupo[idx] <<- siguiente
      return(list(tipo = "grupo", idx = idx))
    }
    x <- m[idx]
    total <- sum(x)
    b <- vapply(seq_len(k - 1L), function(j) {
      t1 <- sum(x[seq_len(j)])
      t1^2 / j + (total - t1)^2 / (k - j) - total^2 / k
    }, numeric(1))
    j <- which.max(b)
    s2c <- mean(ee2[idx])
    si02 <- (sum((x - mean(x))^2) + gl * s2c) / (k + gl)
    lambda <- (pi / (2 * (pi - 2))) * b[j] / si02
    if (lambda > stats::qchisq(alfa, k / (pi - 2), lower.tail = FALSE)) {
      return(list(tipo = "division", altura = b[j], lambda = lambda,
                  hijos = list(partir(idx[seq_len(j)]), partir(idx[(j + 1L):k]))))
    }
    siguiente <<- siguiente + 1L
    grupo[idx] <<- siguiente
    list(tipo = "grupo", idx = idx)
  }
  arbol <- partir(seq_along(m))

  out <- data.frame(
    nivel = names(medias)[o],
    n     = n[o],
    media = m,
    ee    = sqrt(ee2),
    grupo = .ink_letras(grupo),
    stringsAsFactors = FALSE
  )
  attr(out, "orden_niveles") <- levels(g)
  attr(out, "arbol") <- arbol
  rownames(out) <- NULL
  out
}

# 1, 2, ..., 26, 27 -> a, b, ..., z, aa, ab ...
.ink_letras <- function(i) {
  vapply(i, function(v) {
    s <- character()
    while (v > 0) {
      r <- (v - 1L) %% 26L
      s <- c(letters[r + 1L], s)
      v <- (v - 1L) %/% 26L
    }
    paste(s, collapse = "")
  }, character(1))
}

# Segmentos del arbol de Scott-Knott, con las hojas en el eje vertical
# (media mas alta arriba). La altura de cada division es la suma de cuadrados
# entre los dos grupos que separa.
.ink_segmentos_sk <- function(arbol, k) {
  y_de <- function(i) k - i + 1
  alturas <- numeric()
  recoger <- function(n) if (n$tipo == "division") {
    alturas <<- c(alturas, n$altura); lapply(n$hijos, recoger)
  }
  recoger(arbol)
  h_grupo <- if (length(alturas)) 0.04 * max(alturas) else 1
  segs <- list()
  agregar <- function(x, xend, y, yend) {
    segs[[length(segs) + 1L]] <<- data.frame(x = x, xend = xend, y = y, yend = yend)
  }
  dibujar <- function(n) {
    if (n$tipo == "grupo") {
      ys <- y_de(n$idx)
      for (yy in ys) agregar(0, h_grupo, yy, yy)
      if (length(ys) > 1L) agregar(h_grupo, h_grupo, min(ys), max(ys))
      return(c(mean(ys), h_grupo))
    }
    c1 <- dibujar(n$hijos[[1L]])
    c2 <- dibujar(n$hijos[[2L]])
    h <- max(n$altura, 1.15 * max(c1[2L], c2[2L]))
    agregar(c1[2L], h, c1[1L], c1[1L])
    agregar(c2[2L], h, c2[1L], c2[1L])
    agregar(h, h, c1[1L], c2[1L])
    c((c1[1L] + c2[1L]) / 2, h)
  }
  raiz <- dibujar(arbol)
  list(segmentos = do.call(rbind, segs), alto = raiz[2L],
       divisiones = length(alturas))
}

.ink_graf_dendrograma <- function(x, m, borde) {
  if (nrow(m) < 3L) {
    stop("El dendrograma necesita al menos 3 niveles.", call. = FALSE)
  }
  arbol <- attr(m, "arbol")
  if (is.null(arbol)) {
    # La comparacion se hizo con otra prueba: se calcula Scott-Knott solo
    # para el grafico, con el mismo error.
    fac <- attr(m, "factor")
    if (!fac %in% c("A", "B", "AB")) {
      stop("El dendrograma no est\u00e1 disponible para comparaciones dentro de ",
           "cada nivel de otro factor.", call. = FALSE)
    }
    d <- x$datos
    g <- switch(fac, A = d$A, B = d$B,
                AB = interaction(d$A, d$B, sep = " : ", lex.order = TRUE))
    err <- x$errores[[fac]]
    m <- .ink_scott_knott(d$y, g, err[["gl"]], err[["cm"]], x$alfa)
    arbol <- attr(m, "arbol")
  }
  k <- nrow(m)
  dd <- .ink_segmentos_sk(arbol, k)
  hojas <- data.frame(y = k - seq_len(k) + 1,
                      etiqueta = paste0(m$nivel, "  ", m$grupo, "  (", .ink_num(m$media, 1L), ")"))
  tamano_letra <- if (k > 30L) 2.4 else if (k > 15L) 3 else 3.6
  ggplot2::ggplot(dd$segmentos) +
    ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          colour = borde, linewidth = 0.4) +
    ggplot2::geom_text(data = hojas, ggplot2::aes(x = 0, y = y, label = etiqueta),
                       hjust = -0.05, family = "serif", size = tamano_letra) +
    ggplot2::scale_x_reverse(breaks = if (dd$divisiones) pretty(c(0, dd$alto)) else NULL,
                             expand = ggplot2::expansion(mult = c(0.02, 0.6))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::labs(
      x = if (dd$divisiones) "Separaci\u00f3n entre grupos (suma de cuadrados entre medias)" else NULL,
      y = NULL,
      caption = paste(strwrap(paste0(
        if (dd$divisiones) "Cada rama es una divisi\u00f3n significativa de Scott-Knott"
        else "Scott-Knott no encontr\u00f3 divisiones significativas: un solo grupo",
        " (alfa = ", x$alfa, "). Entre par\u00e9ntesis, la media de ",
        x$nombres$respuesta, "."), width = 95), collapse = "\n"))
}
