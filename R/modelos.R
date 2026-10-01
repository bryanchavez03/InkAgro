# Ajuste del modelo, tabla de analisis de varianza y comparacion de medias.

.ink_ajustar <- function(d, codigo, nm, prueba, alfa) {
  tiene_blq <- "blq" %in% names(d)
  formula <- switch(
    codigo,
    dca       = y ~ A,
    dbca      = y ~ blq + A,
    factorial = if (tiene_blq) y ~ blq + A * B else y ~ A * B,
    pd        = y ~ blq + A + blq:A + B + A:B
  )
  modelo <- stats::lm(formula, data = d)
  if (modelo$df.residual < 1L) {
    stop("No quedan grados de libertad para estimar el error: faltan ",
         "repeticiones.", call. = FALSE)
  }

  balanceado <- switch(
    codigo,
    dca       = TRUE,
    dbca      = length(unique(as.integer(table(d$A, d$blq)))) == 1L,
    factorial = if (tiene_blq) {
      length(unique(as.integer(table(d$A, d$B, d$blq)))) == 1L
    } else {
      length(unique(as.integer(table(d$A, d$B)))) == 1L
    },
    pd        = TRUE
  )

  if (balanceado) {
    tab <- stats::anova(modelo)
    tipo_sc <- "I"
  } else {
    tab <- car::Anova(modelo, type = 2)
    tab[["Mean Sq"]] <- tab[["Sum Sq"]] / tab[["Df"]]
    tipo_sc <- "II"
  }
  tab <- data.frame(
    termino = rownames(tab),
    gl      = tab[["Df"]],
    sc      = tab[["Sum Sq"]],
    cm      = tab[["Mean Sq"]],
    F       = tab[["F value"]],
    p       = tab[["Pr(>F)"]],
    stringsAsFactors = FALSE
  )

  error_res <- c(gl = modelo$df.residual,
                 cm = sum(stats::residuals(modelo)^2) / modelo$df.residual)
  errores <- list(A = error_res, B = error_res, AB = error_res)
  cv <- c(general = 100 * sqrt(error_res[["cm"]]) / mean(d$y))

  if (codigo == "pd") {
    fila_a <- which(tab$termino == "blq:A")
    error_a <- c(gl = tab$gl[fila_a], cm = tab$cm[fila_a])
    for (t in c("blq", "A")) {
      i <- which(tab$termino == t)
      tab$F[i] <- tab$cm[i] / error_a[["cm"]]
      tab$p[i] <- stats::pf(tab$F[i], tab$gl[i], error_a[["gl"]],
                            lower.tail = FALSE)
    }
    tab$F[fila_a] <- NA
    tab$p[fila_a] <- NA
    errores$A <- error_a
    cv <- c(a = 100 * sqrt(error_a[["cm"]]) / mean(d$y),
            b = cv[["general"]])
  }

  if (codigo == "pd") {
    orden <- c("blq", "A", "blq:A", "B", "A:B", "Residuals")
    tab <- tab[match(orden, tab$termino), ]
  }
  tab <- rbind(tab, data.frame(
    termino = "Total", gl = nrow(d) - 1L,
    sc = sum((d$y - mean(d$y))^2), cm = NA, F = NA, p = NA))
  tab$fuente <- .ink_etiquetas(tab$termino, nm, codigo)
  tab$sig <- .ink_signif(tab$p)
  tab <- tab[, c("fuente", "gl", "sc", "cm", "F", "p", "sig", "termino")]
  rownames(tab) <- NULL

  p_de <- function(t) {
    v <- tab$p[tab$termino == t]
    if (length(v)) v else NA_real_
  }

  medias <- list()
  avisos <- character()
  comparar <- function(y, g, err) {
    .ink_comparar(y, g, err[["gl"]], err[["cm"]], prueba, alfa)
  }
  if (codigo %in% c("dca", "dbca")) {
    medias[[nm$tratamiento]] <- .ink_marcar(comparar(d$y, d$A, errores$A),
                                            "A", p_de("A"))
  } else {
    medias[[nm$tratamiento]] <- .ink_marcar(comparar(d$y, d$A, errores$A),
                                            "A", p_de("A"))
    medias[[nm$factor_b]] <- .ink_marcar(comparar(d$y, d$B, errores$B),
                                         "B", p_de("B"))
    p_ab <- p_de("A:B")
    if (!is.na(p_ab) && p_ab < alfa) {
      avisos <- c(avisos, sprintf(paste0(
        "La interacci\u00f3n %s x %s es significativa: el efecto de un factor ",
        "depende del nivel del otro. Interpreta las comparaciones de la ",
        "interacci\u00f3n, no las de cada factor por separado."),
        nm$tratamiento, nm$factor_b))
      if (codigo == "factorial") {
        combo <- interaction(d$A, d$B, sep = " : ", lex.order = TRUE)
        medias[[paste(nm$tratamiento, "x", nm$factor_b)]] <-
          .ink_marcar(comparar(d$y, combo, errores$AB), "AB", p_ab)
      } else {
        for (a in levels(d$A)) {
          sel <- d$A == a
          etiqueta <- sprintf("%s dentro de %s = %s", nm$factor_b, nm$tratamiento, a)
          medias[[etiqueta]] <- .ink_marcar(
            comparar(d$y[sel], droplevels(d$B[sel]), errores$B), "B|A", p_ab)
        }
        avisos <- c(avisos, sprintf(paste0(
          "En parcelas divididas, la comparaci\u00f3n de '%s' dentro de cada nivel ",
          "de '%s' requiere un error combinado que InkAgro 0.1 a\u00fan no calcula; ",
          "solo se muestra '%s' dentro de '%s'."),
          nm$tratamiento, nm$factor_b, nm$factor_b, nm$tratamiento))
      }
    }
  }

  list(modelo = modelo, anova = tab, tipo_sc = tipo_sc, errores = errores,
       medias = medias, cv = cv, avisos = avisos)
}

.ink_marcar <- function(tabla, factor, p) {
  attr(tabla, "factor") <- factor
  attr(tabla, "p") <- p
  tabla
}

.ink_etiquetas <- function(termino, nm, codigo) {
  mapa <- c(
    blq       = if (is.null(nm$bloque) || tolower(nm$bloque) %in% c("bloque", "bloques"))
                  "Bloques" else paste0("Bloques (", nm$bloque, ")"),
    A         = nm$tratamiento,
    B         = if (is.null(nm$factor_b)) "B" else nm$factor_b,
    "A:B"     = paste(nm$tratamiento, "x", if (is.null(nm$factor_b)) "B" else nm$factor_b),
    "blq:A"   = "Error (a)",
    Residuals = if (codigo == "pd") "Error (b)" else "Error",
    Total     = "Total"
  )
  out <- unname(mapa[termino])
  out[is.na(out)] <- termino[is.na(out)]
  out
}

.ink_comparar <- function(y, g, gl, cm, prueba, alfa) {
  yy <- as.numeric(y)
  tt <- as.character(g)
  res <- switch(
    prueba,
    tukey  = agricolae::HSD.test(yy, tt, DFerror = gl, MSerror = cm,
                                 alpha = alfa, group = TRUE,
                                 unbalanced = TRUE, console = FALSE),
    duncan = agricolae::duncan.test(yy, tt, DFerror = gl, MSerror = cm,
                                    alpha = alfa, group = TRUE, console = FALSE),
    lsd    = agricolae::LSD.test(yy, tt, DFerror = gl, MSerror = cm,
                                 alpha = alfa, group = TRUE, console = FALSE),
    snk    = agricolae::SNK.test(yy, tt, DFerror = gl, MSerror = cm,
                                 alpha = alfa, group = TRUE, console = FALSE)
  )
  grupos <- res$groups
  niveles <- rownames(grupos)
  n <- as.integer(table(tt)[niveles])
  out <- data.frame(
    nivel = niveles,
    n     = n,
    media = grupos[[1L]],
    ee    = sqrt(cm / n),
    grupo = trimws(as.character(grupos$groups)),
    stringsAsFactors = FALSE
  )
  orden <- if (is.factor(g)) levels(droplevels(g)) else sort(unique(tt))
  attr(out, "orden_niveles") <- orden
  rownames(out) <- NULL
  out
}
