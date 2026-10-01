# Verificacion estructural: comprueba que los datos correspondan al diseno
# declarado. Devuelve avisos (problemas tolerables) o se detiene con un
# mensaje explicativo (problemas que invalidarian el analisis).

.ink_verificar <- function(d, codigo, nm, otras) {
  av <- character()
  tiene_b   <- "B" %in% names(d)
  tiene_blq <- "blq" %in% names(d)

  .ink_min_niveles(d$A, nm$tratamiento, 2L, "tratamientos")
  if (tiene_b)   .ink_min_niveles(d$B, nm$factor_b, 2L, "niveles")
  if (tiene_blq) .ink_min_niveles(d$blq, nm$bloque, 2L, "bloques")

  # Celdas que, por diseno, deberian tener una sola observacion
  celda <- switch(
    codigo,
    dca       = NULL,
    dbca      = interaction(d$A, d$blq, drop = TRUE),
    factorial = if (tiene_blq) interaction(d$A, d$B, d$blq, drop = TRUE),
    pd        = interaction(d$A, d$B, d$blq, drop = TRUE)
  )
  otras_d <- otras[d$.fila, , drop = FALSE]
  # Unidad que se repite: sin bloques, el tratamiento (o la combinacion); con
  # bloques, la celda. Si una columna de localidad/ambiente/anio explica todas
  # las repeticiones, es un ensayo multiambiental: se detiene.
  unidad <- if (is.null(celda)) {
    if (tiene_b) interaction(d$A, d$B, drop = TRUE) else d$A
  } else {
    celda
  }
  if (any(table(unidad) > 1L)) {
    amb <- .ink_buscar_explicacion(unidad, otras_d, ambiente = TRUE)
    if (!is.null(amb)) {
      stop(sprintf(paste0(
        "Las repeticiones de cada tratamiento las explica la columna '%s' ",
        "(%d niveles): parece un ensayo en varias localidades, ambientes o ",
        "a\u00f1os, y tratarlos como repeticiones ser\u00eda un error. InkAgro 0.1 ",
        "analiza un ambiente a la vez. Filtra los datos, por ejemplo: ",
        "subset(datos, %s == \"%s\")."),
        amb$columna, amb$n, amb$columna, amb$primero), call. = FALSE)
    }
  }
  if (!is.null(celda) && any(table(celda) > 1L)) {
    if (codigo == "pd") {
      stop("Parcelas divididas requiere una sola observaci\u00f3n por bloque x '",
           nm$tratamiento, "' x '", nm$factor_b, "', y hay celdas repetidas.",
           call. = FALSE)
    }
    otro <- .ink_buscar_explicacion(celda, otras_d, ambiente = FALSE)
    if (!is.null(otro)) {
      av <- c(av, sprintf(paste0(
        "Cada tratamiento aparece varias veces por bloque y la columna '%s' ",
        "lo explica. Si '%s' es otro factor del experimento, anal\u00edzalo como ",
        "factorial (factor_b = \"%s\"); si es una localidad o ambiente, ",
        "analiza cada uno por separado."), otro$columna, otro$columna, otro$columna))
    } else {
      av <- c(av, paste0(
        "Hay m\u00e1s de una observaci\u00f3n por tratamiento dentro de un mismo bloque. ",
        "Si son submuestras de la misma parcela (no parcelas distintas), ",
        "prom\u00e9dialas antes del an\u00e1lisis: contarlas como repeticiones infla los ",
        "grados de libertad del error y produce diferencias falsas."))
    }
  }

  av <- c(av, switch(
    codigo,
    dca       = .ink_verificar_dca(d, nm, otras),
    dbca      = .ink_verificar_bloques(d$A, d$blq, nm$tratamiento, nm$bloque),
    factorial = .ink_verificar_factorial(d, nm, tiene_blq),
    pd        = .ink_verificar_pd(d, nm)
  ))
  av
}

.ink_min_niveles <- function(x, nombre, minimo, que) {
  if (nlevels(x) < minimo) {
    stop(sprintf("La columna '%s' tiene %d nivel(es); se necesitan al menos %d %s.",
                 nombre, nlevels(x), minimo, que), call. = FALSE)
  }
}

# Busca una columna no usada que explique las repeticiones dentro de las
# celdas. Con ambiente = TRUE solo considera columnas cuyo nombre indica
# localidad, ambiente o ano; con FALSE, cualquier otra columna.
.ink_pal_ambiente <- function() {
  unique(c(pal_ambiente, pal_year, "county", "localidad", "locality",
           "ambiente", "ambientes", "region", "provincia", "distrito", "zona",
           "municipio", "anio", "season", "temporada", "campana", "ensayo",
           "trial", "env_id", "site_id", "loc_id"))
}

.ink_buscar_explicacion <- function(celda, otras, ambiente) {
  es_amb <- tolower(trimws(names(otras))) %in% .ink_pal_ambiente()
  candidatas <- names(otras)[if (ambiente) es_amb else !es_amb]
  for (col in candidatas) {
    v <- otras[[col]]
    if (anyNA(v)) next
    n <- length(unique(v))
    if (n < 2L || n > length(v) / 2) next
    if (max(table(celda, v)) == 1L) {
      return(list(columna = col, n = n,
                  primero = as.character(sort(unique(v))[1L])))
    }
  }
  NULL
}

.ink_verificar_dca <- function(d, nm, otras) {
  n <- table(d$A)
  if (any(n < 2L)) {
    stop(sprintf(paste0(
      "Los tratamientos %s tienen una sola observaci\u00f3n. Sin repeticiones no ",
      "se puede estimar el error experimental. \u00bf'%s' es la columna correcta?"),
      .ink_lista(names(n)[n < 2L]), nm$tratamiento), call. = FALSE)
  }
  av <- character()
  if (length(unique(as.integer(n))) > 1L) {
    av <- c(av, sprintf("Dise\u00f1o desbalanceado: entre %d y %d repeticiones por tratamiento.",
                        min(n), max(n)))
  }
  # Hay una columna que parece de bloques y se esta ignorando?
  sospechosas <- names(otras)[tolower(trimws(names(otras))) %in% pal_bloque]
  for (col in sospechosas) {
    v <- otras[[col]][d$.fila]
    k <- length(unique(stats::na.omit(v)))
    if (k >= 2L && k <= nrow(d) / 2 &&
        all(rowSums(table(d$A, v) > 0) >= 2L)) {
      av <- c(av, sprintf(paste0(
        "La columna '%s' parece de bloques o repeticiones. Si el experimento ",
        "se instal\u00f3 en bloques, usa diseno = \"dbca\", bloque = \"%s\": ",
        "ignorarlos suma su variaci\u00f3n al error y resta sensibilidad."), col, col))
      break
    }
  }
  av
}

.ink_verificar_bloques <- function(trat, blq, nombre_trat, nombre_blq) {
  tab <- table(trat, blq)
  if (all(rowSums(tab > 0) == 1L)) {
    stop(sprintf(paste0(
      "Cada nivel de '%s' aparece en un solo bloque, as\u00ed que '%s' no funciona ",
      "como bloque (cada bloque debe contener todos los tratamientos). ",
      "\u00bfEs un DCA, o los bloques est\u00e1n en otra columna?"),
      nombre_trat, nombre_blq), call. = FALSE)
  }
  vacias <- sum(tab == 0L)
  if (vacias == 0L) return(character())
  prop <- vacias / length(tab)
  if (prop > 0.25) {
    stop(sprintf(paste0(
      "Faltan %d de %d combinaciones %s x bloque (%.0f%%). Con tantos huecos ",
      "no es un dise\u00f1o de bloques completos; si los bloques son incompletos ",
      "por dise\u00f1o (l\u00e1tice, alfa), InkAgro 0.1 a\u00fan no lo soporta."),
      vacias, length(tab), nombre_trat, 100 * prop), call. = FALSE)
  }
  sprintf(paste0(
    "Faltan %d combinaci\u00f3n(es) %s x bloque (parcelas perdidas). Se usa suma de ",
    "cuadrados tipo II (efectos ajustados por bloque); las medias mostradas ",
    "son aritm\u00e9ticas."), vacias, nombre_trat)
}

.ink_verificar_factorial <- function(d, nm, tiene_blq) {
  tab <- table(d$A, d$B)
  falt <- which(tab == 0L, arr.ind = TRUE)
  if (nrow(falt) > 0L) {
    combos <- paste0(rownames(tab)[falt[, 1L]], " x ", colnames(tab)[falt[, 2L]])
    stop(sprintf(paste0(
      "Factorial incompleto: faltan las combinaciones %s. Un factorial ",
      "necesita todas las combinaciones de '%s' y '%s'."),
      .ink_lista(combos), nm$tratamiento, nm$factor_b), call. = FALSE)
  }
  if (tiene_blq) {
    return(.ink_verificar_bloques(interaction(d$A, d$B, sep = " x "), d$blq,
                                  paste(nm$tratamiento, "x", nm$factor_b),
                                  nm$bloque))
  }
  if (any(tab < 2L)) {
    stop(sprintf(paste0(
      "Algunas combinaciones de '%s' x '%s' tienen una sola observaci\u00f3n; ",
      "sin repeticiones no se puede estimar el error del factorial."),
      nm$tratamiento, nm$factor_b), call. = FALSE)
  }
  if (length(unique(as.integer(tab))) > 1L) {
    return(sprintf("Factorial desbalanceado: entre %d y %d repeticiones por combinaci\u00f3n; se usa suma de cuadrados tipo II.",
                   min(tab), max(tab)))
  }
  character()
}

.ink_verificar_pd <- function(d, nm) {
  tab <- table(d$A, d$B, d$blq)
  if (any(tab != 1L)) {
    stop(sprintf(paste0(
      "InkAgro 0.1 analiza parcelas divididas solo con datos balanceados: una ",
      "observaci\u00f3n por cada bloque x '%s' x '%s'. Faltan %d."),
      nm$tratamiento, nm$factor_b, sum(tab == 0L)), call. = FALSE)
  }
  character()
}
