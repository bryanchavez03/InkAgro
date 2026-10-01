#' Gráficos de un análisis InkAgro
#'
#' Grafica los resultados con un estilo listo para publicar: escala de
#' grises, fuente con serifa, sin cuadrícula y con las letras de la
#' comparación de medias. Devuelve un objeto 'ggplot2' que se puede seguir
#' modificando.
#'
#' @param x Objeto devuelto por [inkagro()].
#' @param comparacion Nombre o número de la comparación a graficar (ver
#'   `names(x$medias)`). Por defecto, la interacción si fue significativa;
#'   si no, el primer factor. Con `tipo = "regresion"`, nombre del factor
#'   cuantitativo (ver `names(x$regresion)`).
#' @param tipo Uno de:
#'   \describe{
#'     \item{`"barras"`}{Medias, error estándar y letras de la comparación.}
#'     \item{`"puntos"`}{Medias con intervalo de confianza y letras; lo
#'       prefieren muchas revistas porque no esconde la escala.}
#'     \item{`"cajas"`}{Distribución de los datos con las letras.}
#'     \item{`"interaccion"`}{Medias de cada combinación (factorial y
#'       parcelas divididas).}
#'     \item{`"regresion"`}{Curva dosis-respuesta para factores
#'       cuantitativos, con ecuación y R²; marca el máximo técnico si lo hay.}
#'     \item{`"residuos"`}{Diagnóstico: gráfico cuantil-cuantil y residuos
#'       frente a valores ajustados.}
#'     \item{`"dendrograma"`}{Árbol de Scott-Knott: cada rama es una división
#'       significativa de las medias y las hojas llevan su grupo. Útil con
#'       muchos tratamientos. Si el análisis usó otra prueba, el árbol se
#'       calcula con Scott-Knott solo para el gráfico.}
#'   }
#' @param estilo `"articulo"` (escala de grises, por defecto) o `"color"`.
#' @param color_barras,color_relleno Colores de borde y relleno cuando
#'   `estilo = "color"`.
#' @param fuente Familia tipográfica: `"serif"` (Times en la mayoría de
#'   sistemas, por defecto), `"sans"` o el nombre de una fuente instalada.
#' @param tamano Tamaño base del texto en puntos.
#' @param archivo Ruta opcional para guardar el gráfico (`.png`, `.pdf`,
#'   `.tiff`, `.jpg`). Si es `NULL` no se guarda nada.
#' @param ancho,alto Tamaño en centímetros al guardar. Por defecto 16 x 10,
#'   el ancho de una columna de página A4 con márgenes.
#' @param resolucion Resolución en puntos por pulgada al guardar.
#' @param ... No se usa.
#' @return Un objeto `ggplot`.
#' @examples
#' res <- inkagro(npk, "yield", "N", factor_b = "P", bloque = "block",
#'                diseno = "factorial")
#' plot(res)
#' plot(res, tipo = "interaccion")
#' plot(res, comparacion = "P", tipo = "cajas")
#' plot(res, tipo = "puntos")
#' plot(res, tipo = "residuos")
#'
#' # Factor cuantitativo: curva dosis-respuesta
#' if (requireNamespace("nlme", quietly = TRUE)) {
#'   avena <- inkagro(as.data.frame(nlme::Oats), "yield", "Variety",
#'                    factor_b = "nitro", bloque = "Block",
#'                    diseno = "parcelas_divididas")
#'   plot(avena, tipo = "regresion")
#'   plot(avena, comparacion = "nitro", tipo = "dendrograma")
#' }
#' @export
plot.inkagro <- function(x, comparacion = NULL,
                         tipo = c("barras", "puntos", "cajas", "interaccion",
                                  "regresion", "residuos", "dendrograma"),
                         estilo = c("articulo", "color"),
                         color_barras = "#2E75B6", color_relleno = "#BDD7EE",
                         fuente = "serif", tamano = 12,
                         archivo = NULL, ancho = 16, alto = 10,
                         resolucion = 600, ...) {
  tipo   <- match.arg(tipo)
  estilo <- match.arg(estilo)
  if (estilo == "articulo") {
    borde <- "black"
    relleno <- "grey80"
  } else {
    borde <- color_barras
    relleno <- color_relleno
  }

  g <- switch(
    tipo,
    interaccion = .ink_graf_interaccion(x, borde, estilo),
    regresion   = .ink_graf_regresion(x, comparacion, borde),
    residuos    = .ink_graf_residuos(x, borde),
    dendrograma = .ink_graf_dendrograma(x, .ink_elegir_comparacion(x, comparacion), borde),
    {
      m <- .ink_elegir_comparacion(x, comparacion)
      switch(tipo,
             barras = .ink_graf_barras(x, m, borde, relleno),
             puntos = .ink_graf_puntos(x, m, borde),
             cajas  = .ink_graf_cajas(x, m, borde, relleno))
    }
  )

  g <- g + .ink_tema(fuente, tamano)
  if (tipo == "dendrograma") {
    g <- g + ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                            axis.ticks.y = ggplot2::element_blank())
  }
  if (tipo == "residuos") {
    g <- g + ggplot2::theme(strip.background = ggplot2::element_blank(),
                            strip.text = ggplot2::element_text(face = "bold"))
  }
  if (tipo %in% c("barras", "puntos", "cajas") && nlevels(g$data$nivel) > 8L &&
      nlevels(g$data$nivel) <= 15L) {
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  if (!is.null(archivo)) {
    ggplot2::ggsave(archivo, g, width = ancho, height = alto, units = "cm",
                    dpi = resolucion)
  }
  g
}

.ink_tema <- function(fuente, tamano) {
  ggplot2::theme_classic(base_size = tamano, base_family = fuente) +
    ggplot2::theme(
      axis.text        = ggplot2::element_text(colour = "black"),
      axis.line        = ggplot2::element_line(colour = "black", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(colour = "black", linewidth = 0.4),
      legend.position  = "top",
      legend.key.width = ggplot2::unit(1.5, "lines"),
      plot.caption     = ggplot2::element_text(hjust = 0, size = ggplot2::rel(0.75))
    )
}

.ink_elegir_comparacion <- function(x, comparacion) {
  if (is.null(comparacion)) {
    factores <- vapply(x$medias, function(m) attr(m, "factor"), character(1))
    inter <- which(factores == "AB")
    comparacion <- if (length(inter)) inter[1L] else 1L
  }
  m <- x$medias[[comparacion]]
  if (is.null(m)) {
    stop("No existe la comparaci\u00f3n '", comparacion, "'. Opciones: ",
         paste(names(x$medias), collapse = ", "), call. = FALSE)
  }
  attr(m, "titulo") <- if (is.numeric(comparacion)) names(x$medias)[comparacion] else comparacion
  m$nivel <- factor(m$nivel, levels = attr(m, "orden_niveles"))
  m
}

.ink_graf_barras <- function(x, m, borde, relleno) {
  m$tope <- m$media + m$ee
  if (nrow(m) > 15L) {
    # Muchos niveles (genotipos): barras horizontales ordenadas por la media,
    # para que nombres y letras se lean.
    m$nivel <- factor(as.character(m$nivel), levels = m$nivel[order(m$media)])
    return(
      ggplot2::ggplot(m, ggplot2::aes(x = media, y = nivel)) +
        ggplot2::geom_col(fill = relleno, colour = borde, width = 0.7, linewidth = 0.3) +
        ggplot2::geom_errorbar(ggplot2::aes(xmin = media - ee, xmax = media + ee),
                               width = 0.3, colour = borde, linewidth = 0.3) +
        ggplot2::geom_text(ggplot2::aes(x = tope, label = grupo), hjust = -0.25,
                           family = "serif", size = 2.8) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(y = attr(m, "titulo"), x = x$nombres$respuesta)
    )
  }
  ggplot2::ggplot(m, ggplot2::aes(x = nivel, y = media)) +
    ggplot2::geom_col(fill = relleno, colour = borde, width = 0.6, linewidth = 0.4) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = media - ee, ymax = media + ee),
                           width = 0.15, colour = borde, linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(y = tope, label = grupo), vjust = -0.7,
                       family = "serif", size = 3.8) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = attr(m, "titulo"), y = x$nombres$respuesta)
}

.ink_graf_cajas <- function(x, m, borde, relleno) {
  fac <- attr(m, "factor")
  if (!fac %in% c("A", "B")) {
    stop("El gr\u00e1fico de cajas solo est\u00e1 disponible para los factores ",
         "principales.", call. = FALSE)
  }
  datos <- data.frame(nivel = factor(as.character(x$datos[[fac]]),
                                     levels = levels(m$nivel)),
                      valor = x$datos$y)
  tope <- stats::aggregate(valor ~ nivel, datos, max)
  m$tope <- tope$valor[match(m$nivel, tope$nivel)]
  ggplot2::ggplot(datos, ggplot2::aes(x = nivel, y = valor)) +
    ggplot2::geom_boxplot(fill = relleno, colour = borde, width = 0.55,
                          linewidth = 0.4, outlier.shape = NA) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.08, height = 0, seed = 1),
                        shape = 21, fill = "white", colour = borde, size = 1.6) +
    ggplot2::geom_text(data = m, ggplot2::aes(x = nivel, y = tope, label = grupo),
                       vjust = -0.9, family = "serif", size = 3.8,
                       inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(x = attr(m, "titulo"), y = x$nombres$respuesta)
}

.ink_graf_interaccion <- function(x, borde, estilo) {
  if (!x$diseno %in% c("factorial", "pd")) {
    stop("El gr\u00e1fico de interacci\u00f3n requiere dos factores (dise\u00f1o factorial ",
         "o parcelas divididas).", call. = FALSE)
  }
  d <- x$datos
  medias <- stats::aggregate(y ~ A + B, d, mean)
  n <- stats::aggregate(y ~ A + B, d, length)$y
  cm <- x$anova$cm[x$anova$termino == "Residuals"]
  medias$ee <- sqrt(cm / n)
  nm <- x$nombres
  mapeo <- if (estilo == "articulo") {
    ggplot2::aes(x = B, y = y, group = A, shape = A, linetype = A)
  } else {
    ggplot2::aes(x = B, y = y, group = A, shape = A, linetype = A, colour = A)
  }
  fijo <- if (estilo == "articulo") list(colour = borde) else list()
  separa <- ggplot2::position_dodge(width = 0.15)
  g <- ggplot2::ggplot(medias, mapeo) +
    do.call(ggplot2::geom_errorbar, c(list(
      mapping = ggplot2::aes(ymin = y - ee, ymax = y + ee),
      width = 0.1, linetype = "solid", linewidth = 0.4, position = separa), fijo)) +
    do.call(ggplot2::geom_line, c(list(linewidth = 0.5, position = separa), fijo)) +
    do.call(ggplot2::geom_point, c(list(size = 2.4, fill = "white", position = separa), fijo)) +
    ggplot2::scale_shape_manual(values = rep(c(21, 16, 24, 15, 22, 17),
                                             length.out = nlevels(d$A))) +
    ggplot2::labs(x = nm$factor_b, y = nm$respuesta, shape = nm$tratamiento,
                  linetype = nm$tratamiento, colour = nm$tratamiento)
  g
}

.ink_error_de <- function(x, m) {
  fac <- attr(m, "factor")
  clave <- switch(fac, "B|A" = "B", fac)
  x$errores[[clave]]
}

.ink_graf_puntos <- function(x, m, borde) {
  err <- .ink_error_de(x, m)
  t <- stats::qt(1 - x$alfa / 2, err[["gl"]])
  m$inf <- m$media - t * m$ee
  m$sup <- m$media + t * m$ee
  conf <- paste0(round(100 * (1 - x$alfa)), "%")
  if (nrow(m) > 15L) {
    m$nivel <- factor(as.character(m$nivel), levels = m$nivel[order(m$media)])
    return(
      ggplot2::ggplot(m, ggplot2::aes(x = media, y = nivel)) +
        ggplot2::geom_errorbar(ggplot2::aes(xmin = inf, xmax = sup), width = 0.3,
                               colour = borde, linewidth = 0.3) +
        ggplot2::geom_point(shape = 21, fill = "white", colour = borde, size = 1.8) +
        ggplot2::geom_text(ggplot2::aes(x = sup, label = grupo), hjust = -0.3,
                           family = "serif", size = 2.8) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
        ggplot2::labs(y = attr(m, "titulo"),
                      x = paste0(x$nombres$respuesta, " (media e IC ", conf, ")"))
    )
  }
  ggplot2::ggplot(m, ggplot2::aes(x = nivel, y = media)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = inf, ymax = sup), width = 0.12,
                           colour = borde, linewidth = 0.4) +
    ggplot2::geom_point(shape = 21, fill = "white", colour = borde, size = 2.6,
                        stroke = 0.6) +
    ggplot2::geom_text(ggplot2::aes(y = sup, label = grupo), vjust = -0.7,
                       family = "serif", size = 3.8) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(x = attr(m, "titulo"),
                  y = paste0(x$nombres$respuesta, " (media e IC ", conf, ")"))
}

.ink_graf_regresion <- function(x, comparacion, borde) {
  validas <- names(x$regresion)[vapply(x$regresion, function(r) is.null(r$nota), logical(1))]
  if (!length(validas)) {
    stop("El gr\u00e1fico de regresi\u00f3n requiere un factor cuantitativo ",
         "(por ejemplo dosis) con al menos 3 niveles y datos balanceados.",
         call. = FALSE)
  }
  if (is.null(comparacion) || !comparacion %in% validas) {
    if (!is.null(comparacion) && !is.numeric(comparacion)) {
      stop("'", comparacion, "' no es un factor cuantitativo. Opciones: ",
           paste(validas, collapse = ", "), call. = FALSE)
    }
    comparacion <- validas[1L]
  }
  r <- x$regresion[[comparacion]]
  puntos <- data.frame(dosis = r$dosis, media = r$medias, ee = r$ee)
  g <- ggplot2::ggplot(puntos, ggplot2::aes(x = dosis, y = media)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = media - ee, ymax = media + ee),
                           width = diff(range(r$dosis)) * 0.02, colour = borde,
                           linewidth = 0.4)
  if (r$grado > 0L) {
    xs <- seq(min(r$dosis), max(r$dosis), length.out = 200L)
    curva <- data.frame(dosis = xs,
                        media = vapply(xs, function(v) sum(r$coef * v^(seq_along(r$coef) - 1L)),
                                       numeric(1)))
    g <- g + ggplot2::geom_line(data = curva, colour = borde, linewidth = 0.6)
    etiqueta <- paste0(.ink_ecuacion(r), "\nR\u00b2 = ", .ink_num(r$r2, 3L))
    if (!is.null(r$optimo)) {
      g <- g + ggplot2::geom_vline(xintercept = r$optimo[["x"]], linetype = "dashed",
                                   colour = "grey40", linewidth = 0.4)
      etiqueta <- paste0(etiqueta, "\nM\u00e1ximo en x = ", .ink_num(r$optimo[["x"]], 3L))
    }
  } else {
    etiqueta <- "Sin tendencia significativa"
  }
  sube <- r$medias[length(r$medias)] >= r$medias[1L]
  g + ggplot2::geom_point(shape = 21, fill = "white", colour = borde, size = 2.6,
                          stroke = 0.6) +
    ggplot2::annotate("text", x = if (sube) min(r$dosis) else max(r$dosis),
                      y = max(r$medias + r$ee), label = etiqueta,
                      hjust = if (sube) 0 else 1, vjust = 1, family = "serif",
                      size = 3.6, lineheight = 1.1) +
    ggplot2::labs(x = comparacion, y = x$nombres$respuesta)
}

.ink_graf_residuos <- function(x, borde) {
  rs <- suppressWarnings(stats::rstandard(x$modelo))
  ok <- is.finite(rs)
  rs <- rs[ok]
  aj <- stats::fitted(x$modelo)[ok]
  n <- length(rs)
  qq <- data.frame(panel = "Normalidad (Q-Q)",
                   x = stats::qnorm(stats::ppoints(n))[rank(rs, ties.method = "first")],
                   y = rs)
  rv <- data.frame(panel = "Residuos vs. ajustados", x = aj, y = rs)
  datos <- rbind(qq, rv)
  datos$panel <- factor(datos$panel, levels = c("Normalidad (Q-Q)", "Residuos vs. ajustados"))
  ggplot2::ggplot(datos, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_abline(data = datos[datos$panel == "Normalidad (Q-Q)", ][1L, ],
                         ggplot2::aes(intercept = 0, slope = 1),
                         colour = "grey40", linetype = "dashed") +
    ggplot2::geom_hline(data = datos[datos$panel == "Residuos vs. ajustados", ][1L, ],
                        ggplot2::aes(yintercept = 0),
                        colour = "grey40", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = c(-3, 3), colour = "grey70", linetype = "dotted") +
    ggplot2::geom_point(shape = 21, fill = "white", colour = borde, size = 1.8) +
    ggplot2::facet_wrap(~ panel, scales = "free_x") +
    ggplot2::labs(x = NULL, y = "Residuo estandarizado")
}

#' Elegir gráficos con un menú
#'
#' Muestra en la consola un menú con los gráficos disponibles para este
#' análisis, dibuja el elegido e imprime la línea de código equivalente para
#' copiarla en el script. Así quien no conoce los argumentos de
#' [plot()][plot.inkagro] puede explorar, y el análisis sigue siendo
#' reproducible.
#'
#' Ejecuta esta función en una línea sola: si se envía junto con otras
#' líneas, R toma la siguiente línea del script como respuesta al menú.
#'
#' @param x Objeto devuelto por [inkagro()].
#' @return El gráfico elegido (objeto `ggplot`), de forma invisible.
#' @examples
#' if (interactive()) {
#'   res <- inkagro(PlantGrowth, "weight", "group", diseno = "dca")
#'   graficos(res)
#' }
#' @export
graficos <- function(x) {
  if (!inherits(x, "inkagro")) {
    stop("'x' debe ser el resultado de inkagro().", call. = FALSE)
  }
  if (!interactive()) {
    stop("graficos() pregunta en la consola y solo funciona en una sesi\u00f3n ",
         "interactiva. En un script usa plot(res, tipo = ...).", call. = FALSE)
  }
  .ink_menu_graficos(x, deparse(substitute(x)), .ink_preguntar_consola, message)
}

# Menu de graficos reutilizado por graficos() y asistente(): dibuja el
# elegido, imprime la linea de codigo y vuelve a preguntar hasta "Terminar".
.ink_menu_graficos <- function(x, objeto, preguntar, decir) {
  opciones <- c(barras = "Barras con error est\u00e1ndar y letras",
                puntos = "Puntos con intervalo de confianza y letras",
                cajas  = "Cajas con los datos",
                interaccion = "Interacci\u00f3n entre los dos factores",
                regresion   = "Curva dosis-respuesta (factor cuantitativo)",
                residuos    = "Diagn\u00f3stico de residuos (supuestos)",
                dendrograma = "Dendrograma de grupos (Scott-Knott)")
  if (!x$diseno %in% c("factorial", "pd")) {
    opciones <- opciones[names(opciones) != "interaccion"]
  }
  validas <- names(x$regresion)[vapply(x$regresion, function(r) is.null(r$nota), logical(1))]
  if (!length(validas)) opciones <- opciones[names(opciones) != "regresion"]
  etiquetas <- c(unname(opciones), "Terminar")
  ultimo <- NULL

  repeat {
    i <- preguntar(if (is.null(ultimo)) "\u00bfQu\u00e9 gr\u00e1fico quieres ver?"
                   else "\u00bfQuieres ver otro gr\u00e1fico?", etiquetas, FALSE)
    if (!length(i) || i == 0L || i > length(opciones)) break
    tipo <- names(opciones)[i]

    comparacion <- NULL
    if (tipo %in% c("barras", "puntos", "cajas", "dendrograma") && length(x$medias) > 1L) {
      nombres <- names(x$medias)
      if (tipo == "cajas") {
        nombres <- nombres[vapply(x$medias, function(m) attr(m, "factor") %in% c("A", "B"), logical(1))]
      }
      j <- if (length(nombres) > 1L) preguntar("\u00bfDe qu\u00e9 factor?", nombres, FALSE) else 1L
      if (!length(j) || j == 0L || j > length(nombres)) next
      comparacion <- nombres[j]
    } else if (tipo == "regresion" && length(validas) > 1L) {
      j <- preguntar("\u00bfDe qu\u00e9 factor?", validas, FALSE)
      if (!length(j) || j == 0L || j > length(validas)) next
      comparacion <- validas[j]
    }

    g <- tryCatch(plot.inkagro(x, comparacion = comparacion, tipo = tipo),
                  error = function(e) e)
    if (inherits(g, "error")) {
      decir(paste0("\nNo se pudo dibujar ese gr\u00e1fico: ", conditionMessage(g)))
      next
    }
    print(g)
    ultimo <- g
    linea <- paste0("plot(", objeto,
                    if (!is.null(comparacion)) paste0(", comparacion = \"", comparacion, "\"") else "",
                    ", tipo = \"", tipo, "\")")
    decir(paste0("\nEl gr\u00e1fico est\u00e1 en la pesta\u00f1a Plots. Para repetirlo o guardarlo:\n  ",
                 linea, "\n  ", sub(")$", ", archivo = \"figura.png\")", linea)))
  }
  invisible(ultimo)
}
