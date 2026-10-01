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
#'   si no, el primer factor.
#' @param tipo `"barras"` (medias, error estándar y letras), `"cajas"`
#'   (distribución de los datos) o `"interaccion"` (medias de cada
#'   combinación, solo para factorial y parcelas divididas).
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
#' @export
plot.inkagro <- function(x, comparacion = NULL,
                         tipo = c("barras", "cajas", "interaccion"),
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

  g <- if (tipo == "interaccion") {
    .ink_graf_interaccion(x, borde, estilo)
  } else {
    m <- .ink_elegir_comparacion(x, comparacion)
    if (tipo == "barras") {
      .ink_graf_barras(x, m, borde, relleno)
    } else {
      .ink_graf_cajas(x, m, borde, relleno)
    }
  }

  g <- g + .ink_tema(fuente, tamano)
  if (tipo != "interaccion" && nlevels(g$data$nivel) > 8L &&
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
