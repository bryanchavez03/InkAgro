#' Reporte de un análisis InkAgro
#'
#' Imprime el reporte del análisis: diseño, avisos, análisis de varianza,
#' coeficiente de variación, conclusiones, supuestos, comparación de medias
#' y valores atípicos.
#'
#' @param x,object Objeto devuelto por [inkagro()].
#' @param max_niveles Número máximo de niveles a mostrar en cada tabla de
#'   medias. Las tablas completas se obtienen con [as.data.frame.inkagro()].
#' @param ... No se usa.
#' @return `x`, de forma invisible.
#' @examples
#' res <- inkagro(PlantGrowth, "weight", "group", diseno = "dca")
#' print(res)
#' summary(res)
#' @export
print.inkagro <- function(x, max_niveles = 20, ...) {
  nm <- x$nombres
  linea <- strrep("-", 66)
  cat("\nInkAgro | ", x$nombre_diseno, "\n", linea, "\n", sep = "")
  cat("Respuesta: ", nm$respuesta, " | ", nm$tratamiento, ": ",
      nlevels(x$datos$A), " niveles", sep = "")
  if (!is.null(nm$factor_b)) cat(" | ", nm$factor_b, ": ", nlevels(x$datos$B), " niveles", sep = "")
  if (!is.null(nm$bloque))   cat(" | Bloques: ", nlevels(x$datos$blq), sep = "")
  cat(" | n = ", x$n, "\n", sep = "")

  if (length(x$avisos)) {
    cat("\nAvisos\n")
    for (a in x$avisos) cat(.ink_envolver(a, "  ! "), "\n", sep = "")
  }

  cat("\nAn\u00e1lisis de varianza (suma de cuadrados tipo ", x$tipo_sc, ")\n", sep = "")
  tab <- x$anova
  mostrar <- data.frame(
    Fuente = formatC(tab$fuente, width = -max(nchar(tab$fuente))),
    GL     = tab$gl,
    SC     = .ink_num(tab$sc, 3L),
    CM     = .ink_num(tab$cm, 3L),
    F      = .ink_num(tab$F, 2L),
    p      = .ink_p(tab$p),
    " "    = tab$sig,
    check.names = FALSE
  )
  print(mostrar, row.names = FALSE)
  cat("Signif.: *** p<0.001  ** p<0.01  * p<0.05  ns no significativo\n")

  cv_txt <- paste0(
    "CV", ifelse(names(x$cv) == "general", "", paste0("(", names(x$cv), ")")),
    " = ", .ink_num(x$cv, 1L), "% (", .ink_clase_cv(x$cv), ")", collapse = " | ")
  cat("\n", cv_txt, " | Media general = ", .ink_num(x$media_general, 3L), "\n", sep = "")

  cat("\nConclusiones (alfa = ", x$alfa, ")\n", sep = "")
  for (cc in .ink_conclusiones(x)) cat(.ink_envolver(cc, "  - "), "\n", sep = "")

  cat("\nSupuestos (sobre los residuos)\n")
  s <- x$supuestos
  cat("  Normalidad (", s$normalidad$prueba, "): ",
      .ink_txt_supuesto(s$normalidad, "W", x$alfa), "\n", sep = "")
  cat("  Homogeneidad de varianzas (", s$homogeneidad$prueba, "): ",
      .ink_txt_supuesto(s$homogeneidad, "F", x$alfa), "\n", sep = "")
  if (isFALSE(.ink_cumple(s, x$alfa))) {
    cat(.ink_envolver(paste0(
      "Alg\u00fan supuesto no se cumple: revisa los valores at\u00edpicos y considera ",
      "transformar la respuesta (logaritmo o ra\u00edz cuadrada) antes de ",
      "interpretar las pruebas."), "  "), "\n", sep = "")
  }

  nombre_prueba <- c(tukey = "Tukey", duncan = "Duncan", lsd = "LSD de Fisher",
                     snk = "Student-Newman-Keuls")[[x$prueba]]
  cat("\nComparaci\u00f3n de medias (", nombre_prueba, ", alfa = ", x$alfa,
      ")\n", sep = "")
  for (nombre in names(x$medias)) {
    m <- x$medias[[nombre]]
    p <- attr(m, "p")
    nota <- if (!is.na(p) && p >= x$alfa) "  [efecto no significativo en el ANOVA]" else ""
    cat("\n  ", nombre, nota, "\n", sep = "")
    vista <- data.frame(Nivel = m$nivel, n = m$n, Media = .ink_num(m$media, 3L),
                        EE = .ink_num(m$ee, 3L), Grupo = m$grupo)
    if (nrow(vista) > max_niveles) {
      print(utils::head(vista, max_niveles), row.names = FALSE, right = FALSE)
      cat("  ... y ", nrow(vista) - max_niveles,
          " niveles m\u00e1s (tabla completa: as.data.frame(x, tabla = \"medias\"))\n", sep = "")
    } else {
      print(vista, row.names = FALSE, right = FALSE)
    }
  }
  cat("  Letras distintas indican diferencias significativas. EE = error est\u00e1ndar de la media.\n")

  cat("\nValores at\u00edpicos (|residuo estandarizado| > 3): ")
  if (nrow(x$atipicos) == 0L) {
    cat("ninguno.\n")
  } else {
    accion <- if (x$outliers == "excluir") "" else
      " Se mantienen en el an\u00e1lisis; usa outliers = \"excluir\" para repetirlo sin ellos."
    cat(nrow(x$atipicos), ".", accion, "\n", sep = "")
    at <- x$atipicos
    names(at) <- c("Fila", nm$tratamiento, nm$respuesta, "Residuo est.")
    print(at, row.names = FALSE, right = FALSE)
  }
  cat("\n")
  invisible(x)
}

#' @rdname print.inkagro
#' @export
summary.inkagro <- function(object, ...) {
  print.inkagro(object, max_niveles = Inf, ...)
}

#' Tablas de un análisis InkAgro
#'
#' Extrae como `data.frame` la tabla de análisis de varianza, las
#' comparaciones de medias o los valores atípicos, para exportarlas (por
#' ejemplo con `write.csv()`) o seguir trabajando con ellas.
#'
#' @param x Objeto devuelto por [inkagro()].
#' @param row.names,optional No se usan; existen por compatibilidad con
#'   [as.data.frame()].
#' @param tabla `"anova"` (por defecto), `"medias"` o `"atipicos"`.
#' @param ... No se usa.
#' @return Un `data.frame`.
#' @examples
#' res <- inkagro(PlantGrowth, "weight", "group", diseno = "dca")
#' as.data.frame(res)
#' as.data.frame(res, tabla = "medias")
#' @export
as.data.frame.inkagro <- function(x, row.names = NULL, optional = FALSE,
                                  tabla = c("anova", "medias", "atipicos"), ...) {
  tabla <- match.arg(tabla)
  switch(
    tabla,
    anova = {
      out <- x$anova[, c("fuente", "gl", "sc", "cm", "F", "p", "sig")]
      rownames(out) <- NULL
      out
    },
    medias = {
      partes <- lapply(names(x$medias), function(nombre) {
        m <- x$medias[[nombre]]
        data.frame(comparacion = nombre, m, stringsAsFactors = FALSE)
      })
      out <- do.call(rbind, partes)
      rownames(out) <- NULL
      out
    },
    atipicos = x$atipicos
  )
}

#' Gráfico de medias de un análisis InkAgro
#'
#' Grafica las medias con su error estándar y las letras de la comparación
#' de medias (`tipo = "barras"`), o la distribución de los datos por nivel
#' (`tipo = "cajas"`). Devuelve un objeto 'ggplot2' que se puede seguir
#' modificando.
#'
#' @param x Objeto devuelto por [inkagro()].
#' @param comparacion Nombre o número de la comparación a graficar (ver
#'   `names(x$medias)`). Por defecto, la interacción si fue significativa;
#'   si no, el primer factor.
#' @param tipo `"barras"` o `"cajas"`.
#' @param color_barras Color del borde y de las barras de error.
#' @param color_relleno Color de relleno.
#' @param tema `"classic"`, `"minimal"` o `"bw"`.
#' @param archivo Ruta opcional para guardar el gráfico (por ejemplo
#'   `"figura1.png"`). Si es `NULL` no se guarda nada.
#' @param ancho,alto Tamaño en pulgadas al guardar.
#' @param resolucion Resolución en puntos por pulgada al guardar.
#' @param ... No se usa.
#' @return Un objeto `ggplot`.
#' @examples
#' res <- inkagro(PlantGrowth, "weight", "group", diseno = "dca")
#' plot(res)
#' plot(res, tipo = "cajas")
#' @export
plot.inkagro <- function(x, comparacion = NULL, tipo = c("barras", "cajas"),
                         color_barras = "#2E75B6", color_relleno = "#BDD7EE",
                         tema = c("classic", "minimal", "bw"),
                         archivo = NULL, ancho = 8, alto = 6,
                         resolucion = 300, ...) {
  tipo <- match.arg(tipo)
  tema <- match.arg(tema)
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
  titulo_x <- if (is.numeric(comparacion)) names(x$medias)[comparacion] else comparacion
  m$nivel <- factor(m$nivel, levels = attr(m, "orden_niveles"))

  tema_gg <- switch(tema, classic = ggplot2::theme_classic(),
                    minimal = ggplot2::theme_minimal(), bw = ggplot2::theme_bw())
  nombre_prueba <- c(tukey = "Tukey", duncan = "Duncan", lsd = "LSD",
                     snk = "SNK")[[x$prueba]]

  if (tipo == "barras") {
    m$tope <- m$media + m$ee
    g <- ggplot2::ggplot(m, ggplot2::aes(x = nivel, y = media)) +
      ggplot2::geom_col(fill = color_relleno, colour = color_barras, width = 0.7) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = media - ee, ymax = media + ee),
                             width = 0.2, colour = color_barras) +
      ggplot2::geom_text(ggplot2::aes(y = tope, label = grupo), vjust = -0.6) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
      ggplot2::labs(x = titulo_x, y = x$nombres$respuesta,
                    caption = sprintf(
                      "Media \u00b1 EE. Letras distintas: diferencias significativas (%s, alfa = %s).",
                      nombre_prueba, x$alfa))
  } else {
    fac <- attr(m, "factor")
    if (!fac %in% c("A", "B")) {
      stop("El gr\u00e1fico de cajas solo est\u00e1 disponible para los factores ",
           "principales.", call. = FALSE)
    }
    datos <- data.frame(nivel = x$datos[[fac]], valor = x$datos$y)
    datos$nivel <- factor(as.character(datos$nivel), levels = levels(m$nivel))
    tope <- stats::aggregate(valor ~ nivel, datos, max)
    m$tope <- tope$valor[match(m$nivel, tope$nivel)]
    g <- ggplot2::ggplot(datos, ggplot2::aes(x = nivel, y = valor)) +
      ggplot2::geom_boxplot(fill = color_relleno, colour = color_barras,
                            outlier.shape = 21, outlier.fill = "white") +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.5, colour = color_barras) +
      ggplot2::geom_text(data = m, ggplot2::aes(x = nivel, y = tope, label = grupo),
                         vjust = -0.8, inherit.aes = FALSE) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
      ggplot2::labs(x = titulo_x, y = x$nombres$respuesta)
  }
  g <- g + tema_gg
  if (nlevels(m$nivel) > 8L) {
    g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  if (!is.null(archivo)) {
    ggplot2::ggsave(archivo, g, width = ancho, height = alto, dpi = resolucion)
  }
  g
}

# ---- auxiliares de reporte ---------------------------------------------

.ink_clase_cv <- function(cv) {
  as.character(cut(cv, c(-Inf, 10, 20, 30, Inf),
                   labels = c("bajo", "medio", "alto", "muy alto"),
                   right = FALSE))
}

.ink_txt_supuesto <- function(s, letra, alfa) {
  if (is.na(s$p)) return(s$nota)
  paste0(letra, " = ", .ink_num(s$estadistico, 3L), ", p = ", .ink_p(s$p),
         if (s$p >= alfa) " -> se cumple" else " -> NO se cumple")
}

.ink_cumple <- function(s, alfa) {
  p <- c(s$normalidad$p, s$homogeneidad$p)
  p <- p[!is.na(p)]
  if (!length(p)) return(NA)
  all(p >= alfa)
}

.ink_conclusiones <- function(x) {
  tab <- x$anova
  probar <- tab[tab$termino %in% c("A", "B", "A:B") & !is.na(tab$p), ]
  vapply(seq_len(nrow(probar)), function(i) {
    f <- probar[i, ]
    que <- if (f$termino == "A:B") paste("la interacci\u00f3n", f$fuente)
           else paste0("los niveles de '", f$fuente, "'")
    stats_txt <- sprintf("(F = %s; p %s).", .ink_num(f$F, 2L),
                         if (f$p < 0.001) "< 0.001" else paste("=", .ink_p(f$p)))
    if (f$p < x$alfa) {
      if (f$termino == "A:B") {
        paste("Hay efecto significativo de", que, stats_txt)
      } else {
        paste("Hay diferencias significativas entre", que, stats_txt)
      }
    } else if (f$termino == "A:B") {
      paste("No se detect\u00f3 interacci\u00f3n significativa", f$fuente, stats_txt)
    } else {
      paste("No se detectaron diferencias significativas entre", que, stats_txt)
    }
  }, character(1))
}

.ink_envolver <- function(texto, prefijo) {
  lineas <- strwrap(texto, width = 74 - nchar(prefijo))
  paste0(c(prefijo, rep(strrep(" ", nchar(prefijo)), length(lineas) - 1L)),
         lineas, collapse = "\n")
}
