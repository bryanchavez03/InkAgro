#' Informe en Word de un análisis InkAgro
#'
#' Genera un documento de Word con el resumen del experimento, los avisos,
#' la tabla de análisis de varianza, las conclusiones, los supuestos, las
#' comparaciones de medias, los valores atípicos y los gráficos, con
#' tablas y figuras numeradas listas para una tesis o un artículo.
#'
#' Requiere los paquetes 'officer' y 'flextable'.
#'
#' @param x Objeto devuelto por [inkagro()].
#' @param archivo Ruta del archivo `.docx` a crear.
#' @param titulo Título del informe. Por defecto se arma con la variable
#'   respuesta.
#' @param fuente Fuente del documento. Por defecto `"Times New Roman"`.
#' @param tamano Tamaño de letra en puntos. Por defecto 12.
#' @param tablas `"articulo"` (solo líneas horizontales, estilo de revista)
#'   o `"cuadros"` (todas las celdas con borde).
#' @param graficos Si es `TRUE` (por defecto), incluye las figuras.
#' @return La ruta del archivo creado, de forma invisible.
#' @examples
#' \donttest{
#' if (requireNamespace("officer", quietly = TRUE) &&
#'     requireNamespace("flextable", quietly = TRUE)) {
#'   res <- inkagro(npk, "yield", "N", factor_b = "P", bloque = "block",
#'                  diseno = "factorial")
#'   archivo <- informe(res, file.path(tempdir(), "informe.docx"))
#' }
#' }
#' @export
informe <- function(x, archivo = "informe_inkagro.docx", titulo = NULL,
                    fuente = "Times New Roman", tamano = 12,
                    tablas = c("articulo", "cuadros"), graficos = TRUE) {
  if (!inherits(x, "inkagro")) {
    stop("'x' debe ser el resultado de inkagro().", call. = FALSE)
  }
  for (p in c("officer", "flextable")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Para crear el informe instala el paquete '", p,
           "': install.packages(\"", p, "\")", call. = FALSE)
    }
  }
  tablas <- match.arg(tablas)
  nm <- x$nombres
  if (is.null(titulo)) {
    titulo <- paste0("An\u00e1lisis de ", nm$respuesta)
  }

  doc <- officer::read_docx()
  num_tabla <- 0L
  num_figura <- 0L
  par <- function(doc, texto, negrita = FALSE, cursiva = FALSE, tam = tamano,
                  alinear = "justify", antes = 0, despues = 6, pegado = FALSE) {
    officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(texto, officer::fp_text(font.family = fuente, font.size = tam,
                                             bold = negrita, italic = cursiva)),
      fp_p = officer::fp_par(text.align = alinear, padding.top = antes,
                             padding.bottom = despues, keep_with_next = pegado)))
  }
  seccion <- function(doc, texto) par(doc, texto, negrita = TRUE, antes = 12, pegado = TRUE)
  rotulo_tabla <- function(doc, texto) {
    num_tabla <<- num_tabla + 1L
    par(doc, paste0("Tabla ", num_tabla, ". ", texto), negrita = TRUE,
        alinear = "left", antes = 8, despues = 4, pegado = TRUE)
  }
  tabla <- function(doc, df, nota = NULL, izquierda = NULL) {
    ft <- flextable::flextable(df)
    ft <- if (tablas == "articulo") flextable::theme_booktabs(ft) else flextable::theme_box(ft)
    if (!is.null(nota)) ft <- flextable::add_footer_lines(ft, nota)
    ft <- flextable::font(ft, fontname = fuente, part = "all")
    ft <- flextable::fontsize(ft, size = tamano - 2, part = "all")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::align(ft, align = "center", part = "all")
    ft <- flextable::align(ft, j = c(1, izquierda), align = "left", part = "all")
    ft <- flextable::align(ft, align = "left", part = "footer")
    ft <- flextable::padding(ft, padding.top = 1.5, padding.bottom = 1.5, part = "all")
    ft <- flextable::autofit(ft)
    if (exists("paginate", envir = asNamespace("flextable"))) {
      ft <- flextable::paginate(ft, init = TRUE, hdr_ftr = TRUE)
    }
    flextable::body_add_flextable(doc, ft, align = "center")
  }
  figura <- function(doc, g, texto) {
    num_figura <<- num_figura + 1L
    doc <- officer::body_add_gg(doc, g, width = 6, height = 3.75, res = 300)
    par(doc, paste0("Figura ", num_figura, ". ", texto), cursiva = TRUE,
        tam = tamano - 1, alinear = "left", despues = 10)
  }

  # Encabezado
  doc <- par(doc, titulo, negrita = TRUE, tam = tamano + 4, alinear = "center",
             despues = 2)
  doc <- par(doc, x$nombre_diseno, cursiva = TRUE, alinear = "center",
             despues = 12)

  # Resumen
  doc <- seccion(doc, "1. Datos del experimento")
  resumen <- data.frame(
    Elemento = c("Dise\u00f1o", "Variable respuesta", "Tratamiento",
                 if (!is.null(nm$factor_b)) "Segundo factor",
                 if (!is.null(nm$bloque)) "Bloques",
                 "Observaciones analizadas", "Comparaci\u00f3n de medias",
                 "Nivel de significancia"),
    Valor = c(x$nombre_diseno, nm$respuesta,
              sprintf("%s (%d niveles)", nm$tratamiento, nlevels(x$datos$A)),
              if (!is.null(nm$factor_b)) sprintf("%s (%d niveles)", nm$factor_b, nlevels(x$datos$B)),
              if (!is.null(nm$bloque)) sprintf("%s (%d)", nm$bloque, nlevels(x$datos$blq)),
              as.character(x$n), .ink_nombre_prueba(x$prueba), as.character(x$alfa)),
    stringsAsFactors = FALSE)
  doc <- rotulo_tabla(doc, "Caracter\u00edsticas del experimento.")
  doc <- tabla(doc, resumen, izquierda = 2)

  if (length(x$avisos)) {
    doc <- par(doc, "Avisos del an\u00e1lisis:", negrita = TRUE, antes = 8, despues = 2)
    for (a in x$avisos) doc <- par(doc, paste0("\u2022 ", a), despues = 2)
  }

  # ANOVA
  doc <- seccion(doc, "2. An\u00e1lisis de varianza")
  tab <- x$anova
  anova_df <- data.frame(
    Fuente = tab$fuente, GL = as.character(tab$gl),
    SC = .ink_num(tab$sc, 2L), CM = .ink_num(tab$cm, 2L),
    F = .ink_num(tab$F, 2L), p = .ink_p(tab$p), Sig = tab$sig,
    stringsAsFactors = FALSE)
  names(anova_df)[c(1, 7)] <- c("Fuente de variaci\u00f3n", "Sig.")
  cv_txt <- paste0("CV", ifelse(names(x$cv) == "general", "", paste0(" (", names(x$cv), ")")),
                   " = ", .ink_num(x$cv, 1L), " %", collapse = "; ")
  doc <- rotulo_tabla(doc, paste0("An\u00e1lisis de varianza para ", nm$respuesta, "."))
  doc <- tabla(doc, anova_df, nota = paste0(
    "GL: grados de libertad; SC: suma de cuadrados (tipo ", x$tipo_sc,
    "); CM: cuadrado medio. *** p < 0.001; ** p < 0.01; * p < 0.05; ",
    "ns: no significativo. ", cv_txt, "; media general = ",
    .ink_num(x$media_general, 2L), "."))

  doc <- par(doc, "Interpretaci\u00f3n:", negrita = TRUE, antes = 8, despues = 2)
  for (cc in .ink_conclusiones(x)) doc <- par(doc, paste0("\u2022 ", cc), despues = 2)

  # Supuestos
  doc <- seccion(doc, "3. Supuestos del an\u00e1lisis de varianza")
  s <- x$supuestos
  sup_df <- data.frame(
    Supuesto = c("Normalidad de los residuos", "Homogeneidad de varianzas"),
    Prueba = c(s$normalidad$prueba, s$homogeneidad$prueba),
    Estadistico = c(.ink_num(s$normalidad$estadistico, 3L),
                    .ink_num(s$homogeneidad$estadistico, 3L)),
    p = c(.ink_p(s$normalidad$p), .ink_p(s$homogeneidad$p)),
    Resultado = vapply(list(s$normalidad, s$homogeneidad), function(z) {
      if (is.na(z$p)) "No aplicable" else if (z$p >= x$alfa) "Se cumple" else "No se cumple"
    }, character(1)),
    stringsAsFactors = FALSE)
  names(sup_df)[3] <- "Estad\u00edstico"
  doc <- rotulo_tabla(doc, "Verificaci\u00f3n de supuestos sobre los residuos del modelo.")
  doc <- tabla(doc, sup_df)

  # Medias
  doc <- seccion(doc, "4. Comparaci\u00f3n de medias")
  for (nombre in names(x$medias)) {
    m <- x$medias[[nombre]]
    med_df <- data.frame(Nivel = m$nivel, n = as.character(m$n),
                         Media = .ink_num(m$media, 2L), EE = .ink_num(m$ee, 2L),
                         Grupo = m$grupo, stringsAsFactors = FALSE)
    p <- attr(m, "p")
    extra <- if (!is.na(p) && p >= x$alfa) " El efecto no fue significativo en el an\u00e1lisis de varianza." else ""
    doc <- rotulo_tabla(doc, paste0("Medias de ", nm$respuesta, " seg\u00fan ", nombre, "."))
    doc <- tabla(doc, med_df, nota = paste0(
      "EE: error est\u00e1ndar de la media. Medias con la misma letra no difieren ",
      "significativamente (", .ink_nombre_prueba(x$prueba), ", alfa = ", x$alfa,
      ").", extra))
  }

  # Atipicos
  if (nrow(x$atipicos)) {
    doc <- seccion(doc, "5. Valores at\u00edpicos")
    at <- x$atipicos
    at_df <- data.frame(Fila = as.character(at$fila), Tratamiento = at$tratamiento,
                        Valor = .ink_num(at$valor, 2L),
                        "Residuo estandarizado" = .ink_num(at$residuo_est, 2L),
                        check.names = FALSE, stringsAsFactors = FALSE)
    doc <- rotulo_tabla(doc, "Observaciones con residuo estandarizado mayor que 3 en valor absoluto.")
    doc <- tabla(doc, at_df, nota = if (x$outliers == "excluir")
      "Estas observaciones se excluyeron y el an\u00e1lisis se repiti\u00f3 sin ellas."
      else "Estas observaciones se mantuvieron en el an\u00e1lisis.")
  }

  # Figuras
  if (graficos) {
    doc <- seccion(doc, paste0(if (nrow(x$atipicos)) "6" else "5", ". Figuras"))
    for (nombre in names(x$medias)) {
      if (!attr(x$medias[[nombre]], "factor") %in% c("A", "B", "AB")) next
      g <- plot.inkagro(x, comparacion = nombre, fuente = "serif")
      doc <- figura(doc, g, paste0(
        "Medias de ", nm$respuesta, " seg\u00fan ", nombre,
        ". Barras: error est\u00e1ndar. Letras distintas indican diferencias ",
        "significativas (", .ink_nombre_prueba(x$prueba), ", alfa = ", x$alfa, ")."))
    }
    if (x$diseno %in% c("factorial", "pd")) {
      g <- plot.inkagro(x, tipo = "interaccion", fuente = "serif")
      doc <- figura(doc, g, paste0(
        "Interacci\u00f3n ", nm$tratamiento, " x ", nm$factor_b, " sobre ",
        nm$respuesta, ". Barras: error est\u00e1ndar."))
    }
  }

  doc <- par(doc, paste0(
    "Informe generado con InkAgro ", utils::packageVersion("InkAgro"), " en ",
    R.version.string, ". Comparaciones de medias calculadas con agricolae."),
    cursiva = TRUE, tam = tamano - 3, alinear = "left", antes = 18)

  print(doc, target = archivo)
  invisible(normalizePath(archivo, mustWork = FALSE))
}

.ink_nombre_prueba <- function(prueba) {
  c(tukey = "Tukey", duncan = "Duncan", lsd = "LSD de Fisher",
    snk = "Student-Newman-Keuls")[[prueba]]
}
