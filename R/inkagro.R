#' Analiza un experimento agrícola de principio a fin
#'
#' Lee los datos, verifica que su estructura corresponda al diseño
#' experimental, ajusta el análisis de varianza, evalúa los supuestos,
#' identifica valores atípicos y compara medias. Devuelve un objeto
#' `inkagro` que se imprime como un reporte en español y que se puede
#' graficar con [plot()][plot.inkagro] o convertir a tablas con
#' [as.data.frame()][as.data.frame.inkagro].
#'
#' @section Diseños soportados:
#' \describe{
#'   \item{`"dca"`}{Completamente al azar. Requiere `tratamiento`.}
#'   \item{`"dbca"`}{Bloques completos al azar. Requiere `tratamiento` y `bloque`.}
#'   \item{`"factorial"`}{Factorial de dos factores (`tratamiento` y
#'     `factor_b`), en DCA o, si se indica `bloque`, en DBCA.}
#'   \item{`"parcelas_divididas"`}{Parcelas divididas en bloques:
#'     `tratamiento` es el factor de la parcela principal y `factor_b` el de
#'     la subparcela. Requiere datos balanceados.}
#' }
#'
#' @section Por qué se pide el diseño:
#' El diseño depende de cómo se aleatorizó en campo, y eso no siempre se
#' puede deducir de los datos: un factorial en bloques y unas parcelas
#' divididas producen tablas idénticas, pero se analizan con errores
#' distintos. Por eso InkAgro no adivina: si `diseno` no se indica, lo
#' infiere solo a partir de los argumentos dados (y lo avisa), y en todos los
#' casos comprueba que los datos sean coherentes con el diseño. Si no lo son
#' (por ejemplo, varias localidades mezcladas, bloques que no contienen los
#' tratamientos o un factorial incompleto), se detiene con un mensaje que
#' explica el problema en lugar de producir un análisis incorrecto.
#'
#' @param datos Un `data.frame` o la ruta a un archivo `.csv`, `.txt`,
#'   `.tsv`, `.xlsx`, `.xls`, `.rds`, `.sav` o `.dta`.
#' @param respuesta Nombre de la columna con la variable medida (por
#'   ejemplo, rendimiento). No distingue mayúsculas.
#' @param tratamiento Nombre de la columna de tratamientos. En factoriales
#'   es el primer factor; en parcelas divididas, el de la parcela principal.
#' @param diseno Uno de `"dca"`, `"dbca"`, `"factorial"` o
#'   `"parcelas_divididas"` (se aceptan alias como `"rcbd"`, `"crd"`,
#'   `"split_plot"`). Si es `NULL` se infiere de los argumentos indicados.
#' @param bloque Nombre de la columna de bloques o repeticiones, si las hay.
#' @param factor_b Nombre de la columna del segundo factor (factorial) o de
#'   la subparcela (parcelas divididas).
#' @param prueba Prueba de comparación de medias: `"tukey"` (por defecto),
#'   `"duncan"`, `"lsd"` o `"snk"`. Se calculan con el paquete 'agricolae'.
#' @param alfa Nivel de significancia. Por defecto 0.05.
#' @param outliers Qué hacer con los valores atípicos (residuo
#'   estandarizado mayor que 3 en valor absoluto): `"marcar"` (por defecto)
#'   los reporta sin quitarlos; `"excluir"` repite el análisis sin ellos y
#'   reporta cuáles se quitaron. Nunca se eliminan datos en silencio.
#'
#' @return Un objeto de clase `inkagro`: una lista con el diseño, la tabla
#'   de análisis de varianza (`anova`), las comparaciones de medias
#'   (`medias`), el coeficiente de variación (`cv`), la evaluación de
#'   supuestos (`supuestos`), los valores atípicos (`atipicos`), los avisos
#'   generados (`avisos`), el modelo ajustado (`modelo`, de clase `lm`) y los
#'   datos usados (`datos`).
#'
#' @references
#' Pimentel-Gomes, F. (1985). *Curso de estatística experimental*. Nobel,
#' São Paulo. (Clasificación del coeficiente de variación.)
#'
#' de Mendiburu, F. (2023). *agricolae: Statistical Procedures for
#' Agricultural Research*. R package.
#'
#' @examples
#' # Completamente al azar: peso seco de plantas con dos tratamientos y un control
#' res <- inkagro(PlantGrowth, respuesta = "weight", tratamiento = "group",
#'                diseno = "dca")
#' res
#'
#' # Factorial 2 x 2 en bloques (nitrógeno x fósforo)
#' inkagro(npk, respuesta = "yield", tratamiento = "N", factor_b = "P",
#'         bloque = "block", diseno = "factorial")
#'
#' # Parcelas divididas: variedad en la parcela principal, dosis de N en la
#' # subparcela (Yates, 1935)
#' if (requireNamespace("nlme", quietly = TRUE)) {
#'   avena <- as.data.frame(nlme::Oats)
#'   inkagro(avena, respuesta = "yield", tratamiento = "Variety",
#'           factor_b = "nitro", bloque = "Block",
#'           diseno = "parcelas_divididas")
#' }
#' @export
inkagro <- function(datos, respuesta, tratamiento,
                    diseno   = NULL,
                    bloque   = NULL,
                    factor_b = NULL,
                    prueba   = c("tukey", "duncan", "lsd", "snk"),
                    alfa     = 0.05,
                    outliers = c("marcar", "excluir")) {

  prueba   <- match.arg(prueba)
  outliers <- match.arg(outliers)
  if (missing(respuesta) || missing(tratamiento)) {
    stop("Indica al menos 'respuesta' (la variable medida) y 'tratamiento'.",
         call. = FALSE)
  }
  if (!is.numeric(alfa) || length(alfa) != 1L || is.na(alfa) ||
      alfa <= 0 || alfa >= 0.5) {
    stop("'alfa' debe ser un n\u00famero entre 0 y 0.5 (lo usual es 0.05).",
         call. = FALSE)
  }

  crudo <- .ink_leer(datos)
  if (nrow(crudo) == 0L) stop("Los datos no tienen filas.", call. = FALSE)

  nm <- list(
    respuesta   = .ink_col(crudo, respuesta, "respuesta"),
    tratamiento = .ink_col(crudo, tratamiento, "tratamiento"),
    bloque      = .ink_col(crudo, bloque, "bloque"),
    factor_b    = .ink_col(crudo, factor_b, "factor_b")
  )
  usados <- unlist(nm, use.names = FALSE)
  if (anyDuplicated(usados)) {
    stop("La columna '", usados[anyDuplicated(usados)],
         "' se indic\u00f3 en dos argumentos distintos.", call. = FALSE)
  }

  dis    <- .ink_resolver_diseno(diseno, nm$bloque, nm$factor_b)
  avisos <- dis$aviso

  d <- data.frame(.fila = seq_len(nrow(crudo)),
                  y     = .ink_numerica(crudo[[nm$respuesta]], nm$respuesta))
  for (rol in c("A", "B", "blq")) {
    columna <- nm[[c(A = "tratamiento", B = "factor_b", blq = "bloque")[[rol]]]]
    if (is.null(columna)) next
    f <- .ink_factor(crudo[[columna]])
    avisos <- c(avisos, .ink_aviso_unificados(f, columna))
    attr(f, "unificados") <- NULL
    d[[rol]] <- f
  }

  completos <- stats::complete.cases(d)
  if (!any(completos)) {
    stop("Ninguna fila tiene datos completos en las columnas indicadas.",
         call. = FALSE)
  }
  if (any(!completos)) {
    avisos <- c(avisos, sprintf(
      "Se %s %d %s con datos faltantes en las columnas analizadas (%s %s).",
      if (sum(!completos) == 1L) "excluy\u00f3" else "excluyeron",
      sum(!completos), if (sum(!completos) == 1L) "fila" else "filas",
      if (sum(!completos) == 1L) "fila" else "filas",
      .ink_lista(d$.fila[!completos])))
  }
  d <- droplevels(d[completos, , drop = FALSE])
  otras <- crudo[, setdiff(names(crudo), usados), drop = FALSE]

  avisos <- c(avisos, .ink_verificar(d, dis$codigo, nm, otras))
  ajuste <- .ink_ajustar(d, dis$codigo, nm, prueba, alfa)
  atip   <- .ink_atipicos(ajuste$modelo, d)

  if (outliers == "excluir" && nrow(atip) > 0L) {
    d2 <- droplevels(d[!d$.fila %in% atip$fila, , drop = FALSE])
    reintento <- tryCatch({
      av2 <- .ink_verificar(d2, dis$codigo, nm, otras)
      list(ok = TRUE, av = av2,
           ajuste = .ink_ajustar(d2, dis$codigo, nm, prueba, alfa))
    }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    if (reintento$ok) {
      avisos <- c(avisos, sprintf(
        "Se %s %d %s (%s %s) y se repiti\u00f3 el an\u00e1lisis sin %s.",
        if (nrow(atip) == 1L) "excluy\u00f3" else "excluyeron", nrow(atip),
        if (nrow(atip) == 1L) "valor at\u00edpico" else "valores at\u00edpicos",
        if (nrow(atip) == 1L) "fila" else "filas", .ink_lista(atip$fila),
        if (nrow(atip) == 1L) "\u00e9l" else "ellos"), reintento$av)
      d      <- d2
      ajuste <- reintento$ajuste
    } else {
      avisos <- c(avisos, paste0(
        "No se excluyeron los valores at\u00edpicos porque, sin ellos, el dise\u00f1o ",
        "quedar\u00eda inv\u00e1lido: ", reintento$msg))
    }
  }

  avisos <- c(avisos, ajuste$avisos)

  regresion <- list()
  for (rol in intersect(c("A", "B"), names(d))) {
    if (!.ink_es_cuantitativo(d[[rol]])) next
    columna <- if (rol == "A") nm$tratamiento else nm$factor_b
    regresion[[columna]] <- .ink_regresion(d$y, d[[rol]], ajuste$errores[[rol]], alfa)
  }
  p_ab <- ajuste$anova$p[ajuste$anova$termino == "A:B"]
  if (length(regresion) && length(p_ab) && !is.na(p_ab) && p_ab < alfa) {
    avisos <- c(avisos, paste0(
      "Con interacci\u00f3n significativa, la curva de regresi\u00f3n promedia ",
      "los niveles del otro factor; interpr\u00e9tala con cuidado."))
  }

  structure(
    list(
      diseno        = dis$codigo,
      nombre_diseno = .ink_nombre_diseno(dis$codigo, !is.null(nm$bloque)),
      nombres       = nm,
      n             = nrow(d),
      anova         = ajuste$anova,
      tipo_sc       = ajuste$tipo_sc,
      medias        = ajuste$medias,
      cv            = ajuste$cv,
      errores       = ajuste$errores,
      regresion     = regresion,
      media_general = mean(d$y),
      supuestos     = .ink_supuestos(ajuste$modelo, d, dis$codigo),
      atipicos      = atip,
      outliers      = outliers,
      prueba        = prueba,
      alfa          = alfa,
      avisos        = unique(avisos[nzchar(avisos)]),
      modelo        = ajuste$modelo,
      datos         = d
    ),
    class = "inkagro"
  )
}
