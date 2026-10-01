#' InkAgro: análisis de experimentos agrícolas en español
#'
#' InkAgro analiza experimentos agrícolas con una sola llamada a
#' [inkagro()]: verifica que los datos correspondan al diseño, ajusta el
#' análisis de varianza, evalúa supuestos, señala valores atípicos y compara
#' medias, con un reporte en español pensado para quien no domina R ni
#' estadística.
#'
#' @keywords internal
"_PACKAGE"

# Columnas usadas dentro de aes() de ggplot2.
utils::globalVariables(c("nivel", "media", "ee", "grupo", "tope", "valor", "A", "B", "y", "inf", "sup", "dosis", "panel"))
