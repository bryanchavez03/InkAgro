inkagro_detect <- function(datos, verbose = TRUE) {

  cols <- tolower(names(datos))

  # Excluir columnas no experimentales
  cols <- cols[!cols %in% cols_excluir]

  # Verificar presencia
  tiene_bloque   <- any(cols %in% pal_bloque)
  tiene_trat     <- any(cols %in% pal_trat)
  tiene_factor   <- sum(cols %in% pal_factor) >= 2
  tiene_parcela  <- any(cols %in% pal_parcela)
  tiene_fila     <- any(cols %in% pal_fila)
  tiene_columna  <- any(cols %in% pal_columna)
  tiene_ambiente <- any(cols %in% pal_ambiente)
  tiene_year     <- any(cols %in% pal_year)
  tiene_alpha    <- any(cols %in% pal_alpha)
  tiene_strip    <- any(cols %in% pal_strip)
  tiene_tiempo   <- any(cols %in% pal_tiempo)

  # Detectar diseño
  if (tiene_strip) {
    diseno <- "Strip-plot"
  } else if (tiene_tiempo & tiene_parcela & tiene_bloque) {
    diseno <- "Repeated Measures"
  } else if (tiene_trat & tiene_ambiente & tiene_bloque) {
    diseno <- "Multiambiental (MET)"
  } else if (tiene_alpha & tiene_bloque) {
    diseno <- "Alfa-latice"
  } else if (tiene_parcela & tiene_bloque) {
    diseno <- "Parcelas Divididas"
  } else if (tiene_fila & tiene_columna & tiene_trat) {
    diseno <- "Cuadrado Latino"
  } else if (tiene_factor & tiene_ambiente) {
    diseno <- "Factorial con Ambientes"
  } else if (tiene_factor) {
    diseno <- "Factorial"
  } else if (tiene_trat & tiene_bloque) {
    diseno <- "DBCA"
  } else if (tiene_trat & !tiene_bloque) {
    diseno <- "DCA"
  } else {
    diseno <- "No detectado - revise nombres de columnas"
  }

  if (verbose) message("Diseño detectado: ", diseno)
  return(diseno)
}

