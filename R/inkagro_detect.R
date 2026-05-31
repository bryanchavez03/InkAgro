inkagro_detect <- function(datos, verbose = TRUE) {
  cols <- tolower(names(datos))

  # Excluir columnas no experimentales
  cols <- cols[!cols %in% cols_excluir]

  # Contar columnas experimentales totales
  n_trat_cols    <- sum(cols %in% pal_trat)
  n_factor_cols  <- sum(cols %in% pal_factor)
  total_factores <- n_trat_cols + n_factor_cols

  tiene_multifactor <- total_factores >= 2

  # Verificar presencia
  tiene_bloque   <- any(cols %in% pal_bloque)
  tiene_trat     <- any(cols %in% pal_trat)
  tiene_factor   <- sum(cols %in% pal_factor) >= 2
  tiene_parcela  <- any(cols %in% pal_parcela)
  tiene_fila     <- any(cols %in% pal_fila)
  tiene_columna  <- any(cols %in% pal_columna)
  tiene_ambiente <- any(cols %in% pal_ambiente)
  tiene_alpha    <- any(cols %in% pal_alpha)
  tiene_strip    <- any(cols %in% pal_strip)
  tiene_tiempo   <- any(cols %in% pal_tiempo)

  # Detectar diseño
  if (tiene_strip) {
    diseno <- "Strip-plot"
  } else if (tiene_tiempo & tiene_parcela & tiene_bloque) {
    diseno <- "Repeated Measures"
  } else if (tiene_trat & tiene_ambiente & tiene_bloque) {
    col_amb  <- cols[cols %in% pal_ambiente][1]
    col_trat <- cols[cols %in% pal_trat][1]
    col_blq  <- cols[cols %in% pal_bloque][1]
    n_amb    <- length(unique(na.omit(datos[[col_amb]])))

    if (!is.na(col_blq) && !is.na(col_amb)) {
      tab_blq_amb  <- table(datos[[col_amb]], datos[[col_blq]])
      bloques_anidados <- any(rowSums(tab_blq_amb > 0) < ncol(tab_blq_amb))
    } else {
      bloques_anidados <- FALSE
    }

    if (n_amb >= 2 && bloques_anidados) {
      diseno <- "Multiambiental (MET)"
    } else {
      diseno <- "DBCA"
    }
  } else if (tiene_alpha & tiene_bloque) {
    diseno <- "Alfa-latice"
  } else if (tiene_parcela & tiene_bloque) {
    diseno <- "Parcelas Divididas"
  } else if (tiene_fila & tiene_columna & tiene_trat) {
    diseno <- "Cuadrado Latino"
  } else if (tiene_factor & tiene_ambiente) {
    diseno <- "Factorial con Ambientes"
  } else if (tiene_multifactor & tiene_bloque) {
    diseno <- "Factorial"
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

