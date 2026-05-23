inkagro_clean <- function(datos, verbose = TRUE,
                          imputar = "media") {
  # 1. Verificación que los datos existen
  if (!is.data.frame(datos)) {
    stop("Los datos deben ser un data.frame")
  }
  # 2. Estandariza nombres de columnas
  nombres_originales <- names(datos)
  names(datos) <- tolower(trimws(names(datos)))
  names(datos) <- gsub(" ", "_", names(datos))
  # 3. Elimina espacios en blanco en columnas de texto
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) trimws(x) else x
  }))
  # 4. Reportar valores faltantes
  na_reporte <- colSums(is.na(datos))
  attr(datos, "na_reporte") <- na_reporte
  # 5. Estandarizar valores de texto en minúscula
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) gsub("\\s+", "_", tolower(trimws(x))) else x
  }))
  # 6. Convertir columnas numericas que llegaron como texto
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) {
      x_num <- suppressWarnings(as.numeric(x))
      if (mean(is.na(x_num)) < 0.5) {
        return(x_num)
      }
    }
    return(x)
  }))
  # 7. Tratar valores faltantes
  if (imputar != "ninguno") {
    datos <- as.data.frame(lapply(datos, function(x) {
      if (is.numeric(x) && any(is.na(x))) {
        if (imputar == "media") {
          x[is.na(x)] <- round(mean(x, na.rm = TRUE), 2)
        } else if (imputar == "mediana") {
          x[is.na(x)] <- round(median(x, na.rm = TRUE), 2)
        } else if (imputar == "moda") {
          moda <- names(sort(table(x), decreasing = TRUE))[1]
          x[is.na(x)] <- as.numeric(moda)
        }
      } else if ((is.character(x) | is.factor(x)) && any(is.na(x))) {
        moda <- names(sort(table(x), decreasing = TRUE))[1]
        x[is.na(x)] <- moda
      }
      return(x)
    }))
  }
  # 8. Convertir numericos con pocos niveles a factor
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.numeric(x) && length(unique(na.omit(x))) <= 8) {
      return(as.factor(x))
    }
    return(x)
  }))
  if (verbose) {
    na_tratados <- sum(is.na(datos))
    if (na_tratados == 0) {
      message("Limpieza completada. Valores faltantes imputados correctamente.")
    } else {
      message("Limpieza completada. ", na_tratados, " valores faltantes no pudieron ser imputados.")
      return(datos)
    }
  }
}
