inkagro_clean <- function(datos, verbose = TRUE,
                          imputar = "media") {
  # 1. Verificar que los datos existen
  if (!is.data.frame(datos)) {
    stop("Los datos deben ser un data.frame")
  }

  # 2. Estandarizar nombres de columnas
  nombres_originales <- names(datos)
  names(datos) <- tolower(trimws(names(datos)))
  names(datos) <- gsub(" ", "_", names(datos))

  # 3. Eliminar espacios en blanco en columnas de texto
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) trimws(x) else x
  }))

  # 4. Convertir strings de NA a NA real
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) {
      x[trimws(tolower(x)) %in% c("na", "n/a", "none", "null",
                                  "missing", "", "nd", ".", "-")] <- NA
    }
    return(x)
  }))

  # 5. Reportar valores faltantes
  na_reporte <- colSums(is.na(datos))
  attr(datos, "na_reporte") <- na_reporte

  # 6. Estandarizar valores de texto en minuscula
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) gsub("\\s+", "_", tolower(trimws(x))) else x
  }))

  # 7. Convertir columnas numericas que llegaron como texto
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) {
      x_num <- suppressWarnings(as.numeric(x))
      if (mean(is.na(x_num)) < 0.5) {
        return(x_num)
      }
    }
    return(x)
  }))

  # 8. Estandarizar repeticiones
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) {
      x_clean <- tolower(trimws(x))
      x_clean <- gsub("^rep[a-z]*\\.?\\s*", "", x_clean)
      x_clean <- gsub("^r\\s*", "", x_clean)
      x_num <- suppressWarnings(as.numeric(x_clean))
      if (mean(is.na(x_num)) < 0.3) {
        return(x_num)
      }
    }
    return(x)
  }))

  # 9. Tratar valores faltantes
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

  # 10. Convertir numericos con pocos niveles a factor
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.numeric(x) && length(unique(na.omit(x))) <= 8) {
      return(as.factor(x))
    }
    return(x)
  }))

  # 11. Advertir niveles inconsistentes en factores
  for (col in names(datos)) {
    if (is.character(datos[[col]]) | is.factor(datos[[col]])) {
      niveles <- unique(as.character(datos[[col]]))
      niveles_lower <- tolower(niveles)
      if (length(niveles) != length(unique(niveles_lower))) {
        message("ADVERTENCIA: Niveles posiblemente inconsistentes en '",
                col, "' — revise mayusculas/minusculas")
      }
    }
  }

  if (verbose) {
    na_tratados <- sum(is.na(datos))
    if (na_tratados == 0) {
      message("Limpieza completada. Valores faltantes imputados correctamente.")
    } else {
      message("Limpieza completada. ", na_tratados,
              " valores faltantes no pudieron ser imputados.")
    }
  }

  return(datos)
}
