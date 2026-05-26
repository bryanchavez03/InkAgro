inkagro_clean <- function(datos, verbose = TRUE,
                          imputar = "media",
                          sinonimos = NULL) {

  # 1. Verificar que los datos existen
  if (!is.data.frame(datos)) {
    stop("Los datos deben ser un data.frame")
  }

  # 2. Estandarizar nombres de columnas
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

  # 5. Estandarizar valores de texto en minuscula
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) gsub("\\s+", "_", tolower(trimws(x))) else x
  }))

  # 6. Convertir columnas numericas que llegaron como texto
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) {
      x_num <- suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", x)))
      if (mean(is.na(x_num)) < 0.5) {
        return(x_num)
      }
    }
    return(x)
  }))

  # 7. Estandarizar repeticiones
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
  # 7.5 Imputar bloques faltantes por moda antes de la imputacion general
  for (col in names(datos)) {
    if (col %in% pal_bloque && any(is.na(datos[[col]]))) {
      moda_bloque <- names(sort(table(datos[[col]]), decreasing = TRUE))[1]
      datos[[col]][is.na(datos[[col]])] <- moda_bloque
    }
  }
  # 8. Tratar valores faltantes
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

  # 9. Convertir numericos con pocos niveles a factor
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.numeric(x) && length(unique(na.omit(x))) <= 8) {
      return(as.factor(x))
    }
    return(x)
  }))

  # 10. Agrupar niveles similares automaticamente
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x) | is.factor(x)) {
      x <- as.character(x)
      niveles <- unique(na.omit(x))

      # Mapear abreviaturas de una sola letra
      niveles_freq <- sort(table(x), decreasing = TRUE)
      for (niv in niveles) {
        if (nchar(niv) == 1) {
          candidatos <- names(niveles_freq)[startsWith(names(niveles_freq), niv)]
          candidatos <- candidatos[nchar(candidatos) > 1]
          if (length(candidatos) > 0) {
            x[x == niv] <- candidatos[1]
          }
        }
      }

      # Actualizar niveles
      niveles <- unique(na.omit(x))

      # Agrupar por distancia de cadenas
      if (length(niveles) > 1 && length(niveles) <= 30) {
        for (i in seq_along(niveles)) {
          for (j in seq_along(niveles)) {
            if (i != j && niveles[i] %in% x && niveles[j] %in% x) {
              dist <- stringdist::stringdist(niveles[i], niveles[j],
                                             method = "jw")
              if (dist < 0.25) {
                freq_i <- sum(x == niveles[i], na.rm = TRUE)
                freq_j <- sum(x == niveles[j], na.rm = TRUE)
                if (freq_i >= freq_j) {
                  x[x == niveles[j]] <- niveles[i]
                } else {
                  x[x == niveles[i]] <- niveles[j]
                }
              }
            }
          }
        }
      }
      return(as.factor(x))
    }
    return(x)
  }))

  # 10.5 Eliminar valores numericos infiltrados en columnas de factor
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.factor(x)) {
      niveles <- levels(x)
      niveles_num <- suppressWarnings(as.numeric(as.character(niveles)))
      niveles_corruptos <- niveles[!is.na(niveles_num) &
                                     nchar(as.character(niveles)) > 1]
      if (length(niveles_corruptos) > 0) {
        x[x %in% niveles_corruptos] <- NA
        x <- droplevels(x)
      }
    }
    return(x)
  }))

  # 11. Aplicar sinonimos definidos por el usuario
  if (!is.null(sinonimos)) {
    for (col in names(sinonimos)) {
      if (col %in% names(datos)) {
        mapa <- sinonimos[[col]]
        datos[[col]] <- as.character(datos[[col]])
        for (original in names(mapa)) {
          datos[[col]][datos[[col]] == original] <- mapa[[original]]
        }
        datos[[col]] <- as.factor(datos[[col]])
        message("Sinonimos aplicados en columna: ", col)
      }
    }
  }

  # 12. Recalcular NA despues de imputacion
  na_reporte <- colSums(is.na(datos))
  attr(datos, "na_reporte") <- na_reporte

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
