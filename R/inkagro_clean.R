inkagro_clean <- function(datos, verbose = TRUE,
                          imputar = "media",
                          sinonimos = NULL) {

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
      x_num <- suppressWarnings(as.numeric(x))
      if (mean(is.na(x_num)) < 0.5) return(x_num)
      x_num2 <- suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", x)))
      patron_num_letra <- grepl("^[0-9\\.]+\\s*[a-zA-Z]+$", x)
      if (mean(patron_num_letra, na.rm = TRUE) > 0.3 &&
          mean(is.na(x_num2)) < 0.5) return(x_num2)
    }
    return(x)
  }))

  # 7. Estandarizar repeticiones
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) {
      x_clean <- tolower(trimws(x))
      x_clean <- gsub("^rep[a-z]*\\.?\\s*", "", x_clean)
      x_clean <- gsub("^r\\s*", "", x_clean)
      x_num   <- suppressWarnings(as.numeric(x_clean))
      if (mean(is.na(x_num)) < 0.3) return(x_num)
    }
    return(x)
  }))

  # 7.5 Imputar bloques faltantes por moda
  for (col in names(datos)) {
    if (col %in% pal_bloque && any(is.na(datos[[col]]))) {
      moda_bloque <- names(sort(table(datos[[col]]), decreasing = TRUE))[1]
      datos[[col]][is.na(datos[[col]])] <- moda_bloque
    }
  }

  # 7.6 Normalizar bloques y factores extrayendo numero final
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.character(x)) {
      x_norm      <- gsub("^[a-z_]+\\s*(\\d+)$", "\\1", x)
      x_num_check <- suppressWarnings(as.numeric(x_norm))
      if (mean(!is.na(x_num_check)) > 0.7 &&
          length(unique(x_norm)) < length(unique(x))) {
        return(x_norm)
      }
    }
    return(x)
  }))

  # 8. Tratar valores faltantes
  # NUNCA imputar variables continuas (respuesta, covariables)
  if (imputar != "ninguno") {
    col_names <- names(datos)
    datos_list <- lapply(col_names, function(col) {
      x <- datos[[col]]
      es_continua <- is.numeric(x) && length(unique(na.omit(x))) > 10
      if (es_continua) return(x)
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
    })
    datos           <- as.data.frame(datos_list)
    names(datos)    <- col_names
  }

  # 9. Convertir numericos con pocos niveles a factor
  # EXCEPTO variables continuas
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.numeric(x)) {
      n_unicos    <- length(unique(na.omit(x)))
      es_continua <- n_unicos > 10
      if (!es_continua && n_unicos <= 8) return(as.factor(x))
    }
    return(x)
  }))

  # 10. Agrupar niveles similares — solo texto, protegiendo columnas de tratamiento/variedad
  col_names_10  <- names(datos)
  datos_list_10 <- lapply(seq_along(col_names_10), function(idx) {
    x   <- datos[[idx]]
    col <- col_names_10[idx]

    if (is.character(x)) {
      niveles <- unique(na.omit(x))

      # Mapear abreviaturas de una sola letra
      niveles_freq <- sort(table(x), decreasing = TRUE)
      for (niv in niveles) {
        if (nchar(niv) == 1) {
          candidatos <- names(niveles_freq)[startsWith(names(niveles_freq), niv)]
          candidatos <- candidatos[nchar(candidatos) > 1]
          if (length(candidatos) > 0) x[x == niv] <- candidatos[1]
        }
      }
      niveles <- unique(na.omit(x))

      # Normalizar prefijos de nivel de factor
      if (all(nchar(niveles) <= 8, na.rm = TRUE)) {
        x <- gsub("^nivel(\\d+)$", "\\1", x)
        x <- gsub("^niv(\\d+)$",   "\\1", x)
        x <- gsub("^n-(\\d+)$",    "\\1", x)
        x <- gsub("^n(\\d+)$",     "\\1", x)
        niveles <- unique(na.omit(x))
      }

      # Normalizar prefijos de tratamiento
      if (all(nchar(niveles) <= 8, na.rm = TRUE)) {
        x <- gsub("^trat_",        "t",   x)
        x <- gsub("^trat",         "t",   x)
        x <- gsub("^t-",           "t",   x)
        x <- gsub("^alpha(\\d+)$", "\\1", x)
        x <- gsub("^a-(\\d+)$",   "\\1", x)
        x <- gsub("^a(\\d+)$",    "\\1", x)
        x <- gsub("^x-(\\d+)$",   "\\1", x)
        x <- gsub("^x(\\d+)$",    "\\1", x)
        x <- gsub("^y-(\\d+)$",   "\\1", x)
        x <- gsub("^ay(\\d+)$",   "\\1", x)
        x <- gsub("^y(\\d+)$",    "\\1", x)
        x <- gsub("^d-(\\d+)$",   "\\1", x)
        x <- gsub("^d(\\d+)$",    "\\1", x)
        niveles <- unique(na.omit(x))
      }

      # Normalizar prefijo t de tratamientos numericos
      x_sin_t <- gsub("^t(\\d+)$", "\\1", x)
      if (length(unique(na.omit(x_sin_t))) < length(unique(na.omit(x)))) {
        x       <- x_sin_t
        niveles <- unique(na.omit(x))
      }

      # Agrupar por distancia de cadenas — SOLO etiquetas cortas y columnas no-tratamiento.
      # Se omite en columnas de tratamiento/variedad/genotipo para evitar colapsar
      # genotipos o variedades con nombres similares (ej. irga_423 vs irga_424).
      es_col_trat <- col %in% pal_trat
      if (!es_col_trat && length(niveles) > 1 && length(niveles) <= 30) {
        for (i in seq_along(niveles)) {
          for (j in seq_along(niveles)) {
            if (i != j && niveles[i] %in% x && niveles[j] %in% x) {
              # Umbral conservador (0.12) solo para codigos cortos (<=6 chars):
              # captura errores tipograficos en etiquetas de bloque/factor,
              # sin riesgo de fusionar nombres de variedades mas largos.
              if (nchar(niveles[i]) <= 6 && nchar(niveles[j]) <= 6) {
                dist_jw <- stringdist::stringdist(niveles[i], niveles[j], method = "jw")
                if (dist_jw < 0.12) {
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
      }
      return(as.factor(x))
    }
    return(x)
  })
  names(datos_list_10) <- col_names_10
  datos <- as.data.frame(datos_list_10)

  # 10.5 Eliminar decimales largos infiltrados en columnas de factor
  datos <- as.data.frame(lapply(datos, function(x) {
    if (is.factor(x)) {
      niveles          <- levels(x)
      niveles_num      <- suppressWarnings(as.numeric(as.character(niveles)))
      niveles_corruptos <- niveles[!is.na(niveles_num) &
                                     grepl("\\.", as.character(niveles)) &
                                     nchar(as.character(niveles)) > 3]
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
        mapa        <- sinonimos[[col]]
        datos[[col]] <- as.character(datos[[col]])
        for (original in names(mapa)) {
          datos[[col]][datos[[col]] == original] <- mapa[[original]]
        }
        datos[[col]] <- as.factor(datos[[col]])
        message("Sinonimos aplicados en columna: ", col)
      }
    }
  }

  # 12. Recalcular NA
  na_reporte        <- colSums(is.na(datos))
  attr(datos, "na_reporte") <- na_reporte

  if (verbose) {
    na_tratados <- sum(is.na(datos))
    if (na_tratados == 0) {
      message("Limpieza completada. Sin valores faltantes.")
    } else {
      message("Limpieza completada. ", na_tratados, " valores faltantes restantes.")
    }
  }

  return(datos)
}
