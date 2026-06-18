inkagro_auto <- function(datos, respuesta, tratamiento,
                         bloque = NULL, fa = NULL, fb = NULL,
                         rep = NULL, iblock = NULL,
                         gen = NULL, env = NULL,
                         sinonimos = NULL) {
  # 1. Leer archivo si es ruta
  if (is.character(datos)) {
    datos_path <- datos
    extension  <- tolower(tools::file_ext(datos_path))

      if (extension == "csv") {
        linea_muestra <- readLines(datos_path, n = 2, warn = FALSE)[2]
        usar_csv2     <- grepl(";", linea_muestra)
        datos <- tryCatch(
          suppressMessages(
            if (usar_csv2) read.csv2(datos_path) else read.csv(datos_path)
          ),
          error = function(e) suppressMessages(read.csv2(datos_path))
        )

    } else if (extension %in% c("xlsx", "xls")) {
      datos <- suppressMessages(readxl::read_excel(datos_path))
      nombres_act    <- names(datos)
      prop_genericos <- mean(grepl("^\\.\\.\\.\\d+$", nombres_act))

      # Condicion 1: muchos nombres genericos (...1, ...2)
      if (prop_genericos > 0.5) {
        skip_n <- 1
        for (i in 1:5) {
          datos_temp   <- suppressMessages(readxl::read_excel(datos_path, skip = i))
          nombres_temp <- names(datos_temp)
          prop_gen     <- mean(grepl("^\\.\\.\\.\\d+$", nombres_temp))
          if (prop_gen <= 0.3) {
            skip_n <- i
            break
          }
        }
        datos <- suppressMessages(readxl::read_excel(datos_path, skip = skip_n))
      }

      # Condicion 2: headers son datos (IDs, fechas, simbolos)
      nombres_act <- names(datos)
      es_dato <- any(grepl(
        "^[0-9]{4}-[0-9]{2}|^[0-9]{2}/[0-9]{2}|^U[0-9]+|^P[0-9]+|^E[0-9]+|^R[0-9]+|^⚠|^\\.\\.\\.\\d+",
        nombres_act))
      if (es_dato) {
        for (i in 1:5) {
          datos_temp   <- suppressMessages(readxl::read_excel(datos_path, skip = i))
          nms          <- names(datos_temp)
          es_dato_temp <- any(grepl(
            "^[0-9]{4}-[0-9]{2}|^[0-9]{2}/[0-9]{2}|^U[0-9]+|^P[0-9]+|^E[0-9]+|^R[0-9]+|^⚠|^\\.\\.\\.\\d+",
            nms))
          if (!es_dato_temp) {
            datos <- datos_temp
            break
          }
        }
      }

    } else if (extension == "txt") {
      datos <- suppressMessages(read.delim(datos_path))
    } else if (extension == "rds") {
      datos <- readRDS(datos_path)
    } else if (extension == "sav") {
      datos <- suppressMessages(haven::read_sav(datos_path))
    } else if (extension == "dta") {
      datos <- suppressMessages(haven::read_dta(datos_path))
    } else {
      stop("Formato no soportado. Use: csv, xlsx, xls, txt, rds, sav, dta")
    }
    # Avisar si hay multiples hojas
    if (extension %in% c("xlsx", "xls")) {
      hojas <- readxl::excel_sheets(datos_path)
      if (length(hojas) > 1) {
        cli::cli_alert_warning(
          "El archivo tiene {length(hojas)} hojas: {paste(hojas, collapse=', ')}. Se leyo solo '{hojas[1]}'.")
        cli::cli_alert_info(
          "Para leer otra hoja: readxl::read_excel('archivo.xlsx', sheet = 'nombre')")
      }
    }
    # Detectar titulo en cualquier formato
    nombres_act <- names(datos)
    if (all(grepl("^V\\d+$", nombres_act))) {
      primera_fila <- as.character(datos[1, ])
      names(datos) <- primera_fila
      datos        <- datos[-1, ]
    }

    datos <- as.data.frame(datos)

    cat("=========================================\n")
    cat(" Datos cargados correctamente\n")
    cat(" Filas:", nrow(datos), "| Columnas:", ncol(datos), "\n")
    cat("=========================================\n\n")
    print(head(datos, 3))
    cat("\n")
  }

  # 2. Limpiar datos
  datos <- inkagro_clean(datos, verbose = FALSE, sinonimos = sinonimos)

  # 3. Detectar diseno UNA SOLA VEZ aqui
  diseno <- inkagro_detect(datos, verbose = FALSE)

  # 4. Analizar pasando el diseno ya detectado
  resultado <- inkagro_analyze(datos,
                               respuesta   = respuesta,
                               tratamiento = tratamiento,
                               bloque      = bloque,
                               fa          = fa,
                               fb          = fb,
                               rep         = rep,
                               iblock      = iblock,
                               gen         = gen,
                               env         = env,
                               diseno      = diseno,
                               verbose     = FALSE,
                               n_obs       = nrow(datos),
                               n_vars      = ncol(datos))

  # 5. Agregar metadatos al resultado para que inkagro_plot pueda usarlos
  resultado$datos       <- datos
  resultado$respuesta   <- tolower(trimws(respuesta))
  resultado$tratamiento <- tolower(trimws(tratamiento))
  resultado$fa          <- if (!is.null(fa))  tolower(trimws(fa))  else NULL
  resultado$fb          <- if (!is.null(fb))  tolower(trimws(fb))  else NULL
  resultado$env         <- if (!is.null(env)) tolower(trimws(env)) else NULL

  # 6. Generar graficos automaticamente
  resultado$graficos <- inkagro_plot(resultado)

  return(invisible(resultado))
}
