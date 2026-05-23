inkagro_auto <- function(datos, respuesta, tratamiento,
                         bloque = NULL, fa = NULL, fb = NULL,
                         rep = NULL, iblock = NULL,
                         gen = NULL, env = NULL) {

  # 1. Leer archivo si es ruta
  if (is.character(datos)) {
    datos_path <- datos
    extension  <- tolower(tools::file_ext(datos_path))

    if (extension == "csv") {
      datos <- tryCatch(
        suppressMessages(read.csv(datos_path)),
        error = function(e) suppressMessages(read.csv2(datos_path))
      )

    } else if (extension %in% c("xlsx", "xls")) {
      datos <- suppressMessages(readxl::read_excel(datos_path))
      # Si mas del 50% de nombres son genericos — releer con skip=1
      nombres_act    <- names(datos)
      prop_genericos <- mean(grepl("^\\.\\.\\.\\d+$", nombres_act))
      if (prop_genericos > 0.5) {
        datos <- suppressMessages(readxl::read_excel(datos_path, skip = 1))
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

    # Detectar titulo en cualquier formato
    nombres_act <- names(datos)
    if (all(grepl("^V\\d+$", nombres_act))) {
      primera_fila <- as.character(datos[1, ])
      names(datos) <- primera_fila
      datos <- datos[-1, ]
    }

    datos <- as.data.frame(datos)

    # Mostrar preview de datos cargados
    cat("=========================================\n")
    cat(" Datos cargados correctamente\n")
    cat(" Filas:", nrow(datos), "| Columnas:", ncol(datos), "\n")
    cat("=========================================\n\n")
    print(head(datos, 6))
    cat("\n")
  }

  # 2. Limpiar datos
  datos <- inkagro_clean(datos, verbose = FALSE)

  # 3. Detectar diseno
  diseno <- inkagro_detect(datos, verbose = FALSE)

  # 4. Analizar
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
                               verbose     = FALSE,
                               n_obs       = nrow(datos),
                               n_vars      = ncol(datos))

  return(invisible(resultado))
}
