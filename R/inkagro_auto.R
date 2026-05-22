inkagro_auto <- function(datos, respuesta, tratamiento,
                         bloque = NULL, fa = NULL, fb = NULL,
                         rep = NULL, iblock = NULL,
                         gen = NULL, env = NULL) {

  # 1. Leer archivo si es ruta
  if (is.character(datos)) {
    extension <- tolower(tools::file_ext(datos))
    if (extension == "csv") {
      datos <- tryCatch(read.csv(datos), error = function(e) read.csv2(datos))
    } else if (extension %in% c("xlsx", "xls")) {
      datos <- readxl::read_excel(datos)
    } else if (extension == "txt") {
      datos <- read.delim(datos)
    } else if (extension == "rds") {
      datos <- readRDS(datos)
    } else if (extension == "sav") {
      datos <- haven::read_sav(datos)
    } else if (extension == "dta") {
      datos <- haven::read_dta(datos)
    } else {
      stop("Formato no soportado. Use: csv, xlsx, xls, txt, rds, sav, dta")
    }
    datos <- as.data.frame(datos)
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
