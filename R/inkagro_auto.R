inkagro_auto <- function(datos, respuesta, tratamiento,
                         bloque = NULL, fa = NULL, fb = NULL,
                         rep = NULL, iblock = NULL,
                         gen = NULL, env = NULL){

  # 1. Leer datos si es una ruta de archivo
  if (is.character(datos)) {
    extension <- tolower(tools::file_ext(datos))

    if (extension == "csv") {
      datos <- tryCatch(
        read.csv(datos),
        error = function(e) read.csv2(datos)
      )
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
    message("Archivo cargado: ", nrow(datos), " filas x ", ncol(datos), " columnas.")
  }
# 2. Pipeline automatico
cat("=========================================\n")
cat(" InkAgro v0.1.0\n")
cat(" Pipeline automatico de analisis agricola\n")
cat("=========================================\n\n")

cat(" Paso 1: Limpiando datos...\n")
datos <- inkagro_clean(datos)

cat(" Paso 2: Detectando diseno experimental...\n")
diseno <- inkagro_detect(datos)

cat(" Paso 3: Analizando...\n\n")
resultado <- inkagro_analyze(datos,
                             respuesta   = respuesta,
                             tratamiento = tratamiento,
                             bloque      = bloque,
                             fa          = fa,
                             fb          = fb,
                             rep         = rep,
                             iblock      = iblock,
                             gen         = gen,
                             env         = env)

return(invisible(resultado))

}
