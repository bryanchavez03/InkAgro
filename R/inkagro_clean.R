inkagro_clean <- function(datos, verbose = TRUE) {
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
      if (sum(is.na(x_num)) < sum(is.na(x)) + 0.5 * length(x)) {
        return(x_num)
      }
    }
    return(x)
  }))

  if (verbose) message("Limpieza completada.")
  return(datos)
}
