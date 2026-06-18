
# Convierte p-value a simbolo de significancia estandar para articulos
.ink_signif <- function(p) {
  ifelse(is.na(p),  "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01,  "**",
                       ifelse(p < 0.05,  "*", "ns"))))
}

# Imprime separador de seccion
.ink_sep <- function(ancho = 64, doble = FALSE) {
  car <- if (doble) "=" else "-"
  cat(paste0(paste(rep(car, ancho), collapse = ""), "\n"))
}

# Calcula CV experimental desde modelo y datos
.ink_cv <- function(modelo, datos) {
  tryCatch({
    resp_col   <- as.character(formula(modelo)[[2]])
    residuos   <- residuals(modelo)
    media_gral <- mean(datos[[resp_col]], na.rm = TRUE)
    rmse       <- sqrt(mean(residuos^2))
    round((rmse / media_gral) * 100, 2)
  }, error = function(e) NA)
}

# Verifica si un modelo es mixto (lmer)
.ink_es_lmer <- function(modelo) {
  inherits(modelo, "lmerMod")
}

# Valida estructura del diseno detectado
.ink_validar_diseno <- function(datos, diseno, tratamiento, bloque = NULL) {
  if (diseno == "DBCA" && !is.null(bloque) && bloque %in% names(datos)) {
    tabla <- table(datos[[tratamiento]], datos[[bloque]])
    if (any(tabla == 0)) {
      message(
        "Nota: el diseno DBCA detectado tiene celdas vacias en la tabla ",
        "tratamiento x bloque. Se recomienda revisar la estructura de los datos.")
    }
  }
  if (diseno %in% c("DCA", "DBCA")) {
    n_min <- min(table(datos[[tratamiento]]))
    if (n_min < 2) {
      message(sprintf(
        "Nota: el tratamiento '%s' tiene niveles con menos de 2 observaciones.",
        tratamiento))
    }
  }
}
