#' Ordenar una tabla de campo antes del análisis
#'
#' Lee un Excel, CSV o `data.frame` armado "a mano" y lo deja como tabla
#' larga y limpia: una fila por parcela y una columna por variable. Corrige la
#' **estructura** de la tabla y anota cada cambio; lo que no puede decidir con
#' seguridad (valores que no son números, posibles comas mal puestas) no lo
#' cambia, sino que lo deja en la lista de cosas por revisar.
#'
#' @section Qué corrige:
#' \itemize{
#'   \item Filas de título o vacías antes del encabezado.
#'   \item Encabezados en dos filas (por ejemplo "Rendimiento" sobre "I II III").
#'   \item Unidades en el encabezado: `Rendimiento (t/ha)` pasa a llamarse
#'     `rendimiento` y la unidad se guarda en `unidades`.
#'   \item Filas de resumen: Total, Promedio, Media, Suma, CV, desviación,
#'     máximo, mínimo, y notas al pie como "Fuente: ...".
#'   \item Celdas combinadas en columnas de tratamiento o bloque (el nombre
#'     escrito una vez y vacío debajo): se rellena hacia abajo.
#'   \item Formato ancho: bloques o repeticiones como columnas (`I`, `II`,
#'     `III`, `R1`, `R2`, `Bloque 1`...) se pasan a formato largo con una
#'     columna `bloque`.
#'   \item Coma decimal y números guardados como texto.
#'   \item Etiquetas que solo difieren en mayúsculas o espacios (`T1`, `t1`,
#'     `T 1`).
#' }
#'
#' @section Qué nunca hace:
#' No rellena datos faltantes de la variable medida, no borra valores
#' atípicos y no junta nombres de variedades parecidos. Esos casos van a
#' `revisar`.
#'
#' @param datos Ruta a un archivo `.xlsx`, `.xls`, `.csv`, `.txt` o `.tsv`, o
#'   un `data.frame`.
#' @param hoja Hoja del Excel (nombre o número). Por defecto, la primera; si
#'   hay varias, se avisa en `revisar`.
#' @param variable Nombre para la variable medida cuando la tabla está en
#'   formato ancho y las columnas de bloques no tienen un encabezado encima
#'   (por ejemplo `variable = "rendimiento"`). Si es `NULL` se llama `valor`.
#' @return Un objeto de clase `inkagro_preparado`: una lista con `datos` (la
#'   tabla ordenada), `cambios` (cada corrección hecha), `revisar` (lo que
#'   conviene verificar a mano), `unidades` (unidades leídas de los
#'   encabezados) y `origen`. Se puede pasar directamente a [inkagro()] y
#'   guardar con [exportar()].
#' @examples
#' # Tabla en formato ancho, con fila de promedio y coma decimal
#' crudo <- data.frame(
#'   Variedad = c("INIA 1", "inia 1", "Canchan", "Promedio"),
#'   I   = c("4,5", "4,1", "3,2", "3,9"),
#'   II  = c("4,8", "4,4", "3,0", "4,1"),
#'   III = c("4,6", "4,0", "s/d", "4,3"),
#'   check.names = FALSE)
#' p <- preparar(crudo, variable = "rendimiento")
#' p
#' p$datos
#' @export
preparar <- function(datos, hoja = NULL, variable = NULL) {
  reg <- .ink_registro()
  lectura <- .ink_leer_crudo(datos, hoja)
  m <- lectura$matriz
  origen <- lectura$origen
  for (nota in lectura$avisos) reg$revisar(NA, NA, NA, nota)

  # Numero de fila tal como se ve en Excel (1 = primera fila de la hoja)
  fila_excel <- seq_len(nrow(m)) + lectura$desfase

  # 1. Filas y columnas completamente vacias
  vacia <- function(z) is.na(z) | !nzchar(trimws(z))
  m[vacia(m)] <- NA
  filas_ok <- rowSums(!is.na(m)) > 0L
  cols_ok  <- colSums(!is.na(m)) > 0L
  if (sum(!cols_ok) > 0L) {
    reg$cambio(NA, NA, "Columnas vac\u00edas", sprintf("Se quitaron %d columna(s) sin datos.", sum(!cols_ok)))
  }
  m <- m[filas_ok, cols_ok, drop = FALSE]
  fila_excel <- fila_excel[filas_ok]
  if (nrow(m) < 2L || ncol(m) < 2L) {
    stop("La tabla tiene menos de 2 filas o 2 columnas con datos.", call. = FALSE)
  }

  # 2. Encabezado (puede ocupar una o dos filas)
  enc <- .ink_buscar_encabezado(m)
  if (enc$fila > 1L) {
    titulo <- apply(m[seq_len(enc$fila - 1L), , drop = FALSE], 1L,
                    function(f) paste(stats::na.omit(f), collapse = " "))
    reg$cambio(fila_excel[seq_len(enc$fila - 1L)], NA, "T\u00edtulo",
               paste0("Fila(s) antes del encabezado quitadas: ",
                      paste0("'", titulo, "'", collapse = "; ")))
  }
  arriba <- m[enc$fila, ]
  abajo  <- if (enc$dos_filas) m[enc$fila + 1L, ] else rep(NA_character_, ncol(m))
  if (enc$dos_filas) {
    reg$cambio(fila_excel[enc$fila + 0:1], NA, "Encabezado en dos filas",
               "Se combinaron las dos filas del encabezado en una.")
  }
  inicio <- enc$fila + if (enc$dos_filas) 2L else 1L
  if (inicio > nrow(m)) stop("No hay filas de datos debajo del encabezado.", call. = FALSE)
  cuerpo <- m[inicio:nrow(m), , drop = FALSE]
  fila_excel <- fila_excel[inicio:nrow(m)]

  # Encabezado padre: una celda combinada horizontalmente ("Rendimiento"
  # sobre I, II, III) se propaga a la derecha.
  padre <- arriba
  if (enc$dos_filas) {
    for (j in seq_along(padre)[-1L]) {
      if (is.na(padre[j]) && !is.na(abajo[j])) padre[j] <- padre[j - 1L]
    }
  }
  crudo_nombre <- ifelse(!is.na(abajo), abajo, arriba)
  nombres <- .ink_nombres(crudo_nombre, padre, enc$dos_filas)
  unidades <- nombres$unidades
  # Solo se anotan los cambios de nombre que no son obvios (no basta con
  # pasar a minusculas o quitar tildes): unidades separadas o nombres repetidos.
  for (k in seq_along(nombres$nombre)) {
    o <- nombres$original[k]
    if (is.na(o) || identical(.ink_nombre_columna(o), nombres$nombre[k])) next
    if (!is.na(.ink_patron_bloque(nombres$nombre[k]))) next
    reg$cambio(NA, nombres$nombre[k], "Nombre de columna",
               sprintf("'%s' -> '%s'%s", o, nombres$nombre[k],
                       if (!is.na(unidades[k])) paste0(" (unidad: ", unidades[k], ")") else ""))
  }
  colnames(cuerpo) <- nombres$nombre

  # 3. Filas de resumen y notas al pie
  clase <- .ink_es_resumen(cuerpo)
  resumen <- clase == "resumen"
  if (any(resumen)) {
    etiquetas <- apply(cuerpo[resumen, , drop = FALSE], 1L, function(f) stats::na.omit(f)[1L])
    reg$cambio(fila_excel[resumen], NA, "Fila de resumen",
               paste0("Se quit\u00f3 la fila '", etiquetas, "' (no es una parcela)."))
  }
  # Notas: filas que empiezan con "Fuente:", "Nota:"..., o texto suelto en
  # una sola celda al final de la tabla.
  nota <- !resumen & (clase == "nota" | (rowSums(!is.na(cuerpo)) == 1L &
    seq_len(nrow(cuerpo)) > max(which(rowSums(!is.na(cuerpo)) > 1L), 0L)))
  if (any(nota)) {
    texto <- apply(cuerpo[nota, , drop = FALSE], 1L, function(f) stats::na.omit(f)[1L])
    reg$cambio(fila_excel[nota], NA, "Nota al pie",
               paste0("Se quit\u00f3 la nota '", texto, "'."))
  }
  quitar <- resumen | nota
  cuerpo <- cuerpo[!quitar, , drop = FALSE]
  fila_excel <- fila_excel[!quitar]
  if (nrow(cuerpo) == 0L) stop("No quedaron filas de datos.", call. = FALSE)

  d <- as.data.frame(cuerpo, stringsAsFactors = FALSE)
  names(d) <- colnames(cuerpo)
  for (j in seq_along(d)) d[[j]] <- gsub("\\s+", " ", trimws(d[[j]]))

  # 4. Tipo de cada columna: numerica o de etiquetas
  es_num <- vapply(d, .ink_parece_numerica, logical(1))

  # 5. Celdas combinadas en columnas de etiquetas: rellenar hacia abajo
  for (col in names(d)[!es_num]) {
    x <- d[[col]]
    otras_con_datos <- rowSums(!is.na(d[, setdiff(names(d), col), drop = FALSE])) > 0L
    huecos <- which(is.na(x) & otras_con_datos)
    huecos <- huecos[huecos > 1L]
    if (!length(huecos) || all(is.na(x))) next
    # Solo si los huecos siguen a un valor (patron de celda combinada) y la
    # columna repite valores en bloques contiguos.
    rellenado <- x
    for (i in seq_along(rellenado)[-1L]) {
      if (is.na(rellenado[i]) && otras_con_datos[i]) rellenado[i] <- rellenado[i - 1L]
    }
    if (anyNA(rellenado[otras_con_datos][-1L]) && is.na(rellenado[otras_con_datos][1L])) next
    cambiados <- which(is.na(x) & !is.na(rellenado))
    if (length(cambiados)) {
      d[[col]] <- rellenado
      reg$cambio(fila_excel[cambiados], col, "Celda combinada",
                 sprintf("Se complet\u00f3 '%s' con el valor de la fila de arriba.", col))
    }
  }

  # 6. Numeros: coma decimal, texto, unidades pegadas
  coma_n <- 0L
  coma_cols <- character()
  for (col in names(d)[es_num]) {
    conv <- .ink_convertir_numeros(d[[col]])
    if (length(conv$coma)) {
      coma_n <- coma_n + length(conv$coma)
      coma_cols <- c(coma_cols, col)
    }
    if (length(conv$unidad)) {
      reg$cambio(fila_excel[conv$unidad], col, "Unidad pegada al n\u00famero",
                 paste0("'", d[[col]][conv$unidad], "' -> ", conv$valor[conv$unidad]))
    }
    for (i in conv$malos) {
      reg$revisar(fila_excel[i], col, d[[col]][i], paste0(
        "No es un n\u00famero; qued\u00f3 como dato faltante. Corr\u00edgelo en el ",
        "archivo si era un valor real."))
    }
    if (length(conv$miles)) {
      reg$revisar(fila_excel[conv$miles], col, d[[col]][conv$miles], paste0(
        "Coma seguida de tres cifras: se ley\u00f3 como decimal. Si era separador ",
        "de miles, corrige el archivo."))
    }
    d[[col]] <- conv$valor
  }
  if (coma_n > 0L) {
    reg$cambio(NA, paste(coma_cols, collapse = ", "), "Coma decimal", sprintf(
      "Se ley\u00f3 la coma como decimal en %d valor(es) (columnas %s).",
      coma_n, paste0("'", coma_cols, "'", collapse = ", ")))
  }

  # 7. Formato ancho: bloques o repeticiones como columnas
  ancho <- .ink_detectar_ancho(names(d), es_num, nombres$padre)
  if (!is.null(ancho) && !is.null(variable)) {
    for (k in seq_along(ancho$grupos)) {
      if (identical(ancho$grupos[[k]]$variable, "valor")) {
        ancho$grupos[[k]]$variable <- .ink_nombre_columna(variable)
      }
    }
  }
  if (!is.null(ancho)) {
    largo <- .ink_a_largo(d, ancho, fila_excel)
    reg$cambio(NA, NA, "Formato ancho a largo", sprintf(paste0(
      "Las columnas %s eran %s; se pas\u00f3 a una fila por parcela con la ",
      "columna 'bloque'."),
      paste0("'", unlist(lapply(ancho$grupos, `[[`, "columnas")), "'", collapse = ", "),
      "bloques o repeticiones"))
    for (g in ancho$grupos) {
      if (identical(g$variable, "valor")) {
        reg$revisar(NA, "valor", NA, paste0(
          "Las columnas de bloques no ten\u00edan un nombre de variable encima; ",
          "se llam\u00f3 'valor'. C\u00e1mbialo por el nombre real (por ejemplo, rendimiento)."))
      }
    }
    d <- largo$datos
    fila_excel <- largo$fila_excel
    for (g in ancho$grupos) {
      u <- stats::na.omit(unidades[g$columnas])
      if (length(u)) unidades[[g$variable]] <- u[[1L]]
    }
    es_num <- vapply(d, is.numeric, logical(1))
  } else {
    sospechosas <- names(d)[es_num & grepl("^x?[0-9]+$", names(d))]
    if (length(sospechosas) >= 2L) {
      reg$revisar(NA, paste(sospechosas, collapse = ", "), NA, paste0(
        "Hay columnas con n\u00fameros por nombre. Si son bloques o repeticiones, ",
        "ren\u00f3mbralas como I, II, III o R1, R2, R3 y vuelve a preparar; si son ",
        "dosis o fechas, la tabla est\u00e1 en formato ancho por tratamiento."))
    }
  }

  # 8. Etiquetas que solo difieren en mayusculas o espacios
  for (col in names(d)[!es_num]) {
    u <- .ink_unificar(d[[col]])
    if (length(u$cambios)) {
      d[[col]] <- u$x
      reg$cambio(NA, col, "Etiquetas unificadas", paste(u$cambios, collapse = "; "))
    }
  }

  # 9. Avisos que no se corrigen: valores muy lejos de la mediana, duplicados
  for (col in names(d)[vapply(d, is.numeric, logical(1))]) {
    for (i in .ink_sospechosos(d[[col]])) {
      reg$revisar(fila_excel[i], col, as.character(d[[col]][i]), paste0(
        "Valor muy alejado del resto (unas 10 veces mayor o menor que la ",
        "mediana). \u00bfComa mal puesta o error de tipeo? No se cambi\u00f3."))
    }
  }
  dup <- duplicated(d) | duplicated(d, fromLast = TRUE)
  if (any(dup)) {
    reg$revisar(fila_excel[dup], NA, NA, "Filas id\u00e9nticas: \u00bfse copi\u00f3 una fila dos veces?")
  }

  rownames(d) <- NULL
  structure(
    list(datos = d, cambios = reg$tabla_cambios(), revisar = reg$tabla_revisar(),
         unidades = unidades[!is.na(unidades) & names(unidades) %in% names(d)],
         fila_excel = fila_excel, origen = origen),
    class = "inkagro_preparado")
}

#' @noRd
#' @export
print.inkagro_preparado <- function(x, ...) {
  cat("\nInkAgro | Tabla preparada desde ", x$origen, "\n", strrep("-", 66), "\n", sep = "")
  cat(nrow(x$datos), " filas x ", ncol(x$datos), " columnas: ",
      paste(names(x$datos), collapse = ", "), "\n", sep = "")
  if (length(x$unidades)) {
    cat("Unidades: ", paste0(names(x$unidades), " (", x$unidades, ")", collapse = ", "), "\n", sep = "")
  }
  cat("\nCambios hechos (", nrow(x$cambios), ")\n", sep = "")
  if (nrow(x$cambios)) {
    for (i in seq_len(nrow(x$cambios))) {
      cat(.ink_envolver(paste0(x$cambios$tipo[i], ": ", x$cambios$detalle[i]), "  - "), "\n", sep = "")
    }
  } else {
    cat("  Ninguno: la tabla ya estaba ordenada.\n")
  }
  cat("\nPor revisar (", nrow(x$revisar), ")\n", sep = "")
  if (nrow(x$revisar)) {
    for (i in seq_len(nrow(x$revisar))) {
      donde <- c(if (!is.na(x$revisar$fila[i])) paste0("fila ", x$revisar$fila[i]),
                 if (!is.na(x$revisar$columna[i])) paste0("'", x$revisar$columna[i], "'"),
                 if (!is.na(x$revisar$valor[i])) paste0("valor '", x$revisar$valor[i], "'"))
      cat(.ink_envolver(paste0(if (length(donde)) paste0("[", paste(donde, collapse = ", "), "] "),
                               x$revisar$motivo[i]), "  ! "), "\n", sep = "")
    }
  } else {
    cat("  Nada.\n")
  }
  cat("\nPrimeras filas:\n")
  print(utils::head(x$datos, 6L), row.names = FALSE)
  cat("\nGu\u00e1rdala con exportar(x, \"datos_ordenados.xlsx\") o anal\u00edzala con inkagro(x, ...).\n\n")
  invisible(x)
}

#' @noRd
#' @export
as.data.frame.inkagro_preparado <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$datos
}

#' Guardar una tabla preparada en Excel y CSV
#'
#' Escribe un Excel con tres hojas (`datos`, `cambios` y `revisar`) para que
#' se pueda verificar cada corrección, y un CSV con solo los datos, listo
#' para cualquier programa.
#'
#' Requiere el paquete 'writexl' para el Excel.
#'
#' @param x Objeto devuelto por [preparar()].
#' @param archivo Ruta del Excel a crear (`.xlsx`).
#' @param csv Si es `TRUE` (por defecto), también guarda un `.csv` con el
#'   mismo nombre.
#' @return Las rutas creadas, de forma invisible.
#' @examples
#' \donttest{
#' if (requireNamespace("writexl", quietly = TRUE)) {
#'   crudo <- data.frame(Tratamiento = c("T1", "T2", "Total"),
#'                       I = c("4,5", "3,2", "7,7"), II = c("4,8", "3,0", "7,8"))
#'   p <- preparar(crudo)
#'   exportar(p, file.path(tempdir(), "datos_ordenados.xlsx"))
#' }
#' }
#' @export
exportar <- function(x, archivo = "datos_ordenados.xlsx", csv = TRUE) {
  if (!inherits(x, "inkagro_preparado")) {
    stop("'x' debe ser el resultado de preparar().", call. = FALSE)
  }
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Para guardar en Excel instala el paquete 'writexl': install.packages(\"writexl\")",
         call. = FALSE)
  }
  if (!grepl("\\.xlsx$", archivo, ignore.case = TRUE)) archivo <- paste0(archivo, ".xlsx")
  vacio <- function(df, texto) {
    if (nrow(df)) return(df)
    out <- df[1L, , drop = FALSE]
    out[1L, ] <- NA
    out[1L, ncol(out)] <- texto
    out
  }
  hojas <- list(datos = x$datos,
                cambios = vacio(x$cambios, "Sin cambios"),
                revisar = vacio(x$revisar, "Nada por revisar"))
  writexl::write_xlsx(hojas, archivo)
  rutas <- archivo
  if (csv) {
    ruta_csv <- sub("\\.xlsx$", ".csv", archivo, ignore.case = TRUE)
    # CSV en UTF-8 con marca BOM al inicio: asi Excel muestra bien las tildes.
    tmp <- tempfile(fileext = ".csv")
    utils::write.table(x$datos, tmp, sep = ",", dec = ".", row.names = FALSE,
                       na = "", qmethod = "double", fileEncoding = "UTF-8")
    contenido <- readBin(tmp, "raw", file.size(tmp))
    unlink(tmp)
    writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), contenido), ruta_csv)
    rutas <- c(rutas, ruta_csv)
  }
  message("Guardado: ", paste(rutas, collapse = " y "))
  invisible(rutas)
}

# ---- funciones internas ----------------------------------------------------

.ink_registro <- function() {
  cambios <- list()
  revisar <- list()
  list(
    cambio = function(filas, columna, tipo, detalle, resumir = FALSE) {
      filas <- filas[!is.na(filas)]
      if (length(detalle) > 1L && length(detalle) == length(filas)) {
        for (k in seq_along(filas)) {
          cambios[[length(cambios) + 1L]] <<- data.frame(
            fila = as.character(filas[k]), columna = as.character(columna),
            tipo = tipo, detalle = detalle[k], stringsAsFactors = FALSE)
        }
        return(invisible())
      }
      cambios[[length(cambios) + 1L]] <<- data.frame(
        fila = if (length(filas)) .ink_lista(filas, 15L) else NA_character_,
        columna = as.character(columna), tipo = tipo, detalle = detalle[1L],
        stringsAsFactors = FALSE)
    },
    revisar = function(filas, columna, valor, motivo) {
      if (length(filas) > 1L) {
        for (k in seq_along(filas)) {
          revisar[[length(revisar) + 1L]] <<- data.frame(
            fila = filas[k], columna = as.character(columna),
            valor = as.character(if (length(valor) > 1L) valor[k] else valor),
            motivo = motivo, stringsAsFactors = FALSE)
        }
        return(invisible())
      }
      revisar[[length(revisar) + 1L]] <<- data.frame(
        fila = as.integer(filas), columna = as.character(columna),
        valor = as.character(valor), motivo = motivo, stringsAsFactors = FALSE)
    },
    tabla_cambios = function() {
      if (!length(cambios)) {
        return(data.frame(fila = character(), columna = character(), tipo = character(),
                          detalle = character(), stringsAsFactors = FALSE))
      }
      do.call(rbind, cambios)
    },
    tabla_revisar = function() {
      if (!length(revisar)) {
        return(data.frame(fila = integer(), columna = character(), valor = character(),
                          motivo = character(), stringsAsFactors = FALSE))
      }
      do.call(rbind, revisar)
    }
  )
}

# Lee el archivo sin interpretar nada: una matriz de texto con todas las
# celdas, incluido el encabezado.
.ink_leer_crudo <- function(datos, hoja) {
  avisos <- character()
  if (is.data.frame(datos)) {
    df <- as.data.frame(datos, stringsAsFactors = FALSE)
    cuerpo <- vapply(df, function(z) as.character(z), character(nrow(df)))
    if (!is.matrix(cuerpo)) cuerpo <- matrix(cuerpo, nrow = nrow(df))
    m <- rbind(names(df), cuerpo)
    # Los data.frame no tienen titulo: la fila 1 es el encabezado
    return(list(matriz = unname(m), origen = "un data.frame", desfase = 0L, avisos = avisos))
  }
  if (!is.character(datos) || length(datos) != 1L) {
    stop("'datos' debe ser un data.frame o la ruta a un archivo.", call. = FALSE)
  }
  if (!file.exists(datos)) stop(.ink_no_encontrado(datos), call. = FALSE)
  ext <- tolower(tools::file_ext(datos))
  if (ext %in% c("xlsx", "xls")) {
    hojas <- readxl::excel_sheets(datos)
    if (is.null(hoja)) {
      hoja <- hojas[1L]
      if (length(hojas) > 1L) {
        avisos <- c(avisos, sprintf(paste0(
          "El archivo tiene %d hojas (%s); se ley\u00f3 '%s'. Para otra, usa ",
          "preparar(archivo, hoja = \"nombre\")."),
          length(hojas), paste(hojas, collapse = ", "), hoja))
      }
    }
    # Con 'range' desde A1, readxl conserva las filas vacias del inicio y la
    # numeracion coincide con la de Excel.
    m <- suppressMessages(readxl::read_excel(
      datos, sheet = hoja, col_names = FALSE, col_types = "text",
      range = readxl::cell_limits(c(1L, 1L), c(NA, NA)), .name_repair = "minimal"))
    m <- as.matrix(as.data.frame(m, stringsAsFactors = FALSE))
    return(list(matriz = unname(m), origen = paste0(basename(datos), " [", hoja, "]"),
                desfase = 0L, avisos = avisos))
  }
  if (ext %in% c("csv", "txt", "tsv")) {
    lineas <- readLines(datos, warn = FALSE, encoding = "UTF-8")
    codificacion <- if (all(validUTF8(lineas))) "UTF-8" else "latin1"
    lineas <- readLines(datos, warn = FALSE, encoding = codificacion)
    lineas <- sub("^\ufeff", "", lineas)
    muestra <- lineas[nzchar(lineas)][seq_len(min(20L, sum(nzchar(lineas))))]
    sep <- if (ext == "tsv") "\t" else {
      cuenta <- c(";" = sum(lengths(regmatches(muestra, gregexpr(";", muestra)))),
                  "," = sum(lengths(regmatches(muestra, gregexpr(",", muestra)))),
                  "\t" = sum(lengths(regmatches(muestra, gregexpr("\t", muestra)))))
      names(cuenta)[which.max(cuenta)]
    }
    m <- utils::read.table(text = lineas, sep = sep, header = FALSE, quote = "\"",
                           colClasses = "character", na.strings = character(),
                           fill = TRUE, blank.lines.skip = FALSE, comment.char = "",
                           check.names = FALSE, strip.white = TRUE,
                           encoding = codificacion)
    return(list(matriz = unname(as.matrix(m)), origen = basename(datos),
                desfase = 0L, avisos = avisos))
  }
  stop("Formato '.", ext, "' no soportado por preparar(). Usa xlsx, xls, csv, txt o tsv.",
       call. = FALSE)
}

.ink_es_numero <- function(z) {
  z <- trimws(z)
  grepl("^[-+]?[0-9]+([.,][0-9]+)?([eE][-+]?[0-9]+)?$", z)
}

# Fila del encabezado: la primera fila "ancha" con mayoria de texto, seguida
# de filas con datos. Si la fila siguiente tambien es texto y completa los
# huecos (I, II, III bajo "Rendimiento"), el encabezado ocupa dos filas.
.ink_buscar_encabezado <- function(m) {
  llenas <- rowSums(!is.na(m))
  ancho_max <- max(llenas)
  limite <- min(nrow(m) - 1L, 15L)
  for (i in seq_len(limite)) {
    f <- m[i, ]
    ok <- !is.na(f)
    if (sum(ok) < 2L || sum(ok) < 0.5 * ancho_max) next
    if (mean(!.ink_es_numero(f[ok])) < 0.6) next
    sig <- m[i + 1L, ]
    ok2 <- !is.na(sig)
    dos <- FALSE
    if (i + 2L <= nrow(m) && sum(ok2) >= 2L && all(!.ink_es_numero(sig[ok2])) &&
        any(is.na(f) & ok2)) {
      # La tercera fila debe ser de datos (con numeros)
      ter <- m[i + 2L, ]
      if (any(.ink_es_numero(ter[!is.na(ter)]))) dos <- TRUE
    }
    return(list(fila = i, dos_filas = dos))
  }
  list(fila = 1L, dos_filas = FALSE)
}

.ink_nombres <- function(original, padre, dos_filas) {
  n <- length(original)
  unidades <- rep(NA_character_, n)
  limpio <- character(n)
  padre_limpio <- rep(NA_character_, n)
  patron_unidad <- "^(.*?)\\s*[\\(\\[]\\s*([^\\)\\]]+?)\\s*[\\)\\]]\\s*$"
  separar <- function(z) {
    if (is.na(z)) return(c(NA, NA))
    if (grepl(patron_unidad, z, perl = TRUE)) {
      c(sub(patron_unidad, "\\1", z, perl = TRUE), sub(patron_unidad, "\\2", z, perl = TRUE))
    } else c(z, NA)
  }
  for (j in seq_len(n)) {
    s <- separar(original[j])
    limpio[j] <- .ink_nombre_columna(s[1L])
    unidades[j] <- s[2L]
    if (dos_filas && !is.na(padre[j]) && !identical(padre[j], original[j])) {
      sp <- separar(padre[j])
      padre_limpio[j] <- .ink_nombre_columna(sp[1L])
      if (is.na(unidades[j])) unidades[j] <- sp[2L]
    }
  }
  vacios <- !nzchar(limpio) | is.na(limpio)
  limpio[vacios] <- paste0("col_", which(vacios))
  # Columnas con el mismo nombre bajo padres distintos (altura I, rend. I)
  repetidos <- duplicated(limpio) | duplicated(limpio, fromLast = TRUE)
  con_padre <- repetidos & !is.na(padre_limpio)
  limpio[con_padre] <- paste0(padre_limpio[con_padre], "_", limpio[con_padre])
  limpio <- make.unique(limpio, sep = "_")
  names(unidades) <- limpio
  list(nombre = limpio, original = original, unidades = unidades, padre = padre_limpio)
}

.ink_nombre_columna <- function(z) {
  if (is.na(z)) return("")
  z <- tolower(trimws(enc2utf8(z)))
  # Sin chartr(): falla con tildes si R corre en una configuracion regional
  # que no es UTF-8.
  con_tilde <- c("\u00e1", "\u00e9", "\u00ed", "\u00f3", "\u00fa", "\u00fc", "\u00f1",
                 "\u00e0", "\u00e8", "\u00ec", "\u00f2", "\u00f9", "\u00c1", "\u00c9",
                 "\u00cd", "\u00d3", "\u00da", "\u00d1")
  sin_tilde <- c("a", "e", "i", "o", "u", "u", "n", "a", "e", "i", "o", "u",
                 "a", "e", "i", "o", "u", "n")
  for (k in seq_along(con_tilde)) z <- gsub(con_tilde[k], sin_tilde[k], z, fixed = TRUE)
  z <- gsub("[^a-z0-9]+", "_", z)
  gsub("^_+|_+$", "", z)
}

.ink_palabras_resumen <- function() {
  c("total", "totales", "suma", "sumatoria", "subtotal", "promedio", "promedios",
    "prom", "media", "medias", "media_general", "promedio_general", "x\u0304",
    "desviacion", "desviacion_estandar", "desv", "desv_est", "d_e", "de_estandar",
    "sd", "ds", "cv", "c_v", "cv_porcentaje", "coeficiente_de_variacion",
    "varianza", "error_estandar", "ee", "e_e", "maximo", "minimo", "max", "min",
    "rango", "mediana", "n_total")
}

# Clasifica cada fila: "resumen" (Total, Promedio...), "nota" (Fuente:,
# Nota:...) o "" (dato).
.ink_es_resumen <- function(cuerpo) {
  apply(cuerpo, 1L, function(f) {
    primeras <- stats::na.omit(f)[seq_len(min(2L, sum(!is.na(f))))]
    if (!length(primeras)) return("")
    clave <- vapply(primeras, function(z) {
      if (startsWith(enc2utf8(z), "x\u0304")) return("x\u0304")
      .ink_nombre_columna(sub("\\s*[:=].*$", "", z))
    }, character(1))
    if (any(grepl("^(fuente|nota|notas|elaboracion|elaborado|leyenda)(_|$)", clave))) return("nota")
    if (any(clave %in% .ink_palabras_resumen())) return("resumen")
    ""
  })
}

# Numero seguido de una unidad: "12.5 kg", "30cm", "4,2 t/ha", "25 oC".
.ink_patron_unidad <- function() {
  "^([-+]?[0-9]+([.,][0-9]+)?)\\s*[a-zA-Z/%]+[a-zA-Z0-9/%.]*$"
}

# Simbolos de unidades (grado, cuadrado, cubo) como ASCII, para que las
# expresiones regulares funcionen en cualquier configuracion regional.
.ink_ascii_unidades <- function(v) {
  v <- enc2utf8(v)
  for (k in list(c("\u00b0", "o"), c("\u00ba", "o"), c("\u00b2", "2"), c("\u00b3", "3"))) {
    v <- gsub(k[1L], k[2L], v, fixed = TRUE)
  }
  v
}

.ink_parece_numerica <- function(x) {
  v <- .ink_ascii_unidades(x[!is.na(x)])
  if (!length(v)) return(FALSE)
  limpio <- sub(.ink_patron_unidad(), "\\1", v)
  mean(.ink_es_numero(limpio)) >= 0.75
}

.ink_convertir_numeros <- function(x) {
  valor <- rep(NA_real_, length(x))
  coma <- unidad <- malos <- miles <- integer()
  for (i in seq_along(x)) {
    z <- x[i]
    if (is.na(z)) next
    z <- .ink_ascii_unidades(z)
    patron_u <- .ink_patron_unidad()
    if (grepl(patron_u, z)) {
      z <- sub(patron_u, "\\1", z)
      unidad <- c(unidad, i)
    }
    if (grepl("^[-+]?[0-9]+,[0-9]+$", z)) {
      if (grepl("^[-+]?[0-9]{1,3},[0-9]{3}$", z)) miles <- c(miles, i)
      z <- sub(",", ".", z, fixed = TRUE)
      if (!i %in% unidad) coma <- c(coma, i)
    }
    v <- suppressWarnings(as.numeric(z))
    if (is.na(v)) {
      malos <- c(malos, i)
      unidad <- setdiff(unidad, i)
    } else {
      valor[i] <- v
    }
  }
  list(valor = valor, coma = coma, unidad = unidad, malos = malos, miles = miles)
}

.ink_patron_bloque <- function(nombre) {
  romanos <- "^(i|ii|iii|iv|v|vi|vii|viii|ix|x|xi|xii)$"
  con_numero <- "^(r|rep|repeticion|repeticiones|b|bl|blq|bloque|bloques|block|replica|parcela|p)_?([0-9]+)$"
  if (grepl(romanos, nombre)) return(toupper(nombre))
  if (grepl(con_numero, nombre)) return(sub(con_numero, "\\2", nombre))
  NA_character_
}

# Detecta columnas de bloques en formato ancho, agrupadas por variable (el
# encabezado padre) cuando hay varias variables.
.ink_detectar_ancho <- function(nombres, es_num, padre) {
  base <- ifelse(!is.na(padre) & startsWith(nombres, paste0(padre, "_")),
                 substring(nombres, nchar(padre) + 2L), nombres)
  etiqueta <- vapply(base, .ink_patron_bloque, character(1))
  candidatas <- which(!is.na(etiqueta) & es_num)
  if (length(candidatas) < 2L) return(NULL)
  variable <- ifelse(is.na(padre[candidatas]), "valor", padre[candidatas])
  grupos <- lapply(split(candidatas, factor(variable, levels = unique(variable))), function(idx) {
    list(columnas = nombres[idx], bloques = unname(etiqueta[idx]),
         variable = if (is.na(padre[idx[1L]])) "valor" else padre[idx[1L]])
  })
  grupos <- grupos[vapply(grupos, function(g) length(g$columnas) >= 2L, logical(1))]
  if (!length(grupos)) return(NULL)
  bloques <- lapply(grupos, `[[`, "bloques")
  if (length(unique(lapply(bloques, sort))) > 1L) return(NULL)
  list(grupos = grupos)
}

.ink_a_largo <- function(d, ancho, fila_excel) {
  todas <- unlist(lapply(ancho$grupos, `[[`, "columnas"))
  id <- setdiff(names(d), todas)
  bloques <- ancho$grupos[[1L]]$bloques
  partes <- lapply(seq_along(bloques), function(b) {
    parte <- d[, id, drop = FALSE]
    parte$bloque <- bloques[b]
    for (g in ancho$grupos) {
      parte[[g$variable]] <- d[[g$columnas[match(bloques[b], g$bloques)]]]
    }
    parte$.orden <- seq_len(nrow(d))
    parte$.fila <- fila_excel
    parte
  })
  largo <- do.call(rbind, partes)
  largo <- largo[order(largo$.orden, match(largo$bloque, bloques)), , drop = FALSE]
  fila <- largo$.fila
  largo$.orden <- NULL
  largo$.fila <- NULL
  rownames(largo) <- NULL
  list(datos = largo, fila_excel = fila)
}

.ink_unificar <- function(x) {
  ok <- !is.na(x)
  clave <- gsub("[ _-]+", "", tolower(x))
  cambios <- character()
  for (k in unique(clave[ok])) {
    sel <- ok & clave == k
    tab <- table(x[sel])
    if (length(tab) > 1L) {
      # La mas frecuente; en empate, la que no tiene espacios y luego la que
      # aparece primero.
      v <- names(tab)
      orden <- order(-as.integer(tab), grepl(" ", v), match(v, x[sel]))
      variantes <- tab[orden]
      canon <- names(variantes)[1L]
      cambios <- c(cambios, sprintf("%s -> '%s'",
                                    paste0("'", names(variantes)[-1L], "'", collapse = ", "), canon))
      x[sel] <- canon
    }
  }
  list(x = x, cambios = cambios)
}

.ink_sospechosos <- function(x) {
  v <- x[!is.na(x) & x > 0]
  if (length(v) < 6L || any(x < 0, na.rm = TRUE)) return(integer())
  med <- stats::median(v)
  which(!is.na(x) & x > 0 & abs(log10(x / med)) >= 0.95)
}
