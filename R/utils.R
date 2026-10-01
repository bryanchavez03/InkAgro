# Funciones internas de apoyo: lectura, columnas, conversiones y formato.

.ink_leer <- function(datos) {
  if (is.data.frame(datos)) return(as.data.frame(datos))
  if (!is.character(datos) || length(datos) != 1L) {
    stop("'datos' debe ser un data.frame o la ruta a un archivo.", call. = FALSE)
  }
  if (!file.exists(datos)) {
    stop("No se encontr\u00f3 el archivo: ", datos, call. = FALSE)
  }
  ext <- tolower(tools::file_ext(datos))
  leido <- switch(
    ext,
    csv = {
      cabecera <- readLines(datos, n = 1L, warn = FALSE)
      lector <- if (length(cabecera) && grepl(";", cabecera)) {
        utils::read.csv2
      } else {
        utils::read.csv
      }
      lector(datos, stringsAsFactors = FALSE, check.names = FALSE)
    },
    txt = , tsv = utils::read.delim(datos, stringsAsFactors = FALSE,
                                    check.names = FALSE),
    xlsx = , xls = readxl::read_excel(datos),
    rds  = readRDS(datos),
    sav  = haven::zap_labels(haven::read_sav(datos)),
    dta  = haven::zap_labels(haven::read_dta(datos)),
    stop("Formato '.", ext, "' no soportado. Usa csv, txt, tsv, xlsx, xls, ",
         "rds, sav o dta.", call. = FALSE)
  )
  as.data.frame(leido)
}

# Devuelve el nombre real de la columna (tolera mayusculas y espacios) o
# se detiene sugiriendo las columnas mas parecidas.
.ink_col <- function(datos, nombre, arg) {
  if (is.null(nombre)) return(NULL)
  if (!is.character(nombre) || length(nombre) != 1L || is.na(nombre)) {
    stop("'", arg, "' debe ser el nombre de una columna, entre comillas.",
         call. = FALSE)
  }
  nms <- names(datos)
  if (nombre %in% nms) return(nombre)
  norm <- function(z) tolower(trimws(z))
  hit <- nms[norm(nms) == norm(nombre)]
  if (length(hit) == 1L) return(hit)
  dist <- stringdist::stringdist(norm(nombre), norm(nms), method = "jw")
  sug  <- nms[order(dist)][seq_len(min(3L, length(nms)))]
  stop(sprintf(
    "No existe la columna '%s' (argumento '%s'). \u00bfQuisiste decir %s?\nColumnas disponibles: %s",
    nombre, arg, paste0("'", sug, "'", collapse = ", "),
    paste(nms, collapse = ", ")), call. = FALSE)
}

.ink_numerica <- function(x, nombre) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "na", "N/A", "n/a", ".", "-")] <- NA
  y <- suppressWarnings(as.numeric(x))
  malos <- !is.na(x) & is.na(y)
  if (any(malos)) {
    y2 <- suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
    if (sum(!is.na(x) & is.na(y2)) < sum(malos)) {
      y <- y2
      malos <- !is.na(x) & is.na(y)
    }
  }
  if (any(malos)) {
    stop(sprintf(
      "La respuesta '%s' tiene valores que no son n\u00fameros: %s. Corr\u00edgelos o indica otra columna.",
      nombre, paste0("'", utils::head(unique(x[malos]), 5L), "'", collapse = ", ")),
      call. = FALSE)
  }
  y
}

# Convierte a factor. Unifica etiquetas que solo difieren en mayusculas,
# espacios o coma decimal ("Golden Rain" / "golden rain ", "0,2" / "0.2") y
# guarda en el atributo "unificados" que se unio, para avisarlo. Si todos
# los niveles son numeros (dosis, por ejemplo), los ordena numericamente.
.ink_factor <- function(x) {
  x <- gsub("\\s+", " ", trimws(as.character(x)))
  x[x == ""] <- NA
  ok <- !is.na(x)
  coma <- ok & grepl("^-?[0-9]*,[0-9]+$", x)
  x[coma] <- sub(",", ".", x[coma], fixed = TRUE)
  num <- suppressWarnings(as.numeric(x))
  es_numerico <- any(ok) && !anyNA(num[ok])
  if (es_numerico) x[ok] <- as.character(num[ok])

  unificados <- character()
  clave <- tolower(x)
  for (k in unique(clave[ok])) {
    sel <- ok & clave == k
    variantes <- sort(table(x[sel]), decreasing = TRUE)
    if (length(variantes) > 1L) {
      canon <- names(variantes)[1L]
      x[sel] <- canon
      unificados <- c(unificados, sprintf(
        "%s -> '%s'", paste0("'", names(variantes)[-1L], "'", collapse = ", "), canon))
    }
  }

  niveles <- unique(x[ok])
  niveles <- if (es_numerico) niveles[order(as.numeric(niveles))] else sort(niveles)
  f <- factor(x, levels = niveles)
  attr(f, "unificados") <- unificados
  f
}

.ink_aviso_unificados <- function(f, nombre) {
  u <- attr(f, "unificados")
  if (!length(u)) return(character())
  sprintf(paste0("En '%s' se unieron etiquetas que solo difer\u00edan en ",
                 "may\u00fasculas o espacios: %s. Revisa que sean el mismo nivel."),
          nombre, paste(u, collapse = "; "))
}

.ink_lista <- function(x, max = 10L) {
  x <- as.character(x)
  if (length(x) <= max) return(paste(x, collapse = ", "))
  paste0(paste(x[seq_len(max)], collapse = ", "), " y ", length(x) - max, " m\u00e1s")
}

.ink_signif <- function(p) {
  out <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "ns")))
  out[is.na(p)] <- ""
  out
}

.ink_num <- function(x, digitos = 2L) {
  out <- formatC(x, format = "f", digits = digitos, big.mark = "")
  out[is.na(x)] <- ""
  out
}

.ink_p <- function(p) {
  out <- ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3L))
  out[is.na(p)] <- ""
  out
}

.ink_alias_diseno <- c(
  dca = "dca", crd = "dca", completamente_al_azar = "dca",
  dbca = "dbca", rcbd = "dbca", bloques = "dbca",
  bloques_completos_al_azar = "dbca",
  factorial = "factorial",
  parcelas_divididas = "pd", parcela_dividida = "pd", pd = "pd",
  split_plot = "pd", splitplot = "pd"
)

.ink_nombre_diseno <- function(codigo, con_bloque) {
  switch(
    codigo,
    dca       = "Dise\u00f1o completamente al azar (DCA)",
    dbca      = "Dise\u00f1o de bloques completos al azar (DBCA)",
    factorial = if (con_bloque) "Factorial en bloques completos al azar"
                else "Factorial en dise\u00f1o completamente al azar",
    pd        = "Parcelas divididas en bloques completos al azar"
  )
}

.ink_resolver_diseno <- function(diseno, bloque, factor_b) {
  if (is.null(diseno)) {
    if (!is.null(bloque) && !is.null(factor_b)) {
      stop(paste0(
        "Con 'bloque' y 'factor_b' el experimento puede ser un factorial en ",
        "bloques o unas parcelas divididas. Los datos no permiten distinguirlos ",
        "(depende de c\u00f3mo se aleatoriz\u00f3 en campo) y se analizan con errores ",
        "distintos. Indica diseno = \"factorial\" o diseno = \"parcelas_divididas\"."),
        call. = FALSE)
    }
    codigo <- if (is.null(bloque) && is.null(factor_b)) "dca"
              else if (!is.null(bloque)) "dbca" else "factorial"
    aviso <- sprintf(paste0(
      "No indicaste 'diseno'; se analiz\u00f3 como %s seg\u00fan las columnas indicadas. ",
      "Si en campo se instal\u00f3 de otra forma, ind\u00edcalo con 'diseno'."),
      .ink_nombre_diseno(codigo, !is.null(bloque)))
    return(list(codigo = codigo, aviso = aviso))
  }

  if (!is.character(diseno) || length(diseno) != 1L) {
    stop("'diseno' debe ser un texto, por ejemplo \"dbca\".", call. = FALSE)
  }
  clave  <- gsub("[ .-]+", "_", tolower(trimws(diseno)))
  codigo <- unname(.ink_alias_diseno[clave])
  if (is.na(codigo)) {
    stop("Dise\u00f1o '", diseno, "' no reconocido. Opciones: \"dca\", \"dbca\", ",
         "\"factorial\", \"parcelas_divididas\".", call. = FALSE)
  }

  if (codigo == "dca" && !is.null(bloque)) {
    stop("Indicaste 'bloque' pero el dise\u00f1o es DCA, que no tiene bloques. ",
         "Usa diseno = \"dbca\" o quita 'bloque'.", call. = FALSE)
  }
  if (codigo %in% c("dca", "dbca") && !is.null(factor_b)) {
    stop("Indicaste 'factor_b' (dos factores): usa diseno = \"factorial\" ",
         "o \"parcelas_divididas\".", call. = FALSE)
  }
  if (codigo == "dbca" && is.null(bloque)) {
    stop("El DBCA necesita la columna de bloques: indica 'bloque'.", call. = FALSE)
  }
  if (codigo == "factorial" && is.null(factor_b)) {
    stop("El factorial necesita el segundo factor: indica 'factor_b'.", call. = FALSE)
  }
  if (codigo == "pd" && (is.null(bloque) || is.null(factor_b))) {
    stop("Parcelas divididas necesita 'bloque', 'tratamiento' (parcela ",
         "principal) y 'factor_b' (subparcela).", call. = FALSE)
  }
  list(codigo = codigo, aviso = character())
}
