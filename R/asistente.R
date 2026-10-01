#' Asistente para analizar un experimento sin saber R
#'
#' Lee el archivo, reconoce qué columnas son variables medidas, factores,
#' bloques o localidades, y hace preguntas en lenguaje de campo (qué
#' mediste, qué comparaste, cómo se sembró). Con las respuestas ejecuta
#' [inkagro()], muestra el reporte, ofrece crear el informe en Word e
#' imprime las líneas de código para repetir el análisis sin preguntas.
#' Después del análisis ofrece un menú de gráficos.
#'
#' Ejecuta esta función en una línea sola: si se envía junto con otras
#' líneas del script, R toma la siguiente línea como respuesta.
#'
#' @param datos Ruta a un archivo de datos o un `data.frame`. Si se omite,
#'   se abre una ventana para elegir el archivo.
#' @return El resultado de [inkagro()], de forma invisible, o `NULL` si se
#'   cancela. Guárdalo con `res <- asistente()` para seguir usándolo.
#' @examples
#' if (interactive()) {
#'   res <- asistente()
#' }
#' @export
asistente <- function(datos) {
  if (!interactive()) {
    stop("asistente() hace preguntas en la consola y solo funciona en una ",
         "sesi\u00f3n interactiva. En un script usa inkagro() directamente.",
         call. = FALSE)
  }
  if (missing(datos)) {
    message("Elige el archivo con tus datos en la ventana que se abri\u00f3...")
    datos <- file.choose()
  }
  fuente <- if (is.character(datos)) datos else deparse(substitute(datos))
  .ink_asistente(datos, fuente, .ink_preguntar_consola, decir = message)
}

#' Leer un archivo de datos
#'
#' Lee archivos `.csv` (con coma o punto y coma), `.txt`, `.tsv`, `.xlsx`,
#' `.xls`, `.rds`, `.sav` y `.dta` y devuelve un `data.frame`. Es el mismo
#' lector que usa [inkagro()]; si el archivo no existe, sugiere dónde está.
#'
#' @param archivo Ruta al archivo.
#' @return Un `data.frame`.
#' @examples
#' f <- tempfile(fileext = ".csv")
#' write.csv2(PlantGrowth, f, row.names = FALSE)
#' head(leer_datos(f))
#' unlink(f)
#' @export
leer_datos <- function(archivo) {
  .ink_leer(archivo)
}

# ---- logica del asistente (sin E/S directa, para poder probarla) ---------

# preguntar(titulo, opciones, multiple) devuelve el/los indices elegidos;
# integer(0) o 0 significa cancelar.
.ink_preguntar_consola <- function(titulo, opciones, multiple = FALSE) {
  cat("\n", paste(strwrap(titulo, width = 76), collapse = "\n"), "\n\n", sep = "")
  cat(paste0(format(seq_along(opciones), width = nchar(length(opciones))), ": ",
             opciones), sep = "\n")
  indicacion <- if (multiple) {
    "\nEscribe los n\u00fameros separados por espacio (0 para cancelar): "
  } else {
    "\nEscribe el n\u00famero (0 para cancelar): "
  }
  for (intento in 1:3) {
    texto <- trimws(readline(indicacion))
    numeros <- suppressWarnings(as.integer(strsplit(texto, "[ ,]+")[[1]]))
    if (length(numeros) && !anyNA(numeros)) {
      if (identical(numeros, 0L)) return(0L)
      if (all(numeros >= 1L & numeros <= length(opciones)) &&
          (multiple || length(numeros) == 1L)) {
        return(unique(numeros))
      }
    }
    cat("No entend\u00ed la respuesta. Escribe ",
        if (multiple) "uno o m\u00e1s n\u00fameros" else "un n\u00famero",
        " entre 1 y ", length(opciones), ".\n", sep = "")
  }
  0L
}

.ink_asistente <- function(datos, fuente, preguntar, decir) {
  crudo <- .ink_leer(datos)
  cls <- .ink_clasificar(crudo)
  cancelar <- function() {
    decir("Asistente cancelado.")
    invisible(NULL)
  }

  decir(sprintf("\nLe\u00ed %d filas y %d columnas.", nrow(crudo), ncol(crudo)))
  decir(.ink_resumen_columnas(crudo, cls))

  if (!length(cls$medidas)) {
    decir(paste0(
      "\nNo encontr\u00e9 ninguna columna con n\u00fameros medidos (rendimiento, ",
      "altura, peso...). Revisa que esa columna tenga solo n\u00fameros."))
    return(invisible(NULL))
  }

  # 1. Variable respuesta
  if (length(cls$medidas) == 1L) {
    respuesta <- cls$medidas
    decir(sprintf("\nVariable a analizar: '%s' (es la \u00fanica medida).", respuesta))
  } else {
    i <- preguntar("\u00bfQu\u00e9 variable quieres analizar?", cls$medidas, FALSE)
    if (!length(i) || i == 0L) return(cancelar())
    respuesta <- cls$medidas[i]
  }

  # 2. Localidades / ambientes: se analiza uno a la vez
  filtro <- NULL
  for (col in cls$ambientes) {
    amb <- .ink_factor(crudo[[col]])
    niveles <- levels(amb)
    i <- preguntar(sprintf(paste0(
      "Tus datos tienen %d niveles en '%s' (localidades, ambientes o a\u00f1os). ",
      "InkAgro analiza uno a la vez. \u00bfCu\u00e1l quieres analizar?"),
      length(niveles), col), niveles, FALSE)
    if (!length(i) || i == 0L) return(cancelar())
    filtro <- list(columna = col, valor = niveles[i])
    crudo <- crudo[!is.na(amb) & amb == niveles[i], , drop = FALSE]
    break
  }

  candidatos <- cls$factores
  if (!length(candidatos)) {
    decir("\nNo encontr\u00e9 columnas de tratamientos (grupos como variedad o dosis).")
    return(invisible(NULL))
  }

  # 3. Bloques
  sugeridos <- candidatos[.ink_norm(candidatos) %in% pal_bloque]
  orden <- c(sugeridos, setdiff(candidatos, sugeridos))
  etiquetas <- c(ifelse(orden %in% sugeridos, paste0(orden, "  (parece de bloques)"), orden),
                 "Ninguna: no hubo bloques")
  i <- preguntar(paste0(
    "\u00bfEl campo se dividi\u00f3 en bloques o repeticiones, cada uno con todos ",
    "los tratamientos? Elige la columna:"), etiquetas, FALSE)
  if (!length(i) || i == 0L) return(cancelar())
  bloque <- if (i <= length(orden)) orden[i] else NULL
  if (length(sugeridos) && (is.null(bloque) || !bloque %in% sugeridos)) {
    elegido <- if (is.null(bloque)) "que no hubo bloques" else paste0("'", bloque, "' como bloque")
    j <- preguntar(sprintf(paste0(
      "Elegiste %s, pero la columna '%s' parece de bloques. ",
      "Si te equivocas, el an\u00e1lisis sale mal. \u00bfQu\u00e9 hacemos?"),
      elegido, sugeridos[1L]),
      c(sprintf("Usar '%s' como bloque", sugeridos[1L]),
        "Mantener lo que eleg\u00ed"), FALSE)
    if (!length(j) || j == 0L) return(cancelar())
    if (j == 1L) bloque <- sugeridos[1L]
  }

  # 4. Tratamientos
  resto <- setdiff(candidatos, bloque)
  if (!length(resto)) {
    decir("\nNo quedan columnas de tratamientos aparte del bloque.")
    return(invisible(NULL))
  }
  if (length(resto) == 1L) {
    factores <- resto
    decir(sprintf("\nTratamiento: '%s'.", factores))
  } else {
    idx <- preguntar(paste0(
      "\u00bfQu\u00e9 comparaste en el experimento? Elige uno o dos factores ",
      "(escribe los n\u00fameros separados por espacio):"), resto, TRUE)
    if (!length(idx) || all(idx == 0L)) return(cancelar())
    factores <- resto[idx]
  }
  # Si otra columna explica las repeticiones dentro de cada bloque (o de
  # cada tratamiento), probablemente tambien es un factor del experimento.
  if (length(factores) <= 2L) {
    fs <- lapply(c(factores, bloque), function(cc) .ink_factor(crudo[[cc]]))
    unidad <- interaction(fs, drop = TRUE)
    if (any(table(unidad) > 1L)) {
      for (otro in setdiff(resto, factores)) {
        v <- .ink_factor(crudo[[otro]])
        ok <- !is.na(unidad) & !is.na(v)
        if (!any(ok) || max(table(droplevels(unidad[ok]), droplevels(v[ok]))) != 1L) next
        veces <- max(table(droplevels(unidad[ok])))
        donde <- if (is.null(bloque)) "en tus datos" else "dentro de cada bloque"
        if (!is.null(bloque) && .ink_norm(otro) %in% pal_bloque) {
          decir(sprintf(paste0(
            "\nLos bloques de '%s' se repiten dentro de cada nivel de '%s': ",
            "cada repetici\u00f3n est\u00e1 dividida en bloques peque\u00f1os que no tienen ",
            "todos los tratamientos. Es un dise\u00f1o de bloques incompletos ",
            "(por ejemplo alfa-l\u00e1tice), que InkAgro 0.1 a\u00fan no analiza.\n\n",
            "Opci\u00f3n v\u00e1lida mientras tanto: analizarlo como bloques completos ",
            "usando '%s' como bloque (pierde algo de precisi\u00f3n, pero es ",
            "correcto). Vuelve a correr el asistente y elige '%s' como bloque."),
            bloque, otro, otro, otro))
          return(invisible(NULL))
        }
        if (.ink_norm(otro) %in% .ink_pal_submuestra()) {
          decir(sprintf(paste0(
            "\nCada parcela tiene %d mediciones (columna '%s'). Son submuestras de ",
            "la misma parcela, no repeticiones: si se analizan como tales, el ",
            "error se subestima y aparecen diferencias falsas.\n\nPromedia las ",
            "mediciones de cada parcela y vuelve a correr el asistente con ese ",
            "archivo, por ejemplo:\n%s",
            "  promedios <- aggregate(`%s` ~ %s, data = datos, FUN = mean)\n",
            "  res <- asistente(promedios)"),
            veces, otro,
            if (is.character(datos)) paste0("  datos <- leer_datos(\"", fuente, "\")\n") else "",
            respuesta,
            paste0("`", c(bloque, factores), "`", collapse = " + ")))
          return(invisible(NULL))
        }
        if (length(factores) == 2L) {
          decir(sprintf(paste0(
            "\nCada combinaci\u00f3n de '%s' y '%s' aparece %d veces %s, una con cada ",
            "nivel de '%s'. Eso indica un tercer factor en el experimento, y ",
            "InkAgro 0.1 analiza hasta dos. Ignorarlo sumar\u00eda sus diferencias ",
            "al error y el an\u00e1lisis saldr\u00eda mal.\n\nOpci\u00f3n: analiza ",
            "cada nivel de '%s' por separado, por ejemplo:\n  ",
            "res <- inkagro(subset(datos, %s == \"%s\"), ...)"),
            factores[1L], factores[2L], veces, donde, otro, otro, otro,
            as.character(levels(v)[1L])))
          return(invisible(NULL))
        }
        j <- preguntar(sprintf(paste0(
          "Cada nivel de '%s' aparece %d veces %s, una con cada nivel de '%s'. ",
          "Eso indica que '%s' tambi\u00e9n es un factor del experimento. ",
          "Si lo ignoras, sus diferencias se suman al error y el an\u00e1lisis ",
          "sale mal. \u00bfTambi\u00e9n comparaste '%s'?"),
          factores, veces, donde, otro, otro, otro),
          c(sprintf("S\u00ed, tambi\u00e9n compar\u00e9 '%s'", otro),
            sprintf("No, analizar solo '%s'", factores)), FALSE)
        if (!length(j) || j == 0L) return(cancelar())
        if (j == 1L) factores <- c(factores, otro)
        break
      }
    }
  }
  if (length(factores) > 2L) {
    decir("\nInkAgro 0.1 analiza hasta dos factores a la vez. Elige como m\u00e1ximo dos.")
    return(invisible(NULL))
  }

  # 5. Diseno
  if (length(factores) == 1L) {
    diseno <- if (is.null(bloque)) "dca" else "dbca"
    tratamiento <- factores
    factor_b <- NULL
  } else if (is.null(bloque)) {
    diseno <- "factorial"
    tratamiento <- factores[1L]
    factor_b <- factores[2L]
  } else {
    i <- preguntar(
      "Tienes dos factores en bloques. \u00bfC\u00f3mo se sembr\u00f3 dentro de cada bloque?",
      c("Todas las combinaciones sorteadas juntas, parcela por parcela",
        "Un factor en parcelas grandes y el otro dentro de ellas",
        "No s\u00e9"), FALSE)
    if (!length(i) || i == 0L) return(cancelar())
    if (i == 3L) {
      decir(.ink_explicar_pd(factores))
      return(invisible(NULL))
    }
    if (i == 1L) {
      diseno <- "factorial"
      tratamiento <- factores[1L]
      factor_b <- factores[2L]
    } else {
      j <- preguntar("\u00bfCu\u00e1l factor iba en las parcelas grandes?", factores, FALSE)
      if (!length(j) || j == 0L) return(cancelar())
      diseno <- "parcelas_divididas"
      tratamiento <- factores[j]
      factor_b <- factores[-j]
    }
  }

  prueba <- "tukey"
  n_niveles <- max(nlevels(.ink_factor(crudo[[tratamiento]])),
                   if (!is.null(factor_b)) nlevels(.ink_factor(crudo[[factor_b]])) else 0L)
  if (n_niveles >= 10L) {
    i <- preguntar(sprintf(paste0(
      "Tienes %d tratamientos. Con tantos, Tukey suele dar letras repetidas ",
      "(ab, abc, abcd) que casi no separan. Scott-Knott forma grupos sin letras ",
      "repetidas. \u00bfQu\u00e9 prueba usamos para comparar medias?"), n_niveles),
      c("Scott-Knott (recomendada con muchos tratamientos)", "Tukey"), FALSE)
    if (!length(i) || i == 0L) return(cancelar())
    if (i == 1L) prueba <- "scottknott"
  }

  codigo <- .ink_codigo_asistente(fuente, filtro, respuesta, tratamiento,
                                  factor_b, bloque, diseno, prueba)
  res <- tryCatch(
    inkagro(crudo, respuesta = respuesta, tratamiento = tratamiento,
            diseno = diseno, bloque = bloque, factor_b = factor_b, prueba = prueba),
    error = function(e) e)
  if (inherits(res, "error")) {
    decir(paste0("\nNo se pudo hacer el an\u00e1lisis:\n", conditionMessage(res)))
    decir(paste0("\nEl c\u00f3digo que se intent\u00f3 fue:\n", codigo))
    return(invisible(NULL))
  }
  print(res)

  decir(paste0("\nPara repetir este an\u00e1lisis sin preguntas, copia en tu script:\n",
               codigo))

  .ink_menu_graficos(res, "res", preguntar, decir)

  i <- preguntar("\u00bfQuieres el informe en Word?", c("S\u00ed", "No"), FALSE)
  if (length(i) && i == 1L) {
    if (requireNamespace("officer", quietly = TRUE) &&
        requireNamespace("flextable", quietly = TRUE)) {
      archivo <- file.path(
        if (is.character(datos)) dirname(path.expand(datos)) else getwd(),
        paste0("informe_", gsub("[^A-Za-z0-9]+", "_", respuesta), ".docx"))
      informe(res, archivo)
      decir(paste0("Informe guardado en: ", archivo))
    } else {
      decir(paste0("Para el informe instala dos paquetes y vuelve a intentarlo:\n",
                   "  install.packages(c(\"officer\", \"flextable\"))"))
    }
  }
  decir(paste0(
    "\nListo. Si quieres seguir usando este resultado (graficos(res), ",
    "informe(res, ...)), la pr\u00f3xima vez escribe:  res <- asistente()"))
  invisible(res)
}

# Clasifica columnas en medidas (respuestas), factores, ambientes e ignoradas.
.ink_clasificar <- function(crudo) {
  n <- nrow(crudo)
  medidas <- factores <- ambientes <- ignoradas <- character()
  es_nota <- function(nombre) {
    z <- .ink_norm(nombre)
    grepl("^obs|nota|coment|^fecha$|^date$|remark|^id$|^codigo|^code", z) ||
      z %in% cols_excluir
  }
  for (col in names(crudo)) {
    v <- crudo[[col]]
    texto <- trimws(as.character(v))
    texto[texto %in% c("", "NA", "na", "-", ".")] <- NA
    llenos <- sum(!is.na(texto))
    if (llenos < n / 2 || es_nota(col)) {
      ignoradas <- c(ignoradas, col)
      next
    }
    k <- length(unique(stats::na.omit(tolower(gsub("\\s+", " ", texto)))))
    num <- suppressWarnings(as.numeric(gsub(",", ".", texto, fixed = TRUE)))
    es_num <- mean(is.na(num[!is.na(texto)])) < 0.05
    # Posiciones en el campo (fila, columna, numero de parcela): enteros que
    # se repiten el mismo numero de veces o que no se repiten nunca.
    enteros <- es_num && all(num[!is.na(num)] == round(num[!is.na(num)]))
    es_posicion <- .ink_norm(col) %in% c(pal_fila, pal_columna, pal_parcela) ||
      (enteros && k > 8 && length(unique(as.integer(table(num)))) == 1L &&
         diff(range(num, na.rm = TRUE)) + 1 == k)
    if (es_posicion) {
      ignoradas <- c(ignoradas, col)
    } else if (es_num && k > max(8, 0.25 * llenos)) {
      medidas <- c(medidas, col)
    } else if (k < 2L || k > n / 2) {
      ignoradas <- c(ignoradas, col)
    } else if (.ink_norm(col) %in% .ink_pal_ambiente()) {
      ambientes <- c(ambientes, col)
    } else {
      factores <- c(factores, col)
    }
  }
  list(medidas = medidas, factores = factores, ambientes = ambientes,
       ignoradas = ignoradas)
}

.ink_resumen_columnas <- function(crudo, cls) {
  niveles <- function(cols) {
    if (!length(cols)) return("ninguna")
    paste0(cols, " (", vapply(cols, function(c) nlevels(.ink_factor(crudo[[c]])), integer(1)),
      ")", collapse = ", ")
  }
  paste0(
    "  Variables medidas:     ", if (length(cls$medidas)) paste(cls$medidas, collapse = ", ") else "ninguna",
    "\n  Grupos (niveles):      ", niveles(cls$factores),
    if (length(cls$ambientes)) paste0("\n  Localidades/ambientes: ", niveles(cls$ambientes)) else "",
    if (length(cls$ignoradas)) paste0("\n  No se usar\u00e1n:         ", paste(cls$ignoradas, collapse = ", ")) else "")
}

.ink_codigo_asistente <- function(fuente, filtro, respuesta, tratamiento,
                                  factor_b, bloque, diseno, prueba = "tukey") {
  cita <- function(z) paste0("\"", z, "\"")
  es_archivo <- grepl("\\.[A-Za-z0-9]{2,4}$", fuente) && !grepl("[()]", fuente)
  lineas <- character()
  objeto <- fuente
  if (es_archivo) {
    lineas <- c(lineas, paste0("datos <- leer_datos(", cita(fuente), ")"))
    objeto <- "datos"
  }
  if (!is.null(filtro)) {
    # Compara sin mayusculas ni espacios, igual que hace el asistente
    objeto <- paste0("subset(", objeto, ", tolower(trimws(`", filtro$columna, "`)) == ",
                     cita(tolower(filtro$valor)), ")")
  }
  args <- c(objeto, paste0("respuesta = ", cita(respuesta)),
            paste0("tratamiento = ", cita(tratamiento)),
            if (!is.null(factor_b)) paste0("factor_b = ", cita(factor_b)),
            if (!is.null(bloque)) paste0("bloque = ", cita(bloque)),
            paste0("diseno = ", cita(diseno)),
            if (prueba != "tukey") paste0("prueba = ", cita(prueba)))
  lineas <- c(lineas, paste0("res <- inkagro(", paste(args, collapse = ",\n               "), ")"),
              "res")
  paste0("  ", lineas, collapse = "\n")
}

.ink_explicar_pd <- function(factores) {
  a <- factores[1L]
  b <- factores[2L]
  paste0(
    "\nLa diferencia est\u00e1 en c\u00f3mo se sorte\u00f3 en campo, y no se puede ",
    "saber mirando los datos:\n\n",
    "  FACTORIAL: cada parcela recibe una combinaci\u00f3n sorteada al azar.\n",
    "    Bloque 1: [", a, "1-", b, "2] [", a, "2-", b, "1] [", a, "1-", b, "1] [", a, "2-", b, "2]\n\n",
    "  PARCELAS DIVIDIDAS: primero se sortea '", a, "' en parcelas grandes y\n",
    "  luego '", b, "' dentro de cada una.\n",
    "    Bloque 1: [ ", a, "2: ", b, "1 ", b, "2 ] [ ", a, "1: ", b, "2 ", b, "1 ]\n\n",
    "Suele ser parcelas divididas cuando un factor es dif\u00edcil de aplicar en\n",
    "parcelas peque\u00f1as (riego, labranza, fecha de siembra). Pregunta a quien\n",
    "instal\u00f3 el ensayo y vuelve a correr asistente().")
}
