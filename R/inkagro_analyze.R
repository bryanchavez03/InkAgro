# Imprime tabla Tukey estilo publicacion cientifica
.ink_print_tukey <- function(hsd_obj, tratamiento = NULL) {
  tab <- hsd_obj$groups
  n   <- nrow(tab)

  tab_clean <- data.frame(
    Tratamiento = rownames(tab),
    Media       = round(tab[[1]], 3),
    Grupos      = trimws(tab$groups),
    stringsAsFactors = FALSE
  )
  if (!is.null(tratamiento)) names(tab_clean)[1] <- tratamiento
  rownames(tab_clean) <- NULL

  cat("----------------------------------------------------------------\n")
  if (n > 15) {
    cat(sprintf("Nota: tabla con %d niveles. Se muestran los primeros 10.\n", n))
    print(head(tab_clean, 10), row.names = FALSE)
    cat(sprintf("... (%d niveles adicionales. Use resultado$tukey$groups)\n", n - 10))
  } else {
    print(tab_clean, row.names = FALSE)
  }
  cat("----------------------------------------------------------------\n")
  cat("Nota: medias con igual letra no difieren significativamente (Tukey HSD, alfa=0.05).\n\n")
}

inkagro_analyze <- function(datos, respuesta, tratamiento,
                            bloque = NULL, fa = NULL, fb = NULL,
                            rep = NULL, iblock = NULL,
                            gen = NULL, env = NULL,
                            diseno = NULL,
                            verbose = TRUE,
                            n_obs = nrow(datos),
                            n_vars = ncol(datos)) {

  fa_usuario <- fa

  # 1. Estandarizar nombres
  names(datos) <- tolower(trimws(names(datos)))

  # 2. Convertir parametros a minuscula
  respuesta   <- tolower(trimws(respuesta))
  tratamiento <- tolower(trimws(tratamiento))
  if (!is.null(bloque))  bloque  <- tolower(trimws(bloque))
  if (!is.null(fa))      fa      <- tolower(trimws(fa))
  if (!is.null(fb))      fb      <- tolower(trimws(fb))
  if (!is.null(rep))     rep     <- tolower(trimws(rep))
  if (!is.null(iblock))  iblock  <- tolower(trimws(iblock))
  if (!is.null(gen))     gen     <- tolower(trimws(gen))
  if (!is.null(env))     env     <- tolower(trimws(env))

  # 3. Validaciones
  if (!respuesta %in% names(datos))
    stop("La variable respuesta '", respuesta, "' no existe en los datos.")
  if (!is.numeric(datos[[respuesta]]))
    stop("La variable respuesta '", respuesta, "' no es numerica.")
  if (!tratamiento %in% names(datos))
    stop("El tratamiento '", tratamiento, "' no existe en los datos.")
  if (nlevels(as.factor(datos[[tratamiento]])) < 2)
    stop("El tratamiento '", tratamiento, "' tiene menos de 2 niveles.")

  # 4. Diseno
  if (is.null(diseno)) diseno <- inkagro_detect(datos, verbose = FALSE)
  if (diseno == "DBCA" && !is.null(fa_usuario)) diseno <- "Factorial"
  if (diseno == "Factorial con Ambientes" && !is.null(fa_usuario) && is.null(env))
    diseno <- "Factorial"
  if (grepl("No detectado", diseno)) {
    if (!is.null(bloque) && bloque %in% names(datos)) {
      message("Advertencia: diseno no detectado automaticamente. Se asume DBCA.")
      diseno <- "DBCA"
    } else {
      message("Advertencia: diseno no detectado automaticamente. Se asume DCA.")
      diseno <- "DCA"
    }
  }

  # 5. Detectar columnas
  cols <- names(datos)
  if (is.null(fa)) {
    ya_usadas <- c(tratamiento, bloque, rep, gen, env)
    factores  <- cols[cols %in% pal_factor & !cols %in% ya_usadas]
    if (length(factores) >= 1) fa <- factores[1]
    if (length(factores) >= 2) fb <- factores[2]
  }
  if (is.null(bloque)) {
    col_bloque <- cols[cols %in% pal_bloque]
    if (length(col_bloque) >= 1) bloque <- col_bloque[1]
  }
  if (is.null(rep)) {
    col_rep <- cols[cols %in% pal_bloque]
    if (length(col_rep) >= 1) rep <- col_rep[1]
  }
  if (is.null(iblock)) {
    col_iblock <- cols[cols %in% pal_alpha]
    if (length(col_iblock) >= 1) iblock <- col_iblock[1]
  }
  if (is.null(gen)) {
    col_gen <- cols[cols %in% pal_trat]
    if (length(col_gen) >= 1) gen <- col_gen[1]
  }
  if (is.null(env)) {
    col_env <- cols[cols %in% pal_ambiente]
    if (length(col_env) >= 1) env <- col_env[1]
  }

  # 6. Convertir a factor
  datos[[tratamiento]] <- as.factor(trimws(as.character(datos[[tratamiento]])))
  if (!is.null(bloque) && bloque %in% names(datos))
    datos[[bloque]] <- as.factor(trimws(as.character(datos[[bloque]])))
  if (!is.null(fa) && fa %in% names(datos))
    datos[[fa]]     <- as.factor(trimws(as.character(datos[[fa]])))
  if (!is.null(fb) && fb %in% names(datos))
    datos[[fb]]     <- as.factor(trimws(as.character(datos[[fb]])))
  if (!is.null(rep) && rep %in% names(datos))
    datos[[rep]]    <- as.factor(trimws(as.character(datos[[rep]])))
  if (!is.null(iblock) && iblock %in% names(datos))
    datos[[iblock]] <- as.factor(trimws(as.character(datos[[iblock]])))
  if (!is.null(gen) && gen %in% names(datos))
    datos[[gen]]    <- as.factor(trimws(as.character(datos[[gen]])))
  if (!is.null(env) && env %in% names(datos))
    datos[[env]]    <- as.factor(trimws(as.character(datos[[env]])))

  # Eliminar filas con NA en columnas de diseno
  cols_diseno <- unique(c(tratamiento, bloque, fa, fb, rep, env, gen, iblock))
  cols_diseno <- cols_diseno[!is.null(cols_diseno) & cols_diseno %in% names(datos)]
  if (length(cols_diseno) > 0) {
    na_diseno <- rowSums(is.na(datos[, cols_diseno, drop = FALSE])) > 0
    if (any(na_diseno)) {
      n_eliminadas <- sum(na_diseno)
      message(sprintf("Advertencia: se eliminaron %d fila(s) con datos faltantes en columnas del diseno.",
                      n_eliminadas))
      datos <- datos[!na_diseno, ]
    }
  }

  # Informar NAs en respuesta
  n_na_resp <- sum(is.na(datos[[respuesta]]))
  if (n_na_resp > 0) {
    n_efectivo <- nrow(datos) - n_na_resp
    message(sprintf("Nota: %d observaciones excluidas por datos faltantes en '%s' (n efectivo = %d).",
                    n_na_resp, respuesta, n_efectivo))
  }

  # Remover outliers extremos (3 * IQR)
  q1  <- quantile(datos[[respuesta]], 0.25, na.rm = TRUE)
  q3  <- quantile(datos[[respuesta]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  datos <- datos[
    !is.na(datos[[respuesta]]) &
      datos[[respuesta]] >= (q1 - 3 * iqr) &
      datos[[respuesta]] <= (q3 + 3 * iqr), ]

  # 7. Modelo segun diseno
  if (diseno == "DCA") {
    formula <- as.formula(paste(respuesta, "~", tratamiento))
    modelo  <- aov(formula, data = datos)

  } else if (diseno == "DBCA") {
    formula <- as.formula(paste(respuesta, "~", bloque, "+", tratamiento))
    modelo  <- aov(formula, data = datos)

  } else if (diseno == "Factorial") {
    if (!is.null(bloque)) {
      if (!is.null(fb)) {
        formula <- as.formula(paste(respuesta, "~", bloque, "+",
                                    tratamiento, "*", fa, "*", fb))
      } else {
        formula <- as.formula(paste(respuesta, "~", bloque, "+",
                                    tratamiento, "*", fa))
      }
    } else {
      if (!is.null(fb)) {
        formula <- as.formula(paste(respuesta, "~",
                                    tratamiento, "*", fa, "*", fb))
      } else {
        formula <- as.formula(paste(respuesta, "~", tratamiento, "*", fa))
      }
    }
    modelo <- aov(formula, data = datos)

  } else if (diseno == "Factorial con Ambientes") {
    if (!is.null(bloque)) {
      if (!is.null(fa_usuario)) {
        formula <- as.formula(paste(respuesta, "~", bloque, "+",
                                    tratamiento, "*", fa, "*", env))
      } else {
        formula <- as.formula(paste(respuesta, "~", bloque, "+",
                                    tratamiento, "*", env))
      }
    } else {
      if (!is.null(fa_usuario)) {
        formula <- as.formula(paste(respuesta, "~",
                                    tratamiento, "*", fa, "*", env))
      } else {
        formula <- as.formula(paste(respuesta, "~",
                                    tratamiento, "*", env))
      }
    }
    modelo <- aov(formula, data = datos)

  } else if (diseno == "Parcelas Divididas") {
    if (is.null(fa) || is.null(fb) || is.null(bloque))
      stop("Parcelas Divididas requiere fa (parcela principal), fb (subparcela) y bloque.")
    formula <- as.formula(paste(respuesta, "~", fa, "*", fb,
                                "+ Error(", bloque, "/", fa, ")"))
    modelo  <- aov(formula, data = datos)

  } else if (diseno == "Alfa-latice") {
    formula <- as.formula(paste(respuesta, "~", tratamiento,
                                "+ (1|", rep, ") + (1|", rep, ":", iblock, ")"))
    modelo  <- lme4::lmer(formula, data = datos)

  } else if (diseno == "Multiambiental (MET)") {
    formula <- as.formula(paste(respuesta, "~", gen, "*", env,
                                "+ (1|", env, ":", bloque, ")"))
    modelo  <- lme4::lmer(formula, data = datos)

  } else if (diseno == "Cuadrado Latino") {
    col_fila <- cols[cols %in% pal_fila][1]
    col_col  <- cols[cols %in% pal_columna][1]
    formula  <- as.formula(paste(respuesta, "~", tratamiento, "+",
                                 col_fila, "+", col_col))
    modelo   <- aov(formula, data = datos)

  } else if (diseno == "Strip-plot") {
    col_strip_r <- cols[cols %in% c("strip_row", "row_trt", "horizontal_factor",
                                    "factor_row", "row_factor")][1]
    col_strip_c <- cols[cols %in% c("strip_col", "col_trt", "vertical_factor",
                                    "factor_col", "column_factor", "cross_strip")][1]
    if (!is.na(col_strip_r) && !is.na(col_strip_c)) {
      datos[[col_strip_r]] <- as.factor(datos[[col_strip_r]])
      datos[[col_strip_c]] <- as.factor(datos[[col_strip_c]])
      if (!is.null(bloque)) {
        formula <- as.formula(paste(
          respuesta, "~", col_strip_r, "*", col_strip_c,
          "+ (1|", bloque, ") + (1|", bloque, ":", col_strip_r,
          ") + (1|", bloque, ":", col_strip_c, ")"))
        modelo <- lme4::lmer(formula, data = datos)
      } else {
        formula <- as.formula(paste(respuesta, "~", col_strip_r, "*", col_strip_c))
        modelo  <- aov(formula, data = datos)
      }
    } else {
      message("Advertencia: columnas strip no detectadas. Se usa DCA como alternativa.")
      formula <- as.formula(paste(respuesta, "~", tratamiento))
      modelo  <- aov(formula, data = datos)
    }

  } else if (diseno == "Repeated Measures") {
    col_parcela <- cols[cols %in% pal_parcela][1]
    col_tiempo  <- cols[cols %in% pal_tiempo][1]
    if (!is.na(col_parcela) && !is.na(col_tiempo)) {
      datos[[col_tiempo]]  <- as.factor(datos[[col_tiempo]])
      datos[[col_parcela]] <- as.factor(datos[[col_parcela]])
      if (!is.null(bloque)) {
        formula <- as.formula(paste(
          respuesta, "~", bloque, "+", tratamiento, "*", col_tiempo,
          "+ Error(", col_parcela, "/", col_tiempo, ")"))
      } else {
        formula <- as.formula(paste(
          respuesta, "~", tratamiento, "*", col_tiempo,
          "+ Error(", col_parcela, "/", col_tiempo, ")"))
      }
      modelo <- aov(formula, data = datos)
    } else {
      message("Advertencia: columnas de parcela/tiempo no detectadas. Se usa DCA.")
      formula <- as.formula(paste(respuesta, "~", tratamiento))
      modelo  <- aov(formula, data = datos)
    }

  } else {
    stop("Diseno no soportado en esta version.")
  }

  # 8. Encabezado de resultados
  n_trat   <- nlevels(as.factor(datos[[tratamiento]]))
  n_bloque <- if (!is.null(bloque)) nlevels(as.factor(datos[[bloque]])) else NULL

  cat("\n")
  cat("================================================================\n")
  cat("        InkAgro v0.1.0 - Analisis de Datos Agricolas           \n")
  cat("================================================================\n")
  cat(sprintf(" Observaciones : %d\n", n_obs))
  cat(sprintf(" Variables     : %d\n", n_vars))
  cat(sprintf(" Diseno        : %s\n", diseno))
  cat(sprintf(" Variable resp.: %s\n", respuesta))
  cat(sprintf(" Tratamiento   : %s (%d niveles)\n", tratamiento, n_trat))
  if (!is.null(bloque) && bloque %in% names(datos))
    cat(sprintf(" Bloques       : %s (%d bloques)\n", bloque, n_bloque))
  if (!is.null(env) && env %in% names(datos) && !diseno %in% c("DCA", "DBCA"))
    cat(sprintf(" Ambiente      : %s\n", env))
  cat("================================================================\n")

  # 9. Reporte de calidad de datos
  na_reporte <- colSums(is.na(datos))
  na_total   <- sum(na_reporte)
  cat("\n CALIDAD DE DATOS\n")
  cat("----------------------------------------------------------------\n")
  if (na_total == 0) {
    cat(" Sin valores faltantes en el conjunto de datos.\n")
  } else {
    cols_na <- na_reporte[na_reporte > 0]
    for (i in seq_along(cols_na)) {
      pct <- round(cols_na[i] / nrow(datos) * 100, 1)
      cat(sprintf(" %-20s %3d NA  (%s%%)\n",
                  names(cols_na)[i], cols_na[i], pct))
    }
  }
  cat("----------------------------------------------------------------\n")

  # 10. Tabla ANOVA estilo publicacion
  cat("\n ANALISIS DE VARIANZA (ANOVA)\n")
  cat("----------------------------------------------------------------\n")

  es_lmer    <- inherits(modelo, "lmerMod")
  es_parcela <- diseno == "Parcelas Divididas"

  .ink_format_anova <- function(tab) {
    op <- options(scipen = 999)
    on.exit(options(op))
    sig_col <- .ink_signif(tab[["Pr(>F)"]])
    tab[["F value"]] <- ifelse(is.na(tab[["F value"]]), NA,
                               round(tab[["F value"]], 3))
    tab[["Sum Sq"]]  <- round(tab[["Sum Sq"]], 3)
    tab[["Mean Sq"]] <- round(tab[["Mean Sq"]], 3)
    tab[["p-value"]] <- format.pval(tab[["Pr(>F)"]],
                                    digits = 3, eps = 0.001)
    tab[["Sig"]]     <- sig_col
    tab[["Pr(>F)"]]  <- NULL
    print(tab)
    cat("----------------------------------------------------------------\n")
    cat("Sig: *** p<0.001  ** p<0.01  * p<0.05  ns p>=0.05\n")
  }

  if (es_lmer) {
    print(summary(modelo))
  } else if (es_parcela) {
    anova_sum <- summary(modelo)
    for (estrato in names(anova_sum)) {
      cat(sprintf("\n Estrato: %s\n", estrato))
      tab <- tryCatch(as.data.frame(anova_sum[[estrato]]),
                      error = function(e) NULL)
      if (!is.null(tab)) .ink_format_anova(tab)
    }
  } else {
    anova_tab <- as.data.frame(summary(modelo)[[1]])
    .ink_format_anova(anova_tab)
  }

  # 11. Supuestos
  inkagro_supuestos(modelo, datos, tratamiento)

  # 12. Comparacion de medias
  cat("\n COMPARACION DE MEDIAS - Tukey HSD (alfa = 0.05)\n")
  cat("================================================================\n")

  if (diseno %in% c("DCA", "DBCA")) {
    tukey <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
    .ink_print_tukey(tukey, tratamiento)
    return(invisible(list(diseno = diseno, modelo = modelo, tukey = tukey)))

  } else if (diseno == "Factorial") {
    if (!is.null(fa) && !is.null(fb)) {
      p_triple <- tryCatch(
        summary(modelo)[[1]][paste0(tratamiento, ":", fa, ":", fb), "Pr(>F)"],
        error = function(e) NA)
      if (!is.na(p_triple) && p_triple < 0.05) {
        cat(sprintf("Interaccion triple significativa (p = %.4f %s).\n",
                    p_triple, .ink_signif(p_triple)))
        cat("Se procede con analisis de efectos simples (emmeans).\n\n")
        em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~",
                                                        tratamiento, "|", fa, "+", fb)))
        cat("Medias estimadas:\n")
        print(em$emmeans)
        cat("\nContrastes (Tukey):\n")
        print(em$contrasts)
        return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
      } else {
        cat("Interaccion triple no significativa. Se comparan efectos principales.\n\n")
        tukey_a <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
        tukey_b <- agricolae::HSD.test(modelo, fa, group = TRUE)
        tukey_c <- agricolae::HSD.test(modelo, fb, group = TRUE)
        cat(sprintf("Factor: %s\n", tratamiento))
        .ink_print_tukey(tukey_a, tratamiento)
        cat(sprintf("Factor: %s\n", fa))
        .ink_print_tukey(tukey_b, fa)
        cat(sprintf("Factor: %s\n", fb))
        .ink_print_tukey(tukey_c, fb)
        return(invisible(list(diseno = diseno, modelo = modelo,
                              tukey_a = tukey_a, tukey_b = tukey_b,
                              tukey_c = tukey_c)))
      }
    } else {
      p_interaccion <- tryCatch(
        summary(modelo)[[1]][paste0(tratamiento, ":", fa), "Pr(>F)"],
        error = function(e) NA)
      if (!is.na(p_interaccion) && p_interaccion < 0.05) {
        cat(sprintf("Interaccion %s x %s significativa (p = %.4f %s).\n",
                    tratamiento, fa, p_interaccion, .ink_signif(p_interaccion)))
        cat("Se procede con analisis de efectos simples (emmeans).\n\n")
        em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~",
                                                        tratamiento, "|", fa)))
        cat("Medias estimadas:\n")
        print(em$emmeans)
        cat("\nContrastes (Tukey):\n")
        print(em$contrasts)
        return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
      } else {
        cat("Interaccion no significativa. Se comparan efectos principales.\n\n")
        tukey_trat <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
        tukey_fa   <- agricolae::HSD.test(modelo, fa, group = TRUE)
        cat(sprintf("Factor: %s\n", tratamiento))
        .ink_print_tukey(tukey_trat, tratamiento)
        cat(sprintf("Factor: %s\n", fa))
        .ink_print_tukey(tukey_fa, fa)
        return(invisible(list(diseno = diseno, modelo = modelo,
                              tukey_trat = tukey_trat, tukey_fa = tukey_fa)))
      }
    }

  } else if (diseno == "Factorial con Ambientes") {
    p_interaccion <- tryCatch(
      summary(modelo)[[1]][paste0(tratamiento, ":", env), "Pr(>F)"],
      error = function(e) NA)
    if (!is.na(p_interaccion) && p_interaccion < 0.05) {
      cat(sprintf("Interaccion tratamiento x ambiente significativa (p = %.4f %s).\n",
                  p_interaccion, .ink_signif(p_interaccion)))
      cat("Se comparan tratamientos dentro de cada ambiente.\n\n")
      em <- emmeans::emmeans(modelo,
                             as.formula(paste("pairwise ~", tratamiento, "|", env)))
      cat("Medias estimadas por ambiente:\n")
      print(em$emmeans)
      cat("\nContrastes (Tukey):\n")
      print(em$contrasts)
      return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
    } else {
      cat("Interaccion tratamiento x ambiente no significativa.\n")
      cat("Se comparan efectos principales.\n\n")
      tukey_trat <- tryCatch(agricolae::HSD.test(modelo, tratamiento, group = TRUE),
                             error = function(e) NULL)
      tukey_fa   <- if (!is.null(fa_usuario))
        tryCatch(agricolae::HSD.test(modelo, fa, group = TRUE),
                 error = function(e) NULL) else NULL
      tukey_env  <- tryCatch(agricolae::HSD.test(modelo, env, group = TRUE),
                             error = function(e) NULL)
      if (!is.null(tukey_trat)) {
        cat(sprintf("Factor: %s\n", tratamiento))
        .ink_print_tukey(tukey_trat, tratamiento)
      }
      if (!is.null(tukey_fa)) {
        cat(sprintf("Factor: %s\n", fa))
        .ink_print_tukey(tukey_fa, fa)
      }
      if (!is.null(tukey_env)) {
        cat(sprintf("Factor: %s\n", env))
        .ink_print_tukey(tukey_env, env)
      }
      return(invisible(list(diseno = diseno, modelo = modelo,
                            tukey_trat = tukey_trat,
                            tukey_fa   = tukey_fa,
                            tukey_env  = tukey_env)))
    }

  } else if (diseno == "Parcelas Divididas") {
    cat("Prueba de Tukey mediante emmeans para Parcelas Divididas.\n\n")
    em <- tryCatch(
      emmeans::emmeans(modelo, as.formula(paste("pairwise ~", fa, "|", fb))),
      error = function(e) NULL)
    if (!is.null(em)) {
      cat("Medias estimadas:\n")
      print(em$emmeans)
      cat("\nContrastes (Tukey):\n")
      print(em$contrasts)
    }
    return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))

  } else if (diseno == "Cuadrado Latino") {
    tukey <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
    .ink_print_tukey(tukey, tratamiento)
    return(invisible(list(diseno = diseno, modelo = modelo, tukey = tukey)))

  } else if (diseno == "Strip-plot") {
    col_strip_r <- cols[cols %in% c("strip_row", "row_trt", "horizontal_factor",
                                    "factor_row", "row_factor")][1]
    col_strip_c <- cols[cols %in% c("strip_col", "col_trt", "vertical_factor",
                                    "factor_col", "column_factor", "cross_strip")][1]
    if (!is.na(col_strip_r) && !is.na(col_strip_c)) {
      em <- tryCatch(
        emmeans::emmeans(modelo,
                         as.formula(paste("pairwise ~", col_strip_r, "|", col_strip_c))),
        error = function(e) NULL)
      if (!is.null(em)) {
        cat("Medias estimadas (Factor Fila | Factor Columna):\n")
        print(em$emmeans)
        cat("\nContrastes (Tukey):\n")
        print(em$contrasts)
      }
      return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
    } else {
      tukey <- tryCatch(agricolae::HSD.test(modelo, tratamiento, group = TRUE),
                        error = function(e) NULL)
      if (!is.null(tukey)) .ink_print_tukey(tukey, tratamiento)
      return(invisible(list(diseno = diseno, modelo = modelo, tukey = tukey)))
    }

  } else if (diseno == "Repeated Measures") {
    col_tiempo <- cols[cols %in% pal_tiempo][1]
    if (!is.na(col_tiempo)) {
      em <- tryCatch(
        emmeans::emmeans(modelo,
                         as.formula(paste("pairwise ~", tratamiento, "|", col_tiempo))),
        error = function(e) NULL)
      if (!is.null(em)) {
        cat("Medias estimadas (Tratamiento | Tiempo):\n")
        print(em$emmeans)
        cat("\nContrastes (Tukey):\n")
        print(em$contrasts)
      }
      return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
    } else {
      tukey <- tryCatch(agricolae::HSD.test(modelo, tratamiento, group = TRUE),
                        error = function(e) NULL)
      if (!is.null(tukey)) .ink_print_tukey(tukey, tratamiento)
      return(invisible(list(diseno = diseno, modelo = modelo, tukey = tukey)))
    }

  } else {
    em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~", tratamiento)))
    cat("Medias estimadas:\n")
    print(em$emmeans)
    cat("\nContrastes (Tukey):\n")
    print(em$contrasts)
    return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
  }
}
