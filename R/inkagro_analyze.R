# Imprime tabla Tukey de forma compacta cuando hay muchos tratamientos
.ink_print_tukey <- function(hsd_obj) {
  tab <- hsd_obj$groups
  n   <- nrow(tab)
  if (n > 15) {
    cli::cli_alert_info(
      "Tabla con {n} niveles — mostrando los primeros 10.")
    print(head(tab, 10))
    cli::cli_text("  ... ({n - 10} niveles mas) ...")
    cli::cli_alert_info(
      "Use resultado$tukey$groups para ver la tabla completa.")
  } else {
    print(tab)
  }
}

inkagro_analyze <- function(datos, respuesta, tratamiento,
                            bloque = NULL, fa = NULL, fb = NULL,
                            rep = NULL, iblock = NULL,
                            gen = NULL, env = NULL,
                            diseno = NULL,
                            verbose = TRUE,
                            n_obs = nrow(datos),
                            n_vars = ncol(datos)) {

  # Guardar fa original del usuario antes de autodeteccion
  fa_usuario <- fa

  # 1. Estandarizar nombres de datos
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
  if (!respuesta %in% names(datos)) {
    stop("La variable respuesta '", respuesta, "' no existe en los datos.")
  }
  if (!is.numeric(datos[[respuesta]])) {
    stop("La variable respuesta '", respuesta, "' no es numerica.")
  }
  if (!tratamiento %in% names(datos)) {
    stop("El tratamiento '", tratamiento, "' no existe en los datos.")
  }
  if (nlevels(as.factor(datos[[tratamiento]])) < 2) {
    stop("El tratamiento '", tratamiento, "' tiene menos de 2 niveles.")
  }

  # 4. Usar diseno recibido o detectar
  if (is.null(diseno)) {
    diseno <- inkagro_detect(datos, verbose = FALSE)
  }
  if (diseno == "DBCA" && !is.null(fa_usuario)) {
    diseno <- "Factorial"
  }
  if (diseno == "Factorial con Ambientes" && !is.null(fa_usuario) && is.null(env)) {
    diseno <- "Factorial"
  }

  # Fallback cuando el diseno no pudo detectarse automaticamente.
  # Ocurre cuando el nombre de la columna de tratamiento no esta en pal_trat
  # (ej. "Factor_P" esta en pal_factor, no en pal_trat).
  if (grepl("No detectado", diseno)) {
    if (!is.null(bloque) && bloque %in% names(datos)) {
      cli::cli_alert_warning(
        "Diseno no detectado automaticamente — se usara DBCA como alternativa.")
      diseno <- "DBCA"
    } else {
      cli::cli_alert_warning(
        "Diseno no detectado automaticamente — se usara DCA como alternativa.")
      diseno <- "DCA"
    }
  }

  # 5. Detectar columnas automaticamente
  cols <- names(datos)

  if (is.null(fa)) {
    # Excluir columnas ya asignadas como tratamiento, bloque, etc.
    # Evita que "Factor_P" u otras columnas en pal_factor sean
    # reutilizadas como fa cuando ya cumplen el rol de tratamiento principal.
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
    datos[[bloque]]  <- as.factor(trimws(as.character(datos[[bloque]])))
  if (!is.null(fa) && fa %in% names(datos))
    datos[[fa]]      <- as.factor(trimws(as.character(datos[[fa]])))
  if (!is.null(fb) && fb %in% names(datos))
    datos[[fb]]      <- as.factor(trimws(as.character(datos[[fb]])))
  if (!is.null(rep) && rep %in% names(datos))
    datos[[rep]]     <- as.factor(trimws(as.character(datos[[rep]])))
  if (!is.null(iblock) && iblock %in% names(datos))
    datos[[iblock]]  <- as.factor(trimws(as.character(datos[[iblock]])))
  if (!is.null(gen) && gen %in% names(datos))
    datos[[gen]]     <- as.factor(trimws(as.character(datos[[gen]])))
  if (!is.null(env) && env %in% names(datos))
    datos[[env]]     <- as.factor(trimws(as.character(datos[[env]])))

  # Eliminar filas con NA en columnas clave del diseño experimental
  # (tratamiento, bloque, fa, fb, etc.) — no se pueden imputar porque
  # definen la estructura del experimento
  cols_diseno <- unique(c(tratamiento, bloque, fa, fb, rep, env, gen, iblock))
  cols_diseno <- cols_diseno[!is.null(cols_diseno) & cols_diseno %in% names(datos)]
  if (length(cols_diseno) > 0) {
    na_diseno <- rowSums(is.na(datos[, cols_diseno, drop = FALSE])) > 0
    if (any(na_diseno)) {
      n_eliminadas <- sum(na_diseno)
      cli::cli_alert_warning(
        "Se eliminaron {n_eliminadas} fila(s) con NA en columnas del diseno.")
      datos <- datos[!na_diseno, ]
    }
  }

  # Remover outliers extremos
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
        formula <- as.formula(paste(respuesta, "~",
                                    tratamiento, "*", fa))
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
    if (is.null(fa) || is.null(fb) || is.null(bloque)) {
      stop(
        "Parcelas Divididas requiere tres columnas: fa (parcela principal), ",
        "fb (subparcela) y bloque. No se detectaron automaticamente. ",
        "Especificalas manualmente: inkagro_analyze(..., fa='col_fa', fb='col_fb', bloque='col_bloque')"
      )
    }
    if (!fa     %in% names(datos)) stop("La columna fa='",     fa,     "' no existe en los datos.")
    if (!fb     %in% names(datos)) stop("La columna fb='",     fb,     "' no existe en los datos.")
    if (!bloque %in% names(datos)) stop("La columna bloque='", bloque, "' no existe en los datos.")
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
        # Modelo mixto: bloque + efectos de fila y columna de strip anidados en bloque
        formula <- as.formula(paste(
          respuesta, "~", col_strip_r, "*", col_strip_c,
          "+ (1|", bloque, ") + (1|", bloque, ":", col_strip_r,
          ") + (1|", bloque, ":", col_strip_c, ")"
        ))
        modelo <- lme4::lmer(formula, data = datos)
      } else {
        formula <- as.formula(paste(respuesta, "~", col_strip_r, "*", col_strip_c))
        modelo  <- aov(formula, data = datos)
      }
    } else {
      cli::cli_alert_warning(
        "No se detectaron columnas strip (row_trt / col_trt). Usando DCA como alternativa.")
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
          "+ Error(", col_parcela, "/", col_tiempo, ")"
        ))
      } else {
        formula <- as.formula(paste(
          respuesta, "~", tratamiento, "*", col_tiempo,
          "+ Error(", col_parcela, "/", col_tiempo, ")"
        ))
      }
      modelo <- aov(formula, data = datos)
    } else {
      cli::cli_alert_warning(
        "No se detectaron columnas de parcela/tiempo. Usando DCA como alternativa.")
      formula <- as.formula(paste(respuesta, "~", tratamiento))
      modelo  <- aov(formula, data = datos)
    }

  } else {
    stop("Diseno no soportado en esta version.")
  }

  # 8. Output con cli
  cli::cli_h1("InkAgro v0.1.0 — Analisis de Datos Agricolas")
  n_trat   <- nlevels(as.factor(datos[[tratamiento]]))
  n_bloque <- if (!is.null(bloque)) nlevels(as.factor(datos[[bloque]])) else NULL
  cli::cli_dl(c(
    "Datos"       = "{n_obs} observaciones x {n_vars} variables",
    "Diseno"      = diseno,
    "Respuesta"   = respuesta,
    "Tratamiento" = "{tratamiento} ({n_trat} niveles)"
  ))
  if (!is.null(bloque) && bloque %in% names(datos))
    cli::cli_text("  Bloque: {bloque} ({n_bloque} bloques)")
  if (!is.null(env) && env %in% names(datos) && !diseno %in% c("DCA", "DBCA"))
    cli::cli_text("  Ambiente: {env}")

  # 9. Reporte de calidad
  cli::cli_h2("Reporte de Calidad")
  na_reporte <- colSums(is.na(datos))
  na_total   <- sum(na_reporte)
  if (na_total == 0) {
    cli::cli_alert_success("Sin valores faltantes")
  } else {
    cols_na <- na_reporte[na_reporte > 0]
    for (i in seq_along(cols_na)) {
      pct <- round(cols_na[i] / nrow(datos) * 100, 1)
      cli::cli_alert_danger("{names(cols_na)[i]}: {cols_na[i]} NA ({pct}% de observaciones)")
    }
  }
  cli::cli_text("  Variables: {n_vars}")

  # 10. Tabla ANOVA
  cli::cli_h2("Tabla ANOVA")
  es_lmer    <- inherits(modelo, "lmerMod")
  es_parcela <- diseno == "Parcelas Divididas"

  if (es_lmer) {
    print(summary(modelo))
  } else if (es_parcela) {
    anova_sum <- summary(modelo)
    for (estrato in names(anova_sum)) {
      cli::cli_text("── Estrato: {estrato}")
      tab <- tryCatch(as.data.frame(anova_sum[[estrato]]), error = function(e) NULL)
      if (!is.null(tab)) {
        tab[["F value"]] <- round(tab[["F value"]], 2)
        tab[["Pr(>F)"]]  <- format.pval(tab[["Pr(>F)"]], digits = 3, eps = 2e-16)
        tab[["Sum Sq"]]  <- round(tab[["Sum Sq"]], 2)
        tab[["Mean Sq"]] <- round(tab[["Mean Sq"]], 2)
        print(tab)
      }
    }
  } else {
    anova_sum <- summary(modelo)
    anova_tab <- as.data.frame(anova_sum[[1]])
    anova_tab[["F value"]] <- round(anova_tab[["F value"]], 2)
    anova_tab[["Pr(>F)"]]  <- format.pval(anova_tab[["Pr(>F)"]], digits = 3, eps = 2e-16)
    anova_tab[["Sum Sq"]]  <- round(anova_tab[["Sum Sq"]], 2)
    anova_tab[["Mean Sq"]] <- round(anova_tab[["Mean Sq"]], 2)
    print(anova_tab)
  }

  # 11. Supuestos
  inkagro_supuestos(modelo, datos, tratamiento)

  # 12. Comparacion de medias
  cli::cli_h2("Comparacion de Medias — Tukey HSD")

  if (diseno %in% c("DCA", "DBCA")) {
    tukey <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
    .ink_print_tukey(tukey)
    return(invisible(list(diseno = diseno, modelo = modelo, tukey = tukey)))

  } else if (diseno == "Factorial") {
    if (!is.null(fa) && !is.null(fb)) {
      p_triple <- tryCatch(
        summary(modelo)[[1]][paste0(tratamiento, ":", fa, ":", fb), "Pr(>F)"],
        error = function(e) NA)
      if (!is.na(p_triple) && p_triple < 0.05) {
        cli::cli_alert_warning("Interaccion triple significativa (p={round(p_triple,4)})")
        cli::cli_alert_info("Usando efectos simples con emmeans")
        em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~",
                                                        tratamiento, "|", fa, "+", fb)))
        cli::cli_h3("Medias estimadas")
        print(em$emmeans)
        cli::cli_h3("Contrastes (Tukey)")
        print(em$contrasts)
        return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
      } else {
        cli::cli_alert_info("Sin interaccion triple significativa")
        cli::cli_text("Comparando factores principales")
        tukey_a <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
        tukey_b <- agricolae::HSD.test(modelo, fa, group = TRUE)
        tukey_c <- agricolae::HSD.test(modelo, fb, group = TRUE)
        .ink_print_tukey(tukey_a)
        .ink_print_tukey(tukey_b)
        .ink_print_tukey(tukey_c)
        return(invisible(list(diseno = diseno, modelo = modelo,
                              tukey_a = tukey_a, tukey_b = tukey_b,
                              tukey_c = tukey_c)))
      }
    } else {
      p_interaccion <- tryCatch(
        summary(modelo)[[1]][paste0(tratamiento, ":", fa), "Pr(>F)"],
        error = function(e) NA)
      if (!is.na(p_interaccion) && p_interaccion < 0.05) {
        cli::cli_alert_warning("Interaccion significativa (p={round(p_interaccion,4)})")
        cli::cli_alert_info("Usando efectos simples con emmeans")
        em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~",
                                                        tratamiento, "|", fa)))
        cli::cli_h3("Medias estimadas")
        print(em$emmeans)
        cli::cli_h3("Contrastes (Tukey)")
        print(em$contrasts)
        return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
      } else {
        cli::cli_alert_info("Sin interaccion significativa")
        tukey_trat <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
        tukey_fa   <- agricolae::HSD.test(modelo, fa, group = TRUE)
        .ink_print_tukey(tukey_trat)
        .ink_print_tukey(tukey_fa)
        return(invisible(list(diseno = diseno, modelo = modelo,
                              tukey_trat = tukey_trat,
                              tukey_fa   = tukey_fa)))
      }
    }

  } else if (diseno == "Factorial con Ambientes") {
    p_interaccion <- tryCatch(
      summary(modelo)[[1]][paste0(tratamiento, ":", env), "Pr(>F)"],
      error = function(e) NA)
    if (!is.na(p_interaccion) && p_interaccion < 0.05) {
      cli::cli_alert_warning("Interaccion tratamiento x ambiente significativa (p={round(p_interaccion,4)})")
      cli::cli_alert_info("Comparando tratamiento dentro de cada ambiente")
      em <- emmeans::emmeans(modelo,
                             as.formula(paste("pairwise ~", tratamiento, "|", env)))
      cli::cli_h3("Medias estimadas por ambiente")
      print(em$emmeans)
      cli::cli_h3("Contrastes (Tukey)")
      print(em$contrasts)
      return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
    } else {
      cli::cli_alert_info("Sin interaccion significativa")
      cli::cli_text("Comparando factores principales")
      tukey_trat <- tryCatch(
        agricolae::HSD.test(modelo, tratamiento, group = TRUE),
        error = function(e) NULL)
      tukey_fa <- if (!is.null(fa_usuario)) tryCatch(
        agricolae::HSD.test(modelo, fa, group = TRUE),
        error = function(e) NULL) else NULL
      tukey_env <- tryCatch(
        agricolae::HSD.test(modelo, env, group = TRUE),
        error = function(e) NULL)
      if (!is.null(tukey_trat)) .ink_print_tukey(tukey_trat)
      if (!is.null(tukey_fa))   .ink_print_tukey(tukey_fa)
      if (!is.null(tukey_env))  .ink_print_tukey(tukey_env)
      return(invisible(list(diseno = diseno, modelo = modelo,
                            tukey_trat = tukey_trat,
                            tukey_fa   = tukey_fa,
                            tukey_env  = tukey_env)))
    }

  } else if (diseno == "Parcelas Divididas") {
    cli::cli_alert_info("Usando emmeans para Parcelas Divididas")
    em <- tryCatch(
      emmeans::emmeans(modelo, as.formula(paste("pairwise ~", fa, "|", fb))),
      error = function(e) NULL)
    if (!is.null(em)) {
      cli::cli_h3("Medias estimadas")
      print(em$emmeans)
      cli::cli_h3("Contrastes (Tukey)")
      print(em$contrasts)
    }
    return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))

  } else if (diseno == "Cuadrado Latino") {
    tukey <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
    .ink_print_tukey(tukey)
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
        cli::cli_h3("Medias estimadas — Factor Fila | Factor Columna")
        print(em$emmeans)
        cli::cli_h3("Contrastes (Tukey)")
        print(em$contrasts)
      }
      return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
    } else {
      tukey <- tryCatch(
        agricolae::HSD.test(modelo, tratamiento, group = TRUE),
        error = function(e) NULL)
      if (!is.null(tukey)) .ink_print_tukey(tukey)
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
        cli::cli_h3("Medias estimadas — Tratamiento | Tiempo")
        print(em$emmeans)
        cli::cli_h3("Contrastes (Tukey)")
        print(em$contrasts)
      }
      return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
    } else {
      tukey <- tryCatch(
        agricolae::HSD.test(modelo, tratamiento, group = TRUE),
        error = function(e) NULL)
      if (!is.null(tukey)) .ink_print_tukey(tukey)
      return(invisible(list(diseno = diseno, modelo = modelo, tukey = tukey)))
    }

  } else {
    em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~", tratamiento)))
    cli::cli_h3("Medias estimadas")
    print(em$emmeans)
    cli::cli_h3("Contrastes (Tukey)")
    print(em$contrasts)
    return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
  }
}
