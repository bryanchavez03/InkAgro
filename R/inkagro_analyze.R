inkagro_analyze <- function(datos, respuesta, tratamiento,
                            bloque = NULL, fa = NULL, fb = NULL,
                            rep = NULL, iblock = NULL,
                            gen = NULL, env = NULL,
                            verbose = TRUE,
                            n_obs = nrow(datos),
                            n_vars = ncol(datos)) {

  # 1. Detectar diseno
  diseno <- inkagro_detect(datos, verbose = FALSE)

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

  # 3. Detectar columnas automaticamente
  cols <- tolower(names(datos))

  if (is.null(fa)) {
    factores <- cols[cols %in% pal_factor]
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

  # 4. Convertir a factor con trimws
  datos[[tratamiento]] <- as.factor(trimws(as.character(datos[[tratamiento]])))
  if (!is.null(bloque))  datos[[bloque]]  <- as.factor(trimws(as.character(datos[[bloque]])))
  if (!is.null(fa))      datos[[fa]]      <- as.factor(trimws(as.character(datos[[fa]])))
  if (!is.null(fb))      datos[[fb]]      <- as.factor(trimws(as.character(datos[[fb]])))
  if (!is.null(rep))     datos[[rep]]     <- as.factor(trimws(as.character(datos[[rep]])))
  if (!is.null(iblock))  datos[[iblock]]  <- as.factor(trimws(as.character(datos[[iblock]])))
  if (!is.null(gen))     datos[[gen]]     <- as.factor(trimws(as.character(datos[[gen]])))
  if (!is.null(env))     datos[[env]]     <- as.factor(trimws(as.character(datos[[env]])))

  # Remover outliers extremos
  q1  <- quantile(datos[[respuesta]], 0.25, na.rm = TRUE)
  q3  <- quantile(datos[[respuesta]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  datos <- datos[
    datos[[respuesta]] >= (q1 - 3 * iqr) &
      datos[[respuesta]] <= (q3 + 3 * iqr), ]

  # 5. Modelo segun diseno
  if (diseno == "DCA") {
    formula <- as.formula(paste(respuesta, "~", tratamiento))
    modelo  <- aov(formula, data = datos)

  } else if (diseno == "DBCA") {
    formula <- as.formula(paste(respuesta, "~", bloque, "+", tratamiento))
    modelo  <- aov(formula, data = datos)

  } else if (diseno == "Factorial") {
    if (!is.null(bloque)) {
      formula <- as.formula(paste(respuesta, "~", bloque, "+",
                                  tratamiento, "*", fa, "*", fb))
    } else {
      formula <- as.formula(paste(respuesta, "~",
                                  tratamiento, "*", fa, "*", fb))
    }
    modelo <- aov(formula, data = datos)

  } else if (diseno == "Factorial con Ambientes") {
    if (!is.null(bloque)) {
      formula <- as.formula(paste(respuesta, "~", bloque, "+",
                                  tratamiento, "*", env))
    } else {
      formula <- as.formula(paste(respuesta, "~",
                                  tratamiento, "*", env))
    }
    modelo <- aov(formula, data = datos)

  } else if (diseno == "Parcelas Divididas") {
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

  } else {
    stop("Diseno no soportado en esta version.")
  }

  # 6. Output con cli
  cli::cli_h1("InkAgro v0.1.0 — Analisis de Datos Agricolas")

  n_trat   <- nlevels(as.factor(datos[[tratamiento]]))
  n_bloque <- if (!is.null(bloque)) nlevels(as.factor(datos[[bloque]])) else NULL

  cli::cli_dl(c(
    "Datos"       = "{n_obs} observaciones x {n_vars} variables",
    "Diseno"      = diseno,
    "Respuesta"   = respuesta,
    "Tratamiento" = "{tratamiento} ({n_trat} niveles)"
  ))
  if (!is.null(bloque))
    cli::cli_text("  Bloque: {bloque} ({n_bloque} bloques)")
  if (!is.null(env))
    cli::cli_text("  Ambiente: {env}")

  # 7. Reporte de calidad
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

  # 8. Tabla ANOVA
  cli::cli_h2("Tabla ANOVA")
  es_lmer <- inherits(modelo, "lmerMod")
  if (es_lmer) {
    print(summary(modelo))
  } else {
    anova_sum <- summary(modelo)
    anova_tab <- as.data.frame(anova_sum[[1]])
    anova_tab[["F value"]] <- round(anova_tab[["F value"]], 2)
    anova_tab[["Pr(>F)"]]  <- format.pval(anova_tab[["Pr(>F)"]],
                                          digits = 3, eps = 2e-16)
    anova_tab[["Sum Sq"]]  <- round(anova_tab[["Sum Sq"]], 2)
    anova_tab[["Mean Sq"]] <- round(anova_tab[["Mean Sq"]], 2)
    print(anova_tab)
  }

  # 9. Supuestos
  inkagro_supuestos(modelo, datos, tratamiento)

  # 10. Comparacion de medias
  cli::cli_h2("Comparacion de Medias — Tukey HSD")

  if (diseno %in% c("DCA", "DBCA")) {
    tukey <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
    print(tukey$groups)
    return(invisible(list(diseno = diseno, modelo = modelo, tukey = tukey)))

  } else if (diseno == "Factorial") {
    if (!is.null(fa) & !is.null(fb)) {
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
        print(tukey_a$groups)
        print(tukey_b$groups)
        print(tukey_c$groups)
        return(invisible(list(diseno = diseno, modelo = modelo,
                              tukey_a = tukey_a, tukey_b = tukey_b,
                              tukey_c = tukey_c)))
      }
    } else {
      p_interaccion <- tryCatch(
        summary(modelo)[[1]][paste0(fa, ":", fb), "Pr(>F)"],
        error = function(e) NA)
      if (!is.na(p_interaccion) && p_interaccion < 0.05) {
        cli::cli_alert_warning("Interaccion significativa (p={round(p_interaccion,4)})")
        cli::cli_alert_info("Usando efectos simples con emmeans")
        em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~",
                                                        fa, "|", fb)))
        cli::cli_h3("Medias estimadas")
        print(em$emmeans)
        cli::cli_h3("Contrastes (Tukey)")
        print(em$contrasts)
        return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
      } else {
        cli::cli_alert_info("Sin interaccion significativa")
        tukey_a <- agricolae::HSD.test(modelo, fa, group = TRUE)
        tukey_b <- agricolae::HSD.test(modelo, fb, group = TRUE)
        print(tukey_a$groups)
        print(tukey_b$groups)
        return(invisible(list(diseno = diseno, modelo = modelo,
                              tukey_a = tukey_a, tukey_b = tukey_b)))
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
      tukey_env <- tryCatch(
        agricolae::HSD.test(modelo, env, group = TRUE),
        error = function(e) NULL)
      if (!is.null(tukey_trat)) print(tukey_trat$groups)
      if (!is.null(tukey_env))  print(tukey_env$groups)
      return(invisible(list(diseno = diseno, modelo = modelo,
                            tukey_trat = tukey_trat,
                            tukey_env  = tukey_env)))
    }

  } else if (diseno == "Cuadrado Latino") {
    tukey <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
    print(tukey$groups)
    return(invisible(list(diseno = diseno, modelo = modelo, tukey = tukey)))

  } else {
    em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~", tratamiento)))
    cli::cli_h3("Medias estimadas")
    print(em$emmeans)
    cli::cli_h3("Contrastes (Tukey)")
    print(em$contrasts)
    return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
  }
}
