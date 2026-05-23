inkagro_analyze <- function(datos, respuesta, tratamiento,
                            bloque = NULL, fa = NULL, fb = NULL,
                            rep = NULL, iblock = NULL,
                            gen = NULL, env = NULL,
                            verbose = TRUE,
                            n_obs = nrow(datos),
                            n_vars = ncol(datos))  {

  #  Detectar diseno
  diseno <- inkagro_detect(datos, verbose = FALSE)
  # Convertir parametros a minuscula para coincidir con datos limpios
  respuesta   <- tolower(respuesta)
  tratamiento <- tolower(tratamiento)
  if (!is.null(bloque))  bloque  <- tolower(bloque)
  if (!is.null(fa))      fa      <- tolower(fa)
  if (!is.null(fb))      fb      <- tolower(fb)
  if (!is.null(rep))     rep     <- tolower(rep)
  if (!is.null(iblock))  iblock  <- tolower(iblock)
  if (!is.null(gen))     gen     <- tolower(gen)
  if (!is.null(env))     env     <- tolower(env)
  #  Detectar columnas automaticamente si no se declaran
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
  #  Convertir a factor
  datos[[tratamiento]] <- as.factor(datos[[tratamiento]])
  if (!is.null(bloque))  datos[[bloque]]  <- as.factor(datos[[bloque]])
  if (!is.null(fa))      datos[[fa]]      <- as.factor(datos[[fa]])
  if (!is.null(fb))      datos[[fb]]      <- as.factor(datos[[fb]])
  if (!is.null(rep))     datos[[rep]]     <- as.factor(datos[[rep]])
  if (!is.null(iblock))  datos[[iblock]]  <- as.factor(datos[[iblock]])
  if (!is.null(gen))     datos[[gen]]     <- as.factor(datos[[gen]])
  if (!is.null(env))     datos[[env]]     <- as.factor(datos[[env]])

  # Modelo segun diseno
  if (diseno == "DCA") {
    formula <- as.formula(paste(respuesta, "~", tratamiento))
    modelo  <- aov(formula, data = datos)

  } else if (diseno == "DBCA") {
    formula <- as.formula(paste(respuesta, "~", bloque, "+", tratamiento))
    modelo  <- aov(formula, data = datos)

  } else if (diseno == "Factorial" & !is.null(fa) & !is.null(fb) & !is.null(bloque)) {
    formula <- as.formula(paste(respuesta, "~", bloque, "+", fa, "*", fb))
    modelo  <- aov(formula, data = datos)

  } else if (diseno == "Factorial" & !is.null(fa) & !is.null(fb)) {
    # Trifactorial — tratamiento es el tercer factor
    formula <- as.formula(paste(respuesta, "~", bloque, "+",
                                tratamiento, "*", fa, "*", fb))
    modelo  <- aov(formula, data = datos)

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
    formula  <- as.formula(paste(respuesta, "~", tratamiento, "+", col_fila, "+", col_col))
    modelo   <- aov(formula, data = datos)
  } else {
    stop("Diseño no soportado en esta versión.")
  }

  #  Output limpio
  cat("\n")
  cat("=========================================\n")
  cat(" InkAgro v0.1.0\n")
  cat(" Analisis de Datos Agricolas\n")
  cat("=========================================\n\n")
  cat(" Datos         :", n_obs, "observaciones x", n_vars, "variables\n")
  cat(" Diseno        :", diseno, "\n")
  cat(" Respuesta     :", respuesta, "\n")
  cat(" Tratamiento   :", tratamiento, "\n")
  if (!is.null(bloque)) cat(" Bloque        :", bloque, "\n")
  cat("\n")
  cat("-----------------------------------------\n")
  cat(" Reporte de Calidad\n")
  cat("-----------------------------------------\n")
  na_reporte <- attr(datos, "na_reporte")
  if (is.null(na_reporte)) na_reporte <- colSums(is.na(datos))
  na_total <- sum(na_reporte)
  if (na_total == 0) {
    cat(" Valores faltantes : Ninguno\n")
  } else {
    cat(" Valores faltantes :", na_total, "en columnas:\n")
    cols_na <- na_reporte[na_reporte > 0]
    for (i in seq_along(cols_na)) {
      cat("  -", names(cols_na)[i], ":", cols_na[i], "\n")
    }
  }
  cat(" Variables         :", n_vars, "\n\n")
  cat("-----------------------------------------\n")
  cat(" Tabla ANOVA\n")
  cat("-----------------------------------------\n")
  print(summary(modelo))

  cat("\n")
  cat("-----------------------------------------\n")
  cat(" Comparacion de Medias — Tukey HSD\n")
  cat("-----------------------------------------\n")

  if (diseno %in% c("DCA", "DBCA")) {
    tukey <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
    print(tukey$groups)
    return(invisible(list(diseno = diseno, modelo = modelo, tukey = tukey)))

  } else if (diseno == "Factorial") {
    if (!is.null(fa) & !is.null(fb)) {
      # Trifactorial con bloque
      p_triple <- summary(modelo)[[1]][paste0(tratamiento, ":", fa, ":", fb), "Pr(>F)"]
      if (!is.na(p_triple) && p_triple < 0.05) {
        cat(" Interaccion triple significativa (p < 0.05)\n")
        cat(" Usando efectos simples con emmeans\n\n")
        em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~", tratamiento, "|", fa, "+", fb)))
        print(em)
        return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
      } else {
        cat(" Sin interaccion triple significativa\n")
        cat(" Comparando factores principales\n\n")
        tukey_a <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
        tukey_b <- agricolae::HSD.test(modelo, fa, group = TRUE)
        tukey_c <- agricolae::HSD.test(modelo, fb, group = TRUE)
        print(tukey_a$groups)
        print(tukey_b$groups)
        print(tukey_c$groups)
        return(invisible(list(diseno = diseno, modelo = modelo,
                              tukey_a = tukey_a, tukey_b = tukey_b, tukey_c = tukey_c)))
      }
    } else {
      p_interaccion <- summary(modelo)[[1]][paste0(fa, ":", fb), "Pr(>F)"]
      if (!is.na(p_interaccion) && p_interaccion < 0.05) {
        cat(" Interaccion significativa (p < 0.05)\n")
        cat(" Usando efectos simples con emmeans\n\n")
        em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~", fa, "|", fb)))
        print(em)
        return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
      } else {
        cat(" Sin interaccion significativa\n")
        cat(" Comparando factores principales\n\n")
        tukey_a <- agricolae::HSD.test(modelo, fa, group = TRUE)
        tukey_b <- agricolae::HSD.test(modelo, fb, group = TRUE)
        print(tukey_a$groups)
        print(tukey_b$groups)
        return(invisible(list(diseno = diseno, modelo = modelo,
                              tukey_a = tukey_a, tukey_b = tukey_b)))
      }
    }
  } else if (diseno == "Cuadrado Latino") {
    col_fila <- cols[cols %in% pal_fila][1]
    col_col  <- cols[cols %in% pal_columna][1]
    formula  <- as.formula(paste(respuesta, "~", tratamiento, "+", col_fila, "+", col_col))
    modelo   <- aov(formula, data = datos)

    em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~", tratamiento)))
    print(em)
    return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
  }
}
