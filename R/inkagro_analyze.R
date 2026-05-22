inkagro_analyze <- function(datos, respuesta, tratamiento,
                            bloque = NULL, fa = NULL, fb = NULL,
                            rep = NULL, iblock = NULL,
                            gen = NULL, env = NULL) {

  # 1. Detectar diseno
  diseno <- inkagro_detect(datos)

  # 2. Convertir a factor
  datos[[tratamiento]] <- as.factor(datos[[tratamiento]])
  if (!is.null(bloque))  datos[[bloque]]  <- as.factor(datos[[bloque]])
  if (!is.null(fa))      datos[[fa]]      <- as.factor(datos[[fa]])
  if (!is.null(fb))      datos[[fb]]      <- as.factor(datos[[fb]])
  if (!is.null(rep))     datos[[rep]]     <- as.factor(datos[[rep]])
  if (!is.null(iblock))  datos[[iblock]]  <- as.factor(datos[[iblock]])
  if (!is.null(gen))     datos[[gen]]     <- as.factor(datos[[gen]])
  if (!is.null(env))     datos[[env]]     <- as.factor(datos[[env]])

  # 3. Modelo segun diseno
  if (diseno == "DCA") {
    formula <- as.formula(paste(respuesta, "~", tratamiento))
    modelo  <- aov(formula, data = datos)

  } else if (diseno == "DBCA") {
    formula <- as.formula(paste(respuesta, "~", bloque, "+", tratamiento))
    modelo  <- aov(formula, data = datos)

  } else if (diseno == "Factorial") {
    formula <- as.formula(paste(respuesta, "~", fa, "*", fb))
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

  } else {
    stop("Diseno no soportado en esta version.")
  }

  # 4. Output InkAgro
  cat("\n")
  cat("=========================================\n")
  cat(" InkAgro v0.1.0\n")
  cat(" Diseno detectado :", diseno, "\n")
  cat(" Variable respuesta:", respuesta, "\n")
  cat("=========================================\n\n")

  cat("-----------------------------------------\n")
  cat(" Tabla ANOVA\n")
  cat("-----------------------------------------\n")
  print(summary(modelo))

  # 5. Comparacion de medias
  cat("\n")
  cat("-----------------------------------------\n")
  cat(" Comparacion de medias\n")
  cat("-----------------------------------------\n")

  if (diseno %in% c("DCA", "DBCA")) {
    tukey <- agricolae::HSD.test(modelo, tratamiento, group = TRUE)
    print(tukey$groups)
    return(invisible(list(diseno = diseno, modelo = modelo, tukey = tukey)))

  } else if (diseno == "Factorial") {
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

  } else {
    em <- emmeans::emmeans(modelo, as.formula(paste("pairwise ~", tratamiento)))
    print(em)
    return(invisible(list(diseno = diseno, modelo = modelo, emmeans = em)))
  }
}
