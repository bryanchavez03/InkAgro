inkagro_supuestos <- function(modelo, datos, tratamiento) {

  cat("-----------------------------------------\n")
  cat(" Verificacion de Supuestos\n")
  cat("-----------------------------------------\n\n")

  es_lmer <- inherits(modelo, "lmerMod")

  if (es_lmer) {
    cat(" Modelo mixto (lmer) detectado\n")
    cat(" Supuesto verificado: normalidad de residuos\n")
    cat(" Para verificacion completa use DHARMa\n\n")
    residuos <- residuals(modelo)
    shapiro  <- shapiro.test(residuos)
    cat(" Shapiro-Wilk: W =", round(shapiro$statistic, 4),
        "| p =", round(shapiro$p.value, 4), "\n")
    if (shapiro$p.value >= 0.05) {
      cat(" Normalidad aceptada (p >= 0.05)\n\n")
    } else {
      cat(" ADVERTENCIA: Normalidad rechazada (p < 0.05)\n\n")
    }
    return(invisible(list(shapiro = shapiro)))

  } else {

    residuos <- residuals(modelo)

    # 1. Normalidad
    cat(" 1. Normalidad de residuos (Shapiro-Wilk)\n")
    shapiro <- NULL
    if (length(residuos) > 200) {
      cat("    Muestra grande (n > 200) — Shapiro-Wilk es muy sensible\n")
      cat("    Interprete el QQ-plot visualmente\n\n")
    } else {
      shapiro <- shapiro.test(residuos)
      cat("    W =", round(shapiro$statistic, 4),
          "| p =", round(shapiro$p.value, 4), "\n")
      if (shapiro$p.value >= 0.05) {
        cat("    Normalidad aceptada (p >= 0.05)\n\n")
      } else {
        cat("    ADVERTENCIA: Normalidad rechazada (p < 0.05)\n")
        cat("    Considere transformacion logaritmica\n\n")
      }
    }

    # 2. Homogeneidad
    cat(" 2. Homogeneidad de varianzas\n")

    datos_modelo <- tryCatch(model.frame(modelo), error = function(e) datos)
    trat_factor  <- droplevels(as.factor(datos_modelo[[tratamiento]]))
    res_clean    <- residuos

    bartlett <- NULL
    bartlett <- tryCatch({
      bt <- bartlett.test(res_clean ~ trat_factor)
      cat("    Bartlett: K2 =", round(bt$statistic, 4),
          "| p =", round(bt$p.value, 4), "\n")
      if (bt$p.value >= 0.05) {
        cat("    Varianzas homogeneas (p >= 0.05)\n")
      } else {
        cat("    ADVERTENCIA: Varianzas heterogeneas (p < 0.05)\n")
      }
      bt
    }, error = function(e) {
      cat("    No se pudo calcular Bartlett\n")
      NULL
    })

    levene <- NULL
    levene <- tryCatch({
      lv <- car::leveneTest(res_clean ~ trat_factor)
      cat("    Levene:   F =", round(lv$`F value`[1], 4),
          "| p =", round(lv$`Pr(>F)`[1], 4), "\n")
      if (lv$`Pr(>F)`[1] >= 0.05) {
        cat("    Varianzas homogeneas (p >= 0.05)\n\n")
      } else {
        cat("    ADVERTENCIA: Varianzas heterogeneas (p < 0.05)\n\n")
      }
      lv
    }, error = function(e) {
      cat("    No se pudo calcular Levene\n\n")
      NULL
    })

    # 3. Outliers
    cat(" 3. Deteccion de outliers\n")
    res_std  <- tryCatch(rstandard(modelo), error = function(e) NULL)
    outliers <- NULL
    if (!is.null(res_std)) {
      outliers <- which(abs(res_std) > 3)
      if (length(outliers) == 0) {
        cat("    Sin outliers extremos detectados\n\n")
      } else {
        cat("    ADVERTENCIA:", length(outliers),
            "outlier(s) en observaciones:",
            paste(outliers, collapse = ", "), "\n\n")
      }
    }

    # 4. Balance
    cat(" 4. Balance del diseno\n")
    tabla <- table(datos[[tratamiento]])
    if (length(unique(tabla)) == 1) {
      cat("    Balanceado —", unique(tabla),
          "observaciones por tratamiento\n\n")
    } else {
      cat("    ADVERTENCIA: Diseno desbalanceado\n")
      print(tabla)
      cat("\n")
    }

    return(invisible(list(
      shapiro  = shapiro,
      bartlett = bartlett,
      levene   = levene,
      outliers = outliers,
      balance  = tabla
    )))
  }
}
