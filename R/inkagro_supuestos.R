inkagro_supuestos <- function(modelo, datos, tratamiento) {

  es_lmer <- inherits(modelo, "lmerMod")
  residuos <- residuals(modelo)

  # --- Calcular CV experimental ---
  cv <- tryCatch({
    resp_col <- as.character(formula(modelo)[[2]])
    if (resp_col %in% names(datos)) {
      media_gral <- mean(datos[[resp_col]], na.rm = TRUE)
      rmse       <- sqrt(mean(residuos^2))
      (rmse / media_gral) * 100
    } else NA
  }, error = function(e) NA)

  cat("\n")
  cat("================================================================\n")
  cat("                  VERIFICACION DE SUPUESTOS                    \n")
  cat("================================================================\n")

  # --- Modelo mixto ---
  if (es_lmer) {
    cat("Nota: Modelo mixto (lmer). Para diagnostico completo use DHARMa.\n")
    shapiro <- tryCatch(shapiro.test(residuos), error = function(e) NULL)
    if (!is.null(shapiro)) {
      sig <- .ink_signif(shapiro$p.value)
      res <- if (shapiro$p.value >= 0.05) "No se rechaza normalidad" else "Se rechaza normalidad"
      cat(sprintf("Shapiro-Wilk: W = %.4f, p = %.4f %s  [%s]\n",
                  shapiro$statistic, shapiro$p.value, sig, res))
    }
    cat("================================================================\n\n")
    return(invisible(list(shapiro = shapiro)))
  }

  # --- 1. Normalidad ---
  cat("\n1. Normalidad de residuos\n")
  cat("   Prueba: Shapiro-Wilk\n")
  shapiro <- NULL

  if (length(residuos) > 200) {
    cat(sprintf("   Nota: n = %d (muestra grande). Shapiro-Wilk puede ser muy sensible.\n",
                length(residuos)))
    cat("   Recomendacion: interpretar el QQ-plot visualmente.\n")
  } else {
    shapiro <- tryCatch(shapiro.test(residuos), error = function(e) NULL)
    if (!is.null(shapiro)) {
      sig <- .ink_signif(shapiro$p.value)
      res <- if (shapiro$p.value >= 0.05) "No se rechaza normalidad"
      else "Se rechaza normalidad"
      cat(sprintf("   W = %.4f,  p-value = %.4f %s\n",
                  shapiro$statistic, shapiro$p.value, sig))
      cat(sprintf("   Conclusion: %s\n", res))
      if (shapiro$p.value < 0.05)
        cat("   Recomendacion: considere transformacion logaritmica o prueba no parametrica.\n")
    }
  }

  # --- 2. Homogeneidad de varianzas ---
  cat("\n2. Homogeneidad de varianzas\n")
  cat("   Pruebas: Bartlett y Levene\n")

  datos_modelo <- tryCatch(model.frame(modelo), error = function(e) datos)
  trat_factor  <- droplevels(as.factor(datos_modelo[[tratamiento]]))
  res_clean    <- residuos

  bartlett <- tryCatch(bartlett.test(res_clean ~ trat_factor),
                       error = function(e) NULL)
  levene   <- tryCatch(car::leveneTest(res_clean ~ trat_factor),
                       error = function(e) NULL)

  if (!is.null(bartlett)) {
    sig_b <- .ink_signif(bartlett$p.value)
    cat(sprintf("   Bartlett:  K2 = %.4f,  p-value = %.4f %s\n",
                bartlett$statistic, bartlett$p.value, sig_b))
  }
  if (!is.null(levene)) {
    sig_l <- .ink_signif(levene$`Pr(>F)`[1])
    cat(sprintf("   Levene:    F  = %.4f,  p-value = %.4f %s\n",
                levene$`F value`[1], levene$`Pr(>F)`[1], sig_l))
  }

  if (!is.null(bartlett) && !is.null(levene)) {
    bart_ok <- bartlett$p.value >= 0.05
    lev_ok  <- levene$`Pr(>F)`[1] >= 0.05
    if (bart_ok && lev_ok) {
      cat("   Conclusion: varianzas homogeneas (ambas pruebas no significativas).\n")
    } else if (!bart_ok && lev_ok) {
      cat("   Conclusion: resultados discordantes. Se recomienda preferir Levene\n")
      cat("               (mas robusto ante no-normalidad). Varianzas aceptadas.\n")
    } else if (bart_ok && !lev_ok) {
      cat("   Conclusion: resultados discordantes. Se recomienda preferir Levene.\n")
      cat("               Varianzas posiblemente heterogeneas.\n")
    } else {
      cat("   Conclusion: varianzas heterogeneas (ambas pruebas significativas).\n")
      cat("   Recomendacion: considere transformacion de datos o ANOVA de Welch.\n")
    }
  }

  # --- 3. Deteccion de outliers ---
  cat("\n3. Deteccion de valores atipicos\n")
  cat("   Criterio: residuos estandarizados > 3\n")
  res_std  <- tryCatch(rstandard(modelo), error = function(e) NULL)
  outliers <- NULL
  if (!is.null(res_std)) {
    outliers <- which(abs(res_std) > 3)
    if (length(outliers) == 0) {
      cat("   No se detectaron valores atipicos extremos.\n")
    } else {
      cat(sprintf("   Se detectaron %d valor(es) atipico(s) en las observaciones: %s\n",
                  length(outliers), paste(outliers, collapse = ", ")))
      cat("   Recomendacion: revisar estas observaciones antes de interpretar resultados.\n")
    }
  }

  # --- 4. Balance del diseno ---
  cat("\n4. Balance del diseno experimental\n")
  tabla <- table(datos[[tratamiento]])
  if (length(unique(tabla)) == 1) {
    cat(sprintf("   Diseno balanceado: %d observaciones por tratamiento.\n",
                unique(tabla)))
  } else {
    cat("   Diseno desbalanceado. Numero de observaciones por tratamiento:\n")
    print(tabla)
    cat("   Nota: el ANOVA de tipo III es apropiado para disenos desbalanceados.\n")
  }

  # --- 5. Coeficiente de variacion ---
  cat("\n5. Coeficiente de variacion (CV)\n")
  if (!is.na(cv)) {
    cat(sprintf("   CV = %.2f%%\n", cv))
    calidad <- if (cv <= 10) "Excelente precision experimental."
    else if (cv <= 20) "Precision aceptable para condiciones de campo."
    else if (cv <= 30) "Precision regular. Revisar manejo experimental."
    else "CV alto. Se recomienda revisar el protocolo experimental."
    cat(sprintf("   Interpretacion: %s\n", calidad))
  } else {
    cat("   CV no calculado.\n")
  }

  # --- Resumen final ---
  cat("\n")
  cat("----------------------------------------------------------------\n")
  cat(" RESUMEN DE SUPUESTOS\n")
  cat("----------------------------------------------------------------\n")

  # Normalidad
  if (!is.null(shapiro)) {
    norm_sig <- .ink_signif(shapiro$p.value)
    norm_res <- if (shapiro$p.value >= 0.05) "Cumple" else "No cumple"
    cat(sprintf(" Normalidad   (Shapiro-Wilk):  W=%.4f  p=%.4f %-3s  [%s]\n",
                shapiro$statistic, shapiro$p.value, norm_sig, norm_res))
  } else {
    cat(" Normalidad   (Shapiro-Wilk):  n>200, ver QQ-plot\n")
  }

  # Homogeneidad — preferir Levene
  if (!is.null(levene)) {
    hom_sig <- .ink_signif(levene$`Pr(>F)`[1])
    hom_res <- if (levene$`Pr(>F)`[1] >= 0.05) "Cumple" else "No cumple"
    cat(sprintf(" Homogeneidad (Levene):        F=%.4f  p=%.4f %-3s  [%s]\n",
                levene$`F value`[1], levene$`Pr(>F)`[1], hom_sig, hom_res))
  }

  # Outliers
  n_out <- if (!is.null(outliers)) length(outliers) else 0
  cat(sprintf(" Valores atipicos (|Rst|>3):   n=%d  [%s]\n",
              n_out, if (n_out == 0) "Sin atipicos" else "Revisar"))

  # CV
  if (!is.na(cv)) {
    cat(sprintf(" CV experimental:              %.2f%%\n", cv))
  }

  cat("----------------------------------------------------------------\n")
  cat(" Sig: *** p<0.001  ** p<0.01  * p<0.05  ns p>=0.05\n")
  cat("================================================================\n\n")

  return(invisible(list(
    shapiro  = shapiro,
    bartlett = bartlett,
    levene   = levene,
    outliers = outliers,
    balance  = tabla,
    cv       = cv
  )))
}
