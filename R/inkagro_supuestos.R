inkagro_supuestos <- function(modelo, datos, tratamiento) {

  cli::cli_h2("Verificacion de Supuestos")

  es_lmer <- inherits(modelo, "lmerMod")

  if (es_lmer) {
    cli::cli_alert_info("Modelo mixto (lmer) detectado")
    cli::cli_alert_info("Para verificacion completa use DHARMa")
    residuos <- residuals(modelo)
    shapiro  <- shapiro.test(residuos)
    if (shapiro$p.value >= 0.05) {
      cli::cli_alert_success("Normalidad aceptada: W={round(shapiro$statistic,4)} p={round(shapiro$p.value,4)}")
    } else {
      cli::cli_alert_danger("Normalidad rechazada: W={round(shapiro$statistic,4)} p={round(shapiro$p.value,4)}")
    }
    return(invisible(list(shapiro = shapiro)))
  }

  residuos <- residuals(modelo)

  # 1. Normalidad
  cli::cli_h3("1. Normalidad de residuos (Shapiro-Wilk)")
  shapiro <- NULL
  if (length(residuos) > 200) {
    cli::cli_alert_warning("Muestra grande (n>{length(residuos)}) — Shapiro-Wilk muy sensible")
    cli::cli_alert_info("Interprete el QQ-plot visualmente")
  } else {
    shapiro <- shapiro.test(residuos)
    if (shapiro$p.value >= 0.05) {
      cli::cli_alert_success("Normalidad aceptada: W={round(shapiro$statistic,4)} | p={round(shapiro$p.value,4)}")
    } else {
      cli::cli_alert_danger("Normalidad rechazada: W={round(shapiro$statistic,4)} | p={round(shapiro$p.value,4)}")
      cli::cli_alert_info("Considere transformacion logaritmica o prueba no parametrica")
    }
  }

  # 2. Homogeneidad
  cli::cli_h3("2. Homogeneidad de varianzas")
  datos_modelo <- tryCatch(model.frame(modelo), error = function(e) datos)
  trat_factor  <- droplevels(as.factor(datos_modelo[[tratamiento]]))
  res_clean    <- residuos

  bartlett <- NULL
  bartlett <- tryCatch({
    bt <- bartlett.test(res_clean ~ trat_factor)
    bt
  }, error = function(e) NULL)

  levene <- NULL
  levene <- tryCatch({
    car::leveneTest(res_clean ~ trat_factor)
  }, error = function(e) NULL)

  if (!is.null(bartlett) && !is.null(levene)) {
    p_bart <- round(bartlett$p.value, 4)
    p_lev  <- round(levene$`Pr(>F)`[1], 4)
    k2     <- round(bartlett$statistic, 4)
    f_lev  <- round(levene$`F value`[1], 4)

    bart_ok <- bartlett$p.value >= 0.05
    lev_ok  <- levene$`Pr(>F)`[1] >= 0.05

    if (bart_ok && lev_ok) {
      cli::cli_alert_success("Bartlett: K2={k2} p={p_bart} ✔ | Levene: F={f_lev} p={p_lev} ✔")
      cli::cli_alert_success("Varianzas homogeneas")
    } else if (!bart_ok && lev_ok) {
      cli::cli_alert_warning("Bartlett: K2={k2} p={p_bart} ✖ | Levene: F={f_lev} p={p_lev} ✔")
      cli::cli_alert_info("Resultados discordantes — preferir Levene (mas robusto a no-normalidad)")
    } else if (bart_ok && !lev_ok) {
      cli::cli_alert_warning("Bartlett: K2={k2} p={p_bart} ✔ | Levene: F={f_lev} p={p_lev} ✖")
      cli::cli_alert_info("Resultados discordantes — preferir Levene")
    } else {
      cli::cli_alert_danger("Bartlett: K2={k2} p={p_bart} ✖ | Levene: F={f_lev} p={p_lev} ✖")
      cli::cli_alert_danger("Varianzas heterogeneas — considere transformacion o Welch ANOVA")
    }
  } else {
    cli::cli_alert_warning("No se pudo calcular homogeneidad")
  }

  # 3. Outliers
  cli::cli_h3("3. Deteccion de outliers")
  res_std  <- tryCatch(rstandard(modelo), error = function(e) NULL)
  outliers <- NULL
  if (!is.null(res_std)) {
    outliers <- which(abs(res_std) > 3)
    if (length(outliers) == 0) {
      cli::cli_alert_success("Sin outliers extremos detectados")
    } else {
      cli::cli_alert_danger("{length(outliers)} outlier(s) en observaciones: {paste(outliers, collapse=', ')}")
    }
  }

  # 4. Balance
  cli::cli_h3("4. Balance del diseno")
  tabla <- table(datos[[tratamiento]])
  if (length(unique(tabla)) == 1) {
    cli::cli_alert_success("Diseno balanceado — {unique(tabla)} obs por tratamiento")
  } else {
    cli::cli_alert_warning("Diseno desbalanceado")
    print(tabla)
  }

  return(invisible(list(
    shapiro  = shapiro,
    bartlett = bartlett,
    levene   = levene,
    outliers = outliers,
    balance  = tabla
  )))
}
