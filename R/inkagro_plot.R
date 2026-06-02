inkagro_plot <- function(resultado,
                         datos       = NULL,
                         respuesta   = NULL,
                         tratamiento = NULL) {

  # 1. Extraer metadatos del resultado o de los parametros
  if (is.null(datos))       datos       <- resultado$datos
  if (is.null(respuesta))   respuesta   <- resultado$respuesta
  if (is.null(tratamiento)) tratamiento <- resultado$tratamiento

  fa     <- resultado$fa
  fb     <- resultado$fb
  env    <- resultado$env
  diseno <- resultado$diseno
  modelo <- resultado$modelo

  if (is.null(datos) || is.null(respuesta) || is.null(tratamiento)) {
    stop(
      "No se encontraron datos/respuesta/tratamiento en el resultado. ",
      "Llame primero inkagro_auto() o pase estos argumentos manualmente: ",
      "inkagro_plot(resultado, datos=df, respuesta='y', tratamiento='trat')"
    )
  }

  graficos <- list()
  cli::cli_h1("InkAgro — Graficos")

  # ── 1. Boxplot por tratamiento ────────────────────────────────────────────
  cli::cli_h3("1. Boxplot por tratamiento")

  p_boxplot <- ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x = stats::reorder(as.character(.data[[tratamiento]]),
                         .data[[respuesta]], FUN = stats::median),
      y = .data[[respuesta]]
    )
  ) +
    ggplot2::geom_boxplot(fill = "#BDD7EE", color = "#2E75B6",
                          outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.5, size = 1.5,
                         color = "#1F4E79") +
    ggplot2::labs(
      title    = paste("Distribucion de", respuesta, "por tratamiento"),
      subtitle = paste("Diseno:", diseno),
      x        = tratamiento,
      y        = respuesta
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

  graficos$boxplot <- p_boxplot
  print(p_boxplot)

  # ── 2. Barras de medias con letras Tukey ──────────────────────────────────
  cli::cli_h3("2. Medias con letras Tukey HSD")

  tukey_df <- NULL

  # Caso A: resultado de HSD.test (DCA, DBCA, Cuadrado Latino)
  tukey_src <- if (!is.null(resultado$tukey))      resultado$tukey      else
               if (!is.null(resultado$tukey_trat)) resultado$tukey_trat else NULL

  if (!is.null(tukey_src)) {
    tukey_df                <- tukey_src$groups
    tukey_df[[tratamiento]] <- rownames(tukey_df)
    names(tukey_df)[1]      <- "media"
    rownames(tukey_df)      <- NULL
  } else if (!is.null(resultado$emmeans)) {
    # Caso B: resultado de emmeans (Factorial, MET, etc.)
    em_obj  <- resultado$emmeans$emmeans
    cld_obj <- tryCatch(
      as.data.frame(multcomp::cld(em_obj, Letters = letters,
                                  adjust = "tukey", quiet = TRUE)),
      error = function(e) NULL
    )
    if (!is.null(cld_obj) && ".group" %in% names(cld_obj)) {
      cld_obj$media  <- cld_obj$emmean
      cld_obj$groups <- trimws(cld_obj$.group)
      tukey_df <- cld_obj
    }
  }

  if (!is.null(tukey_df) && all(c("media", "groups", tratamiento) %in% names(tukey_df))) {

    # SE por tratamiento desde los datos
    se_df <- tryCatch({
      agg <- stats::aggregate(
        datos[[respuesta]],
        by  = list(trat = as.character(datos[[tratamiento]])),
        FUN = function(x) stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
      )
      names(agg) <- c(tratamiento, "se")
      agg
    }, error = function(e) NULL)

    tukey_df[[tratamiento]] <- as.character(tukey_df[[tratamiento]])
    if (!is.null(se_df)) {
      se_df[[tratamiento]] <- as.character(se_df[[tratamiento]])
      tukey_df <- merge(tukey_df, se_df, by = tratamiento, all.x = TRUE)
    } else {
      tukey_df$se <- 0
    }
    tukey_df$se[is.na(tukey_df$se)] <- 0

    p_barras <- ggplot2::ggplot(
      tukey_df,
      ggplot2::aes(
        x = stats::reorder(as.character(.data[[tratamiento]]),
                           .data[["media"]]),
        y = .data[["media"]]
      )
    ) +
      ggplot2::geom_col(fill = "#2E75B6", color = "white", width = 0.7) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = media - se, ymax = media + se),
        width = 0.25, color = "gray30", linewidth = 0.7
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = groups, y = media + se),
        vjust = -0.5, size = 4.5, fontface = "bold"
      ) +
      ggplot2::labs(
        title    = paste("Medias de", respuesta, "\u2014 Tukey HSD"),
        subtitle = paste("Diseno:", diseno),
        x        = tratamiento,
        y        = paste("Media \u00b1 SE \u2014", respuesta)
      ) +
      ggplot2::theme_classic(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

    graficos$barras <- p_barras
    print(p_barras)
  }

  # ── 3. QQ-plot de residuos ─────────────────────────────────────────────────
  cli::cli_h3("3. QQ-plot de residuos")

  residuos <- tryCatch(as.numeric(stats::residuals(modelo)),
                       error = function(e) NULL)

  if (!is.null(residuos) && length(residuos) > 2) {

    p_qq <- ggplot2::ggplot(data.frame(residuos = residuos),
                            ggplot2::aes(sample = residuos)) +
      ggplot2::stat_qq(color = "#2E75B6", size = 1.5) +
      ggplot2::stat_qq_line(color = "red", linewidth = 0.9) +
      ggplot2::labs(
        title    = "QQ-plot de residuos",
        subtitle = "Supuesto de normalidad",
        x        = "Cuantiles teoricos",
        y        = "Cuantiles muestrales"
      ) +
      ggplot2::theme_classic(base_size = 12)

    graficos$qq <- p_qq
    print(p_qq)

    # ── 4. Residuos vs Ajustados ───────────────────────────────────────────
    cli::cli_h3("4. Residuos vs Valores ajustados")

    ajustados <- tryCatch(as.numeric(stats::fitted(modelo)),
                          error = function(e) NULL)

    if (!is.null(ajustados) && length(ajustados) == length(residuos)) {

      p_residuos <- ggplot2::ggplot(
        data.frame(ajustados = ajustados, residuos = residuos),
        ggplot2::aes(x = ajustados, y = residuos)
      ) +
        ggplot2::geom_point(color = "#2E75B6", alpha = 0.7, size = 1.8) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            color = "red", linewidth = 0.9) +
        ggplot2::labs(
          title    = "Residuos vs Valores ajustados",
          subtitle = "Supuesto de homogeneidad de varianzas",
          x        = "Valores ajustados",
          y        = "Residuos"
        ) +
        ggplot2::theme_classic(base_size = 12)

      graficos$residuos <- p_residuos
      print(p_residuos)
    }
  }

  # ── 5. Grafico de interaccion (Factorial con interaccion significativa) ────
  if (diseno == "Factorial" && !is.null(fa) && !is.null(resultado$emmeans)) {
    em_df <- tryCatch(as.data.frame(resultado$emmeans$emmeans),
                      error = function(e) NULL)

    if (!is.null(em_df) &&
        all(c(tratamiento, fa, "emmean", "lower.CL", "upper.CL") %in% names(em_df))) {

      cli::cli_h3("5. Grafico de interaccion")

      p_interaccion <- ggplot2::ggplot(
        em_df,
        ggplot2::aes(
          x     = as.character(.data[[tratamiento]]),
          y     = emmean,
          color = as.character(.data[[fa]]),
          group = as.character(.data[[fa]])
        )
      ) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = lower.CL, ymax = upper.CL),
          width = 0.2, linewidth = 0.7
        ) +
        ggplot2::labs(
          title    = "Grafico de interaccion",
          subtitle = paste(tratamiento, "\u00d7", fa),
          x        = tratamiento,
          y        = paste("Media estimada \u2014", respuesta),
          color    = fa
        ) +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

      graficos$interaccion <- p_interaccion
      print(p_interaccion)
    }
  }

  # ── 6. Medias por ambiente (MET / Factorial con Ambientes) ────────────────
  if (diseno %in% c("Factorial con Ambientes", "Multiambiental (MET)") &&
      !is.null(env) && !is.null(resultado$emmeans)) {

    em_df <- tryCatch(as.data.frame(resultado$emmeans$emmeans),
                      error = function(e) NULL)

    if (!is.null(em_df) &&
        all(c(tratamiento, env, "emmean") %in% names(em_df))) {

      cli::cli_h3("5. Medias por ambiente")

      p_ambiente <- ggplot2::ggplot(
        em_df,
        ggplot2::aes(
          x     = as.character(.data[[tratamiento]]),
          y     = emmean,
          color = as.character(.data[[env]]),
          group = as.character(.data[[env]])
        )
      ) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::labs(
          title    = "Medias por ambiente",
          subtitle = paste("Diseno:", diseno),
          x        = tratamiento,
          y        = paste("Media estimada \u2014", respuesta),
          color    = env
        ) +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

      graficos$ambiente <- p_ambiente
      print(p_ambiente)
    }
  }

  n_g <- length(graficos)
  cli::cli_alert_success(
    "{n_g} grafico(s) generado(s): {paste(names(graficos), collapse = ', ')}")
  cli::cli_alert_info(
    "Personaliza: resultado$graficos$barras + ggplot2::theme_classic()")

  return(invisible(graficos))
}
