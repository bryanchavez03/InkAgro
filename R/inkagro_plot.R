inkagro_plot <- function(resultado,
                         datos       = NULL,
                         respuesta   = NULL,
                         tratamiento = NULL,
                         color_barras = "#2E75B6",
                         color_relleno = "#BDD7EE",
                         tema = "classic",
                         exportar = FALSE,
                         resolucion = 300,
                         ancho = 8,
                         alto = 6) {

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
      "No se encontraron datos, respuesta o tratamiento en el resultado. ",
      "Llame primero inkagro_auto() o especifique los argumentos manualmente."
    )
  }

  # Eliminar NAs en respuesta antes de graficar
  datos_plot <- datos[!is.na(datos[[respuesta]]), ]

  # CV para subtitulo
  cv_val <- tryCatch({
    residuos   <- as.numeric(stats::residuals(modelo))
    media_gral <- mean(datos_plot[[respuesta]], na.rm = TRUE)
    rmse       <- sqrt(mean(residuos^2))
    round((rmse / media_gral) * 100, 2)
  }, error = function(e) NA)

  subtitulo_cv <- if (!is.na(cv_val)) {
    paste0("Diseno: ", diseno, "  |  CV = ", cv_val, "%")
  } else {
    paste0("Diseno: ", diseno)
  }

  graficos <- list()

  # Tema dinamico segun parametro
  tema_base <- switch(tema,
    "classic" = ggplot2::theme_classic(base_size = 13),
    "bw" = ggplot2::theme_bw(base_size = 13),
    "minimal" = ggplot2::theme_minimal(base_size = 13),
    ggplo2::theme_classic(base_size = 13))

  tema_pub <- tema_base +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1,
                                               color = "black"),
      axis.text.y      = ggplot2::element_text(color = "black"),
      axis.title       = ggplot2::element_text(face = "bold", size = 12),
      plot.title       = ggplot2::element_text(face = "bold", size = 13,
                                               hjust = 0),
      plot.subtitle    = ggplot2::element_text(size = 10, color = "gray40",
                                               hjust = 0),
      panel.border     = ggplot2::element_rect(color = "black",
                                               fill = NA, linewidth = 0.6),
      axis.line        = ggplot2::element_blank()
    )

  # 1. Boxplot
  cat("Generando figura 1: Boxplot por tratamiento...\n")

  p_boxplot <- ggplot2::ggplot(
    datos_plot,
    ggplot2::aes(
      x = stats::reorder(as.character(.data[[tratamiento]]),
                         .data[[respuesta]], FUN = stats::median),
      y = .data[[respuesta]]
    )
  ) +
    ggplot2::geom_boxplot(fill = color_relleno , color = color_barras,
                          outlier.shape = 21, outlier.fill = "white",
                          outlier.color = color_barras, linewidth = 0.6) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.4, size = 1.2,
                         color = color_barras) +
    ggplot2::labs(
      title    = paste0("Distribucion de ", toupper(respuesta),
                        " por tratamiento"),
      subtitle = subtitulo_cv,
      x        = toupper(tratamiento),
      y        = toupper(respuesta)
    ) +
    tema_pub

  graficos$boxplot <- p_boxplot
  print(p_boxplot)

  # 2. Barras de medias con letras Tukey
  cat("Generando figura 2: Medias con comparacion Tukey HSD...\n")

  tukey_df <- NULL

  tukey_src <- if (!is.null(resultado$tukey))      resultado$tukey      else
    if (!is.null(resultado$tukey_trat)) resultado$tukey_trat else
      if (!is.null(resultado$tukey_a))    resultado$tukey_a    else NULL

  if (!is.null(tukey_src)) {
    tukey_df                <- tukey_src$groups
    tukey_df[[tratamiento]] <- rownames(tukey_df)
    names(tukey_df)[1]      <- "media"
    rownames(tukey_df)      <- NULL
  } else if (!is.null(resultado$emmeans)) {
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

  if (!is.null(tukey_df) &&
      all(c("media", "groups", tratamiento) %in% names(tukey_df))) {

    se_df <- tryCatch({
      agg <- stats::aggregate(
        datos_plot[[respuesta]],
        by  = list(trat = as.character(datos_plot[[tratamiento]])),
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
      ggplot2::geom_col(fill = color_barras, color = "black",
                        linewidth = 0.4, width = 0.65) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = media - se, ymax = media + se),
        width = 0.2, color = "black", linewidth = 0.6
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = groups, y = media + se),
        vjust = -0.6, size = 4.2, fontface = "bold", color = "black"
      ) +
      ggplot2::labs(
        title    = paste0("Medias de ", toupper(respuesta),
                          " por tratamiento (Tukey HSD, alfa=0.05)"),
        subtitle = subtitulo_cv,
        x        = toupper(tratamiento),
        y        = paste0(toupper(respuesta), " (Media \u00b1 EE)")
      ) +
      tema_pub

    graficos$barras <- p_barras
    print(p_barras)
  }

  # 3. QQ-plot
  cat("Generando figura 3: QQ-plot de residuos...\n")

  residuos <- tryCatch(as.numeric(stats::residuals(modelo)),
                       error = function(e) NULL)

  if (!is.null(residuos) && length(residuos) > 2) {

    p_qq <- ggplot2::ggplot(
      data.frame(residuos = residuos),
      ggplot2::aes(sample = residuos)
    ) +
      ggplot2::stat_qq(color = color_barras, size = 1.3, alpha = 0.7) +
      ggplot2::stat_qq_line(color = "red", linewidth = 0.8,
                            linetype = "dashed") +
      ggplot2::labs(
        title    = "Grafico de probabilidad normal de residuos",
        subtitle = "Verificacion del supuesto de normalidad",
        x        = "Cuantiles teoricos",
        y        = "Cuantiles observados"
      ) +
      tema_pub

    graficos$qq <- p_qq
    print(p_qq)

    # 4. Residuos vs ajustados
    cat("Generando figura 4: Residuos vs valores ajustados...\n")

    ajustados <- tryCatch(as.numeric(stats::fitted(modelo)),
                          error = function(e) NULL)

    if (!is.null(ajustados) && length(ajustados) == length(residuos)) {

      p_residuos <- ggplot2::ggplot(
        data.frame(ajustados = ajustados, residuos = residuos),
        ggplot2::aes(x = ajustados, y = residuos)
      ) +
        ggplot2::geom_point(color = color_barras, alpha = 0.6,
                            size = 1.5) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            color = "black", linewidth = 0.7) +
        ggplot2::labs(
          title    = "Residuos vs valores ajustados",
          subtitle = "Verificacion del supuesto de homogeneidad de varianzas",
          x        = "Valores ajustados",
          y        = "Residuos"
        ) +
        tema_pub

      graficos$residuos <- p_residuos
      print(p_residuos)
    }
  }

  # 5. Grafico de interaccion (Factorial)
  if (diseno == "Factorial" && !is.null(fa) && !is.null(resultado$emmeans)) {
    em_df <- tryCatch(as.data.frame(resultado$emmeans$emmeans),
                      error = function(e) NULL)

    if (!is.null(em_df) &&
        all(c(tratamiento, fa, "emmean", "lower.CL", "upper.CL") %in% names(em_df))) {

      cat("Generando figura 5: Grafico de interaccion...\n")

      p_interaccion <- ggplot2::ggplot(
        em_df,
        ggplot2::aes(
          x     = as.character(.data[[tratamiento]]),
          y     = emmean,
          color = as.character(.data[[fa]]),
          group = as.character(.data[[fa]])
        )
      ) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = lower.CL, ymax = upper.CL),
          width = 0.15, linewidth = 0.6
        ) +
        ggplot2::labs(
          title    = paste0("Interaccion ", toupper(tratamiento),
                            " \u00d7 ", toupper(fa)),
          subtitle = "Medias estimadas con intervalos de confianza (95%)",
          x        = toupper(tratamiento),
          y        = paste0("Media estimada - ", toupper(respuesta)),
          color    = toupper(fa)
        ) +
        tema_pub

      graficos$interaccion <- p_interaccion
      print(p_interaccion)
    }
  }

  # 6. Medias por ambiente
  if (diseno %in% c("Factorial con Ambientes", "Multiambiental (MET)") &&
      !is.null(env) && !is.null(resultado$emmeans)) {

    em_df <- tryCatch(as.data.frame(resultado$emmeans$emmeans),
                      error = function(e) NULL)

    if (!is.null(em_df) &&
        all(c(tratamiento, env, "emmean") %in% names(em_df))) {

      cat("Generando figura 5: Medias por ambiente...\n")

      p_ambiente <- ggplot2::ggplot(
        em_df,
        ggplot2::aes(
          x     = as.character(.data[[tratamiento]]),
          y     = emmean,
          color = as.character(.data[[env]]),
          group = as.character(.data[[env]])
        )
      ) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::labs(
          title    = paste0("Medias de ", toupper(respuesta),
                            " por ambiente"),
          subtitle = paste0("Diseno: ", diseno),
          x        = toupper(tratamiento),
          y        = paste0("Media estimada - ", toupper(respuesta)),
          color    = toupper(env)
        ) +
        tema_pub

      graficos$ambiente <- p_ambiente
      print(p_ambiente)
    }
  }
# Exportar figuras si el usuario lo solicita
  if (exportar) {
    cat("\nExportando figuras... \n")
    for (nombre in names (graficos)) {
      archivo <- paste0("inkagro_",nombre, "png")
      ggplot2::ggsave(
        filename = archivo,
        plot = graficos[[nombre]],
        dpi = resolucion,
        width = ancho,
        height = alto,
        units = "in"
      )
      cat(sprintf("  Guardada: %s (%d dpi, %dx%d in)\n,
                  archivo, resoluciom,ancho,alto"))
    }
  }
# Resumen final
  n_g <- length(graficos)
  cat(sprintf("\n%d figura(s) generada(s): %s\n",
              n_g, paste(names(graficos), collapse = ", ")))
  cat("----------------------------------------------------------------\n")
  cat("Para personalizar colores:\n")
  cat("  inkagro_plot(resultado, color_barras='#C00000', color_relleno='#FFE0E0')\n")
  cat("Para cambiar tema:\n")
  cat("  inkagro_plot(resultado, tema='bw')   # opciones: classic, bw, minimal\n")
  cat("Para exportar a 300 dpi:\n")
  cat("  inkagro_plot(resultado, exportar=TRUE, resolucion=300)\n")
  cat("Para editar manualmente:\n")
  cat("  resultado$graficos$barras + ggplot2::theme_bw()\n\n")

  return(invisible(graficos))
}
