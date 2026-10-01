# Las referencias se calculan con stats::aov() para que InkAgro se compare
# contra el procedimiento estándar de R, y para parcelas divididas además
# contra los valores publicados por Yates (1935) para los datos de avena.

fila <- function(res, termino) res$anova[res$anova$termino == termino, ]

test_that("DCA reproduce el ANOVA de aov()", {
  res <- inkagro(PlantGrowth, "weight", "group", diseno = "dca")
  ref <- summary(stats::aov(weight ~ group, PlantGrowth))[[1]]
  expect_s3_class(res, "inkagro")
  expect_equal(fila(res, "A")$F, ref[["F value"]][1])
  expect_equal(fila(res, "A")$p, ref[["Pr(>F)"]][1])
  expect_equal(fila(res, "Residuals")$gl, 27)
  expect_equal(res$tipo_sc, "I")
  expect_named(res$medias, "group")
})

test_that("el CV usa el cuadrado medio del error", {
  res <- inkagro(PlantGrowth, "weight", "group", diseno = "dca")
  cme <- fila(res, "Residuals")$cm
  expect_equal(unname(res$cv), 100 * sqrt(cme) / mean(PlantGrowth$weight))
})

test_that("DBCA reproduce aov() con bloques", {
  res <- inkagro(npk, "yield", "N", bloque = "block", diseno = "dbca")
  ref <- summary(stats::aov(yield ~ block + N, npk))[[1]]
  expect_equal(fila(res, "A")$F, ref[["F value"]][2])
  expect_equal(fila(res, "blq")$gl, 5)
  # P explica las dos parcelas de cada N por bloque: debe avisar, no detenerse
  expect_match(res$avisos, "'P'", all = FALSE)
})

test_that("factorial en bloques reproduce aov()", {
  res <- inkagro(npk, "yield", "N", factor_b = "P", bloque = "block",
                 diseno = "factorial")
  ref <- summary(stats::aov(yield ~ block + N * P, npk))[[1]]
  expect_equal(fila(res, "A")$F, ref[["F value"]][2])
  expect_equal(fila(res, "B")$F, ref[["F value"]][3])
  expect_equal(fila(res, "A:B")$p, ref[["Pr(>F)"]][4])
  expect_named(res$medias, c("N", "P"))
})

test_that("factorial en DCA (sin bloques)", {
  res <- inkagro(warpbreaks, "breaks", "wool", factor_b = "tension")
  ref <- summary(stats::aov(breaks ~ wool * tension, warpbreaks))[[1]]
  expect_equal(fila(res, "A:B")$F, ref[["F value"]][3])
  expect_match(res$avisos, "No indicaste 'diseno'", all = FALSE)
  # interaccion significativa: debe comparar combinaciones
  expect_true("wool x tension" %in% names(res$medias))
})

test_that("parcelas divididas usa el error (a) y coincide con Yates (1935)", {
  skip_if_not_installed("nlme")
  avena <- as.data.frame(nlme::Oats)
  res <- inkagro(avena, "yield", "Variety", factor_b = "nitro",
                 bloque = "Block", diseno = "parcelas_divididas")
  ref <- summary(stats::aov(yield ~ Variety * factor(nitro) + Error(Block / Variety),
                            avena))
  f_var <- ref[["Error: Block:Variety"]][[1]][["F value"]][1]
  f_n   <- ref[["Error: Within"]][[1]][["F value"]][1]
  expect_equal(fila(res, "A")$F, f_var)
  expect_equal(fila(res, "B")$F, f_n)
  # Valores publicados
  expect_equal(round(fila(res, "A")$F, 3), 1.485)
  expect_equal(round(fila(res, "B")$F, 2), 37.69)
  expect_equal(fila(res, "blq:A")$gl, 10)
  expect_equal(fila(res, "Residuals")$gl, 45)
  expect_named(res$cv, c("a", "b"))
})

test_that("se detiene si el bloque no cruza con los tratamientos", {
  d <- data.frame(t = rep(c("a", "b", "c"), each = 3),
                  b = rep(c("1", "2", "3"), each = 3),
                  y = c(1, 2, 3, 4, 5, 6, 7, 8, 9))
  expect_error(inkagro(d, "y", "t", bloque = "b", diseno = "dbca"),
               "no funciona como bloque")
})

test_that("se detiene con un ensayo multiambiental y nombra la columna", {
  set.seed(1)
  d <- expand.grid(gen = paste0("G", 1:4), rep = c("R1", "R2", "R3"),
                   localidad = c("Chachapoyas", "Rioja"))
  d$rend <- rnorm(nrow(d), 5)
  expect_error(inkagro(d, "rend", "gen", bloque = "rep", diseno = "dbca"),
               "localidad")
})

test_that("detecta ambientes tambien sin bloques", {
  d <- expand.grid(gen = paste0("G", 1:5), year = c("2024", "2025", "2026"))
  d$rend <- seq_len(nrow(d)) %% 7
  expect_error(inkagro(d, "rend", "gen"), "year")
})

test_that("se detiene con un factorial incompleto", {
  d <- warpbreaks[!(warpbreaks$wool == "A" & warpbreaks$tension == "H"), ]
  expect_error(inkagro(d, "breaks", "wool", factor_b = "tension",
                       diseno = "factorial"), "Factorial incompleto")
})

test_that("con bloque y factor_b sin diseno pide decidir", {
  expect_error(inkagro(npk, "yield", "N", factor_b = "P", bloque = "block"),
               "parcelas_divididas")
})

test_that("sugiere columnas cuando el nombre no existe", {
  expect_error(inkagro(PlantGrowth, "wieght", "group"), "weight")
  res <- inkagro(PlantGrowth, "WEIGHT", "Group", diseno = "dca")
  expect_equal(res$nombres$respuesta, "weight")
})

test_that("avisa si hay una columna de bloques ignorada en un DCA", {
  res <- inkagro(npk, "yield", "N", diseno = "dca")
  expect_match(res$avisos, "parece de bloques", all = FALSE)
})

test_that("los atipicos se marcan por defecto y se excluyen solo si se pide", {
  d <- PlantGrowth
  d$weight[5] <- 30
  marcado <- inkagro(d, "weight", "group", diseno = "dca")
  expect_equal(marcado$atipicos$fila, 5)
  expect_equal(marcado$n, 30)
  excluido <- inkagro(d, "weight", "group", diseno = "dca", outliers = "excluir")
  expect_equal(excluido$n, 29)
  expect_match(excluido$avisos, "Se excluy\u00f3 1 valor at\u00edpico", all = FALSE)
})

test_that("filas con faltantes se excluyen y se reportan", {
  d <- PlantGrowth
  d$weight[c(2, 15)] <- NA
  res <- inkagro(d, "weight", "group", diseno = "dca")
  expect_equal(res$n, 28)
  expect_match(res$avisos, "Se excluyeron 2 filas", all = FALSE)
  expect_match(res$avisos, "filas 2, 15", all = FALSE)
})

test_that("lee archivos csv con punto y coma y coma decimal", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  d <- PlantGrowth
  utils::write.csv2(d, f, row.names = FALSE)
  res <- inkagro(f, "weight", "group", diseno = "dca")
  expect_equal(res$n, 30)
})

test_that("print, as.data.frame y plot funcionan", {
  res <- inkagro(npk, "yield", "N", factor_b = "P", bloque = "block",
                 diseno = "factorial")
  expect_output(print(res), "Comparaci")
  expect_s3_class(as.data.frame(res), "data.frame")
  expect_true(all(c("comparacion", "grupo") %in% names(as.data.frame(res, tabla = "medias"))))
  expect_s3_class(plot(res), "ggplot")
  expect_s3_class(plot(res, tipo = "cajas", comparacion = "P"), "ggplot")
})

test_that("unifica etiquetas que difieren en mayusculas, espacios o coma decimal", {
  d <- PlantGrowth
  d$group <- as.character(d$group)
  d$group[d$group == "trt1"][1:3] <- c("TRT1", " trt1", "Trt1 ")
  d$dosis <- rep(c("0.2", "0,2", "0.20"), 10)
  res <- inkagro(d, "weight", "group", diseno = "dca")
  expect_equal(nlevels(res$datos$A), 3)
  expect_match(res$avisos, "se unieron etiquetas", all = FALSE)
  f <- InkAgro:::.ink_factor(d$dosis)
  expect_equal(levels(f), "0.2")
})

test_that("informe() crea un Word y plot() hace el grafico de interaccion", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  res <- inkagro(npk, "yield", "N", factor_b = "P", bloque = "block",
                 diseno = "factorial")
  f <- tempfile(fileext = ".docx")
  on.exit(unlink(f))
  informe(res, f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 10000)
  expect_s3_class(plot(res, tipo = "interaccion"), "ggplot")
  expect_s3_class(plot(res, estilo = "color"), "ggplot")
  dca <- inkagro(PlantGrowth, "weight", "group", diseno = "dca")
  expect_error(plot(dca, tipo = "interaccion"), "dos factores")
})

test_that("respeta el orden de niveles de un factor y muchos niveles van horizontales", {
  res <- inkagro(warpbreaks, "breaks", "tension", factor_b = "wool",
                 diseno = "factorial")
  expect_equal(levels(res$datos$A), c("L", "M", "H"))
  set.seed(2)
  d <- data.frame(gen = rep(sprintf("G%02d", 1:20), 3), rep = rep(1:3, each = 20))
  d$y <- rnorm(60, 10)
  g <- plot(inkagro(d, "y", "gen", bloque = "rep", diseno = "dbca"))
  expect_match(deparse(g$mapping$y), "nivel")
})

test_that("la regresion polinomial coincide con aov() y con el error correcto", {
  skip_if_not_installed("nlme")
  o <- as.data.frame(nlme::Oats)
  res <- inkagro(o, "yield", "Variety", factor_b = "nitro", bloque = "Block",
                 diseno = "parcelas_divididas")
  r <- res$regresion$nitro
  o$N <- factor(o$nitro)
  contrasts(o$N) <- contr.poly(4, scores = c(0, 0.2, 0.4, 0.6))
  ref <- summary(stats::aov(yield ~ Variety * N + Error(Block / Variety), o),
                 split = list(N = list(L = 1, Q = 2, C = 3)))[["Error: Within"]][[1]]
  expect_equal(r$tabla$F[1:3], unname(ref[["F value"]][2:4]))
  expect_equal(r$grado, 1L)
  expect_named(res$regresion, "nitro")       # Variety no es cuantitativo
  expect_output(print(res), "Respuesta lineal")
})

test_that("detecta el maximo de una respuesta cuadratica", {
  d <- expand.grid(dosis = c(0, 50, 100, 150, 200), rep = 1:4)
  set.seed(10)
  d$y <- 10 + 0.2 * d$dosis - 0.001 * d$dosis^2 + rnorm(nrow(d), sd = 0.5)
  res <- inkagro(d, "y", "dosis", bloque = "rep", diseno = "dbca")
  r <- res$regresion$dosis
  expect_equal(r$grado, 2L)
  expect_true(abs(r$optimo[["x"]] - 100) < 15)
})

test_that("graficos nuevos y menu fuera de sesion interactiva", {
  res <- inkagro(npk, "yield", "N", factor_b = "P", bloque = "block",
                 diseno = "factorial")
  expect_s3_class(plot(res, tipo = "puntos"), "ggplot")
  expect_s3_class(plot(res, tipo = "residuos"), "ggplot")
  expect_error(plot(res, tipo = "regresion"), "cuantitativo")
  expect_error(graficos(res), "interactiva")
})

test_that("si el archivo no existe, sugiere usar la ruta completa", {
  expect_error(inkagro("no_existe_123.xlsx", "y", "t"), "file.choose")
})
