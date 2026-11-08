# Con parcelas perdidas, las medias se ajustan con el modelo. Valores de
# referencia calculados con emmeans::emmeans() sobre el mismo modelo.

batata_incompleta <- function() {
  data(sweetpotato, package = "agricolae", envir = environment())
  d <- data.frame(blq = rep(1:3, 4), virus = rep(c("cc", "fc", "ff", "oo"), each = 3),
                  y = sweetpotato$yield)
  d$y[c(1, 8)] <- NA
  d
}

test_that("DBCA con parcelas perdidas: medias ajustadas iguales a emmeans", {
  res <- inkagro(batata_incompleta(), "y", "virus", bloque = "blq", diseno = "dbca")
  m <- res$medias$virus
  m <- m[order(m$nivel), ]
  expect_equal(m$media, c(24.08762, 12.86667, 34.70762, 36.90000), tolerance = 1e-6)
  expect_equal(m$ee, c(3.215530, 2.476627, 3.215530, 2.476627), tolerance = 1e-6)
  expect_true(isTRUE(attr(res$medias$virus, "ajustadas")))
  # Tukey (emmeans): solo fc difiere de ff y de oo
  expect_equal(setNames(m$grupo, m$nivel), c(cc = "ab", fc = "b", ff = "a", oo = "a"))
  expect_match(res$avisos, "mínimos cuadrados", all = FALSE)
})

test_that("LSD con parcelas perdidas usa las mismas medias ajustadas", {
  res <- inkagro(batata_incompleta(), "y", "virus", bloque = "blq", diseno = "dbca",
                 prueba = "lsd")
  m <- res$medias$virus
  expect_equal(setNames(m$grupo, m$nivel), c(oo = "a", ff = "ab", cc = "bc", fc = "c"))
})

test_that("factorial desbalanceado: medias de cada factor promedian el otro", {
  r <- inkagro(npk[-1, ], "yield", "N", bloque = "block", factor_b = "P",
               diseno = "factorial")
  m <- r$medias$N
  expect_equal(m$media[m$nivel == "0"], 52.34222, tolerance = 1e-6)
  expect_equal(m$ee[m$nivel == "0"], 1.439744, tolerance = 1e-6)
  expect_equal(m$media[m$nivel == "1"], 57.68333, tolerance = 1e-6)
})

test_that("Duncan con parcelas perdidas avisa que las medias son aritmeticas", {
  res <- inkagro(batata_incompleta(), "y", "virus", bloque = "blq", diseno = "dbca",
                 prueba = "duncan")
  expect_null(attr(res$medias$virus, "ajustadas"))
  expect_match(res$avisos, "aritméticas", all = FALSE)
})

test_that("con datos completos no cambia nada", {
  res <- inkagro(npk, "yield", "N", bloque = "block", factor_b = "P", diseno = "factorial")
  expect_null(attr(res$medias$N, "ajustadas"))
  expect_false(any(grepl("mínimos cuadrados", res$avisos)))
})

test_that("las filas de los avisos son las del archivo original", {
  f <- tempfile(fileext = ".csv")
  d <- PlantGrowth
  d$weight[3] <- NA
  utils::write.csv(d, f, row.names = FALSE)
  res <- inkagro(f, "weight", "group", diseno = "dca")
  expect_match(res$avisos, "\\(fila 4\\)", all = FALSE)   # fila 1 = encabezado
  res_df <- inkagro(d, "weight", "group", diseno = "dca")
  expect_match(res_df$avisos, "\\(fila 3\\)", all = FALSE)
})

test_that("el grafico de interaccion en escala de grises no da avisos", {
  res <- inkagro(npk, "yield", "N", bloque = "block", factor_b = "P", diseno = "factorial")
  expect_no_warning(ggplot2::ggplot_build(plot(res, tipo = "interaccion")))
})
