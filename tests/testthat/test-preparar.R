# Tablas desordenadas tipicas de tesis y su version ordenada esperada.

excel_crudo <- function(filas, archivo = tempfile(fileext = ".xlsx")) {
  # 'filas' es una lista de vectores: cada uno es una fila de la hoja, sin
  # interpretar (asi se simulan titulos, filas vacias y encabezados dobles).
  ancho <- max(lengths(filas))
  m <- t(vapply(filas, function(f) c(as.character(f), rep(NA_character_, ancho - length(f))), character(ancho)))
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  writexl::write_xlsx(df, archivo, col_names = FALSE)
  archivo
}

test_that("formato ancho con titulo, promedio, coma decimal y celdas combinadas", {
  skip_if_not_installed("writexl")
  f <- excel_crudo(list(
    c("Rendimiento de papa - Chachapoyas 2025"),
    c(NA),
    c("Variedad", "Dosis N (kg/ha)", "I", "II", "III"),
    c("INIA 303", "0",  "12,5", "13,1", "11,8"),
    c(NA,         "80", "18,2", "17,9", "19,0"),
    c("Canchan",  "0",  "10,4", "s/d",  "10,9"),
    c(NA,         "80", "15,1", "14,7", "15,6"),
    c("Promedio", NA,   "14,1", "15,2", "14,3"),
    c("Fuente: elaboraci\u00f3n propia")
  ))
  p <- preparar(f)
  d <- p$datos
  expect_named(d, c("variedad", "dosis_n", "bloque", "valor"))
  expect_equal(nrow(d), 12L)                       # 4 filas x 3 bloques
  expect_equal(unique(d$variedad), c("INIA 303", "Canchan"))
  expect_equal(unique(d$bloque), c("I", "II", "III"))
  expect_equal(d$valor[1:3], c(12.5, 13.1, 11.8))
  expect_true(is.na(d$valor[d$variedad == "Canchan" & d$dosis_n == 0 & d$bloque == "II"]))
  expect_equal(p$unidades[["dosis_n"]], "kg/ha")
  tipos <- p$cambios$tipo
  expect_true(all(c("T\u00edtulo", "Fila de resumen", "Nota al pie", "Celda combinada",
                    "Coma decimal", "Formato ancho a largo") %in% tipos))
  expect_true(any(p$revisar$valor == "s/d", na.rm = TRUE))
  expect_true(any(p$revisar$fila == 6L, na.rm = TRUE))   # fila de Excel de 's/d'
  expect_true(any(p$revisar$columna == "valor", na.rm = TRUE))
  expect_equal(sum(p$cambios$tipo == "Coma decimal"), 1L)
  p2 <- preparar(f, variable = "Rendimiento")
  expect_named(p2$datos, c("variedad", "dosis_n", "bloque", "rendimiento"))
  expect_false(any(p2$revisar$columna == "valor", na.rm = TRUE))
})

test_that("encabezado en dos filas con dos variables por bloque", {
  skip_if_not_installed("writexl")
  f <- excel_crudo(list(
    c("Tratamiento", "Altura (cm)", NA,   "Rendimiento (t/ha)", NA),
    c(NA,            "R1",         "R2",  "R1",                 "R2"),
    c("T1",          "45",         "47",  "3.2",                "3.4"),
    c("T2",          "52",         "50",  "4.1",                "3.9")
  ))
  p <- preparar(f)
  d <- p$datos
  expect_named(d, c("tratamiento", "bloque", "altura", "rendimiento"))
  expect_equal(nrow(d), 4L)
  expect_equal(d$altura, c(45, 47, 52, 50))
  expect_equal(d$rendimiento, c(3.2, 3.4, 4.1, 3.9))
  expect_equal(p$unidades[["rendimiento"]], "t/ha")
  expect_equal(nrow(p$revisar), 0L)
})

test_that("tabla larga ya ordenada no se toca y unifica etiquetas", {
  d0 <- data.frame(Bloque = c(1, 1, 2, 2), Trat = c("T1", "t1 ", "T2", "T2"),
                   Peso = c(10.2, 11.0, 9.8, 10.5), check.names = FALSE)
  d0$Trat[2] <- "T 1"
  p <- preparar(d0)
  expect_named(p$datos, c("bloque", "trat", "peso"))
  expect_equal(p$datos$trat, c("T1", "T1", "T2", "T2"))
  expect_equal(p$datos$peso, d0$Peso)
  expect_false(any(p$cambios$tipo == "Formato ancho a largo"))
})

test_that("marca valores sospechosos sin cambiarlos", {
  d0 <- data.frame(trat = rep(c("A", "B"), each = 4),
                   rend = c(4.1, 4.3, 41, 4.0, 3.8, 3.9, 4.2, 4.0))
  p <- preparar(d0)
  expect_equal(p$datos$rend[3], 41)
  expect_true(any(grepl("alejado", p$revisar$motivo)))
})

test_that("csv con punto y coma, coma decimal y latin1", {
  f <- tempfile(fileext = ".csv")
  con <- file(f, open = "w", encoding = "latin1")
  writeLines(c("Ensayo de ma\u00edz", "Bloque;Variedad;Rendimiento",
               "I;Marginal 28;5,4", "I;Amarillo;4,9", "II;Marginal 28;5,8",
               "II;Amarillo;5,1", "Total;;21,2"), con)
  close(con)
  p <- preparar(f)
  expect_named(p$datos, c("bloque", "variedad", "rendimiento"))
  expect_equal(p$datos$rendimiento, c(5.4, 4.9, 5.8, 5.1))
  expect_equal(nrow(p$datos), 4L)
})

test_that("unidad pegada al numero y coma de miles ambigua van a revisar", {
  d0 <- data.frame(trat = c("A", "A", "B", "B"),
                   peso = c("12.5 kg", "13 kg", "1,250", "11"))
  p <- preparar(d0)
  expect_equal(p$datos$peso, c(12.5, 13, 1.25, 11))
  expect_true(any(p$cambios$tipo == "Unidad pegada al n\u00famero"))
  expect_true(any(grepl("miles", p$revisar$motivo)))
})

test_that("exportar escribe Excel de tres hojas y CSV, e inkagro acepta el objeto", {
  skip_if_not_installed("writexl")
  crudo <- data.frame(Tratamiento = c("T1", "T2", "T3", "Total"),
                      I = c("4,5", "3,2", "5,0", "12,7"), II = c("4,8", "3,0", "5,3", "13,1"),
                      III = c("4,6", "3,3", "5,1", "13,0"), check.names = FALSE)
  p <- preparar(crudo)
  ruta <- file.path(tempdir(), "prueba_ordenada.xlsx")
  rutas <- suppressMessages(exportar(p, ruta))
  expect_true(all(file.exists(rutas)))
  expect_equal(readxl::excel_sheets(ruta), c("datos", "cambios", "revisar"))
  csv <- utils::read.csv(rutas[2], fileEncoding = "UTF-8-BOM")
  expect_equal(names(csv), names(p$datos))
  expect_equal(as.data.frame(p), p$datos)
})

test_that("inkagro() analiza directamente una tabla preparada", {
  skip_if_not_installed("agricolae")
  crudo <- data.frame(Tratamiento = c("T1", "T2", "T3", "Promedio"),
                      I = c("4,5", "3,2", "5,0", "4,2"), II = c("4,8", "3,0", "5,3", "4,4"),
                      III = c("4,6", "3,3", "5,1", "4,3"), check.names = FALSE)
  p <- preparar(crudo, variable = "rendimiento")
  res <- inkagro(p, respuesta = "rendimiento", tratamiento = "tratamiento",
                 bloque = "bloque", diseno = "dbca")
  ref <- stats::anova(stats::lm(rendimiento ~ bloque + tratamiento, data = p$datos))
  expect_equal(res$n, 9L)
  expect_equal(res$anova$F[res$anova$termino == "A"], ref[["F value"]][2])
})

test_that("inkagro() reporta la fila del Excel, no la de la tabla ordenada", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("agricolae")
  f <- excel_crudo(list(
    c("Ensayo de papa"),
    c("Variedad", "Dosis", "I", "II", "III"),
    c("A", "0",  "12,5", "13,1", "11,8"),
    c(NA,  "80", "18,2", "17,9", "19,0"),
    c("B", "0",  "10,4", "s/d",  "10,9"),
    c(NA,  "80", "15,1", "14,7", "15,6")
  ))
  p <- preparar(f, variable = "rendimiento")
  res <- inkagro(p, "rendimiento", "variedad", factor_b = "dosis", bloque = "bloque",
                 diseno = "factorial")
  expect_match(res$avisos, "\\(fila 5\\)", all = FALSE)
})
