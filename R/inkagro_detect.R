inkagro_detect <- function(datos, verbose = TRUE){
  cols <- tolower (names(datos))
  # Diccionarios para los diferentes tipos de Diseño
  pal_bloque <- c("bloque", "bloco", "block", "rep",
                  "repeticion", "replica", "bloque_id","blk", "block_id",
                  "id_block", "blockno",
                  "block_num", "block_number", "bloq",
                  "bloqueo", "bloc", "bl", "b1",
                  "replication", "repetition", "reps",
                  "rep_no", "repnum", "replicate",
                  "replicate_id", "rep_id", "repn",
                  "rpt", "rpto", "repeat",
                  "complete_block", "incomplete_block",
                  "iblock", "main_block", "superblock",
                  "subblock", "sblock", "alpha_block",
                  "rowblock", "colblock")

  pal_trat <- c("tratamiento", "trat", "tr", "t1",
                "tratamiento_id", "variedad", "var",
                "genotipo", "gen","treatment", "treat", "treatments",
                "trt", "trmt", "trtmnt", "tt",
                "treat_id", "treatment_id",
                "entry", "entry_id", "entry_no",
                "entryname", "geno", "gid",
                "genotype_id", "genotype",
                "cultivar", "cv", "line",
                "inbred", "hybrid", "cross",
                "material", "accession",
                "acc", "accno", "clone",
                "strain", "selection",
                "population", "family",
                "pedigree", "variety_id",
                "cultigen", "check",
                "control", "test_entry",
                "sample", "sample_id","method")

  pal_factor <- c("factor_a", "factora", "fa",
                  "factor_b", "factorb", "fb",
                  "factor1", "factor2","factor", "factora", "factorb",
                  "factorc", "factord",
                  "f1", "f2", "f3", "f4",
                  "mainfactor", "secondaryfactor",
                  "primary_factor", "secondary_factor",
                  "tillage", "dose", "rate",
                  "nitrogen", "phosphorus",
                  "potassium", "fertilizer",
                  "irrigation", "water",
                  "density", "spacing",
                  "temperature", "time",
                  "environment", "env",
                  "location", "site",
                  "year", "season")

  pal_parcela <- c("parcela_principal", "parcela",
                   "pp", "subparcela", "sub", "sp","plot", "plot_id",
                   "plotno", "plot_no", "plotnum",
                   "experimental_unit",
                   "eu", "unit", "unit_id",
                   "mainplot", "main_plot",
                   "subplot", "sub_plot",
                   "splitplot", "split_plot",
                   "wholeplot", "whole_plot",
                   "subsubplot", "ssp",
                   "parcel", "parcela_id",
                   "fieldplot", "trialplot",
                   "bed", "ridge",
                   "pot", "container",
                   "microplot", "macroplot")

  pal_fila    <- c("fila", "row", "linha",  "renglon", "hilera", "surco",
                   "furrow", "rowid", "row_id",
                   "rowno", "row_no", "rno", "linha_id",
                   "grid_row", "field_row",
                   "plot_row", "y1")
  pal_columna <- c("columna", "column", "col", "coluna","colid", "col_id", "colno",
                   "col_no", "cno",
                   "grid_col", "field_col",
                   "plot_col", "x1",
                   "position_x", "position_y",
                   "columnid", "column_no")
  pal_ambiente <- c("env", "environment", "site", "location", "loc", "trial_site",
              "station", "farm",
                       "campo", "field",
                      "nursery")
  pal_year <- c(
    "year", "yr", "anio",
    "ano", "season",
    "cycle", "campaign",
    "crop_year" )
  pal_respuesta <- c(
    "yield", "response",
    "trait", "variable",
    "measurement", "value",
    "obs", "observation",
    "phenotype", "pheno",
    "score", "rating"
  )
  pal_alpha <- c("iblock", "incomplete_block", "subblock", "sblock",
                 "block_in_rep", "rep_block", "alpha_block",
                 "blk_in_rep", "block_nested", "intrablock")

  pal_strip <- c("strip_row", "strip_col", "row_trt", "col_trt",
                 "horizontal_factor", "vertical_factor",
                 "factor_row", "factor_col", "row_factor",
                 "column_factor", "cross_strip")

  pal_tiempo <- c("time", "tempo", "fecha", "date", "dias", "days",
                  "sampling", "evaluation", "week", "month",
                  "dap", "das", "timepoint", "visit",
                  "occasion", "period", "cycle")

  # Verificar presencia de cada palabra en la base de datos

  tiene_bloque  <- any(cols %in% pal_bloque)
  tiene_trat    <- any(cols %in% pal_trat)
  tiene_factor  <- sum(cols %in% pal_factor) >= 2
  tiene_parcela <- any(cols %in% pal_parcela)
  tiene_fila    <- any(cols %in% pal_fila)
  tiene_columna <- any(cols %in% pal_columna)
  tiene_ambiente  <- any(cols %in% pal_ambiente)
  tiene_year<- any(cols %in% pal_year)
  tiene_respuesta<- any(cols %in% pal_respuesta)
  tiene_alpha    <- any(cols %in% pal_alpha)
  tiene_strip    <- any(cols %in% pal_strip)
  tiene_tiempo   <- any(cols %in% pal_tiempo)

  # Detectar diseño en orden de especificidad
  if (tiene_strip) {
    diseno <- "Strip-plot"
  } else if (tiene_tiempo & tiene_parcela) {
    diseno <- "Repeated Measures"
  } else if (tiene_trat & tiene_ambiente & tiene_bloque) {
    diseno <- "Multiambiental (MET)"
  } else if (tiene_alpha & tiene_bloque) {
    diseno <- "Alfa-latice"
  } else if (tiene_parcela & tiene_bloque) {
    diseno <- "Parcelas Divididas"
  } else if (tiene_fila & tiene_columna & tiene_trat) {
    diseno <- "Cuadrado Latino"
  } else if (tiene_factor & tiene_ambiente) {
    diseno <- "Factorial con Ambientes"
  } else if (tiene_factor) {
    diseno <- "Factorial"
  } else if (tiene_trat & tiene_bloque) {
    diseno <- "DBCA"
  } else if (tiene_trat & !tiene_bloque) {
    diseno <- "DCA"
  } else {
    diseno <- "No detectado - revise nombres de columnas"
  }

  if (verbose) message("Diseño detectado: ", diseno)
  return(diseno)
}

