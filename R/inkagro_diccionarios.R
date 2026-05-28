# Diccionarios compartidos de InkAgro
# Usados por inkagro_detect() e inkagro_analyze()

pal_bloque <- c("bloque", "bloco", "block", "rep",
                "repeticion", "replica", "bloque_id", "blk", "block_id",
                "id_block", "blockno", "block_num", "block_number", "bloq",
                "bloqueo", "bloc", "bl", "b1", "replication", "repetition",
                "reps", "rep_no", "repnum", "replicate", "replicate_id",
                "rep_id", "repn", "rpt", "complete_block",
                "main_block", "superblock", "rowblock", "colblock")

pal_trat <- c("tratamiento", "trat", "tr", "t1", "tratamiento_id",
              "variedad", "var", "genotipo", "gen", "treatment", "treat",
              "treatments", "trt", "trmt", "trtmnt", "treat_id",
              "treatment_id", "entry", "entry_id", "entry_no", "entryname",
              "geno", "gid", "genotype_id", "genotype", "cultivar", "cv",
              "inbred", "hybrid", "cross", "material", "accession", "acc",
              "accno", "clone", "strain", "selection", "population",
              "family", "pedigree", "variety_id", "cultigen", "check",
              "control", "test_entry", "sample", "sample_id", "method")

pal_factor <- c("factor_a", "factora", "fa", "factor_b", "factorb", "fb",
                "factor1", "factor2", "factor", "factorc", "factord",
                "f1", "f2", "f3", "f4", "mainfactor", "secondaryfactor",
                "primary_factor", "secondary_factor", "tillage", "dose",
                "rate", "nitrogen", "phosphorus", "potassium", "fertilizer",
                "irrigation", "density", "spacing", "temperature","season","dosis",
                "dosis_n", "dosis_n_kg_ha")

pal_parcela <- c("parcela_principal", "parcela", "pp", "subparcela",
                 "plot", "plot_id", "plotno", "plot_no", "plotnum",
                 "mainplot", "main_plot", "subplot", "sub_plot",
                 "splitplot", "split_plot", "wholeplot", "whole_plot",
                 "subsubplot", "ssp", "parcel", "parcela_id",
                 "fieldplot", "trialplot", "microplot", "macroplot")

pal_fila <- c("fila", "row", "linha", "renglon", "hilera", "surco",
              "rowid", "row_id", "rowno", "row_no", "grid_row",
              "field_row", "plot_row", "y1")

pal_columna <- c("columna", "column", "col", "coluna", "colid", "col_id",
                 "colno", "col_no", "grid_col", "field_col",
                 "plot_col", "x1", "columnid", "column_no")

pal_ambiente <- c("env", "environment", "site", "location", "loc",
                  "trial_site", "station", "farm", "campo", "field",
                  "nursery", "localidad", "sitio", "lugar")

pal_year <- c("year", "yr", "anio", "ano", "season", "cycle",
              "campaign", "crop_year")

pal_alpha <- c("iblock", "incomplete_block", "subblock", "sblock",
               "block_in_rep", "rep_block", "alpha_block",
               "blk_in_rep", "block_nested", "intrablock")

pal_strip <- c("strip_row", "strip_col", "row_trt", "col_trt",
               "horizontal_factor", "vertical_factor",
               "factor_row", "factor_col", "row_factor",
               "column_factor", "cross_strip")

pal_tiempo <- c("time", "tempo", "dias", "days", "sampling",
                "evaluation", "week", "month", "dap", "das",
                "timepoint", "visit", "occasion", "period")

cols_excluir <- c("id", "fecha", "date", "parcela", "plot_id",
                  "observacion", "observation", "notes", "nota",
                  "comentario", "comment", "tecnico", "technician",
                  "notas", "remarks", "operador", "operator",
                  "responsable", "encargado", "evaluador",
                  "nombre", "name", "codigo", "code",
                  "locality", "lugar", "municipio", "estado",
                  "temporada", "season_name", "campaign_name")

