#' Procesa masivamente archivos TXT del SII
#'
#' Lee cada fichero, renombra columnas según un mapeo opcional `hdr_map`,
#' verifica que contenga las claves mínimas y actualiza el DW.
#'
#' @param files `character`. Rutas a los TXT válidos.
#' @param con   Conexión DBI.
#' @param hdr_map `data.table` opcional devuelto por
#'        \code{\link{detectar_claves_sii}}.  Si se suministra, las columnas
#'        se renombrarán dinámicamente.
#'
#' @return Invisiblemente `list(procesados, omitidos)`.
#' @export
#' @import data.table
procesar_archivos_sii <- function(files, con, hdr_map = NULL) {
  library(data.table)

  tiene_anio <- function(dt, fname) {
    ("Anio" %in% names(dt) && any(!is.na(dt$Anio))) ||
      ("AnioComercial" %in% names(dt) &&
         any(!is.na(dt$AnioComercial))) ||
      !is.na(stringr::str_extract(tolower(fname), "(20\\d{2})"))
  }

  cols_metric <- c("NumeroEmpresas", "MontoVentas",
                   "Trabajadores", "Remuneraciones")

  procesados <- character()
  omitidos   <- character()

  for (f in files) {
    fname <- basename(f)

    ## --- lectura con renombrado dinámico ----------------------------------
    dt <- if (is.null(hdr_map)) {
      leer_txt_sii(f)
    } else {
      leer_txt_sii(f, hdr_map = hdr_map)
    }

    ## Filtro de año --------------------------------------------------------
    if (!tiene_anio(dt, fname)) {
      msg <- sprintf("» Omitido %s — sin indicador de año.", fname)
      message(msg); omitidos <- c(omitidos, msg); next
    }

    ## Filtro de métricas mínimas ------------------------------------------
    if (!all(cols_metric %in% names(dt))) {
      falt <- cols_metric[!cols_metric %in% names(dt)]
      msg  <- sprintf("» Omitido %s — faltan métricas: %s.",
                      fname, paste(falt, collapse = ", "))
      message(msg); omitidos <- c(omitidos, msg); next
    }

    ## Carga DW -------------------------------------------------------------
    cargar_dimensiones(dt, con, file_name = fname)
    cargar_hechos_empresas(dt, con)

    message(sprintf("✓ Procesado %s — cargado al DW.", fname))
    procesados <- c(procesados, fname)
  }

  invisible(list(procesados = procesados, omitidos = omitidos))
}
