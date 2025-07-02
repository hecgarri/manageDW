#' Lee un TXT/TAB del SII y renombra columnas según un mapeo
#'
#' @param file    Ruta al archivo .txt
#' @param hdr_map Data.table opcional con columnas
#'                \code{file}, \code{original}, \code{estandar}
#'                devuelto por \code{\link{detectar_claves_sii}}.
#'                Si se suministra, renombra \code{original} → \code{estandar}.
#' @return        \code{data.table} con nombres homogéneos y metadatos añadidos.
#' @export
#' @import data.table
#' @importFrom stringr str_extract
leer_txt_sii <- function(file, hdr_map = NULL) {
  library(data.table)
  library(stringr)

  # 1) Lectura
  dt <- fread(
    file,
    sep          = "\t",
    encoding     = "UTF-8",
    quote        = "'",
    dec          = ",",
    fill         = TRUE,
    header       = TRUE,
    showProgress = FALSE
  )
  setDT(dt)

  # 2) Renombrado dinámico si hdr_map provisto
  if (!is.null(hdr_map)) {
    ren <- hdr_map[file == basename(file)]
    if (nrow(ren)) {
      for (i in seq_len(nrow(ren))) {
        orig <- ren$original[i]
        std  <- ren$estandar[i]
        if (orig %in% names(dt)) {
          setnames(dt, orig, std, skip_absent = TRUE)
        }
      }
    }
  }

  # 3) Metadatos
  nm <- tolower(basename(file))
  # Asegurar columna Anio
  if (!"Anio" %in% names(dt)) {
    if ("AnioComercial" %in% names(dt) && any(!is.na(dt$AnioComercial))) {
      set(dt, j = "Anio", value = as.integer(dt$AnioComercial))
    } else {
      anio_arch <- as.integer(str_extract(nm, "(20\\d{2})"))
      if (!is.na(anio_arch)) set(dt, j = "Anio", value = anio_arch)
    }
  }
  # NivelGeo y TramoVentas (idéntico a antes)
  set(dt, j = "NivelGeo",
      value = fifelse(str_detect(nm, "comuna"),    "COMUNA",
                      fifelse(str_detect(nm, "provincia"), "PROVINCIA",
                              fifelse(str_detect(nm, "region"),    "REGION", NA_character_))))
  set(dt, j = "TramoVentas",
      value = fifelse(str_detect(nm, "5tr"),  "5",
                      fifelse(str_detect(nm, "13tr"), "13",
                              fifelse(str_detect(nm, "18tr"), "18", NA_character_))))

  return(dt)
}
