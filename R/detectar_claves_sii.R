#' Detecta y estandariza columnas clave en archivos TXT del SII
#'
#' Analiza únicamente la primera línea de cada archivo (cabecera) y
#' reconoce, mediante expresiones regulares, columnas que corresponden
#' a claves de las dimensiones **geográfica**, **actividad**,
#' **tramo/ventas**, **tiempo** y **métricas**.
#'
#' @param files `character`. Rutas completas a los archivos `.txt`.
#' @param encoding `character(1)`. Codificación del archivo (por defecto `"UTF-8"`).
#'
#' @return Un `data.table` con las columnas:
#' \describe{
#'   \item{file}{Nombre del archivo (sin ruta).}
#'   \item{original}{Nombre tal cual aparece en la cabecera.}
#'   \item{estandar}{Nombre estandarizado propuesto (p.\ ej.\ `CodRegion`).}
#'   \item{dimension}{Dimensión a la que pertenece: `geo`, `act`, `time`,
#'                    `tramo`, `metrics`.}
#' }
#'
#' @examples
#' \dontrun{
#' hdrs <- detectar_claves_sii(list.files("data-raw/sii", "\\.txt$", TRUE))
#' data.table::dcast(hdrs, file ~ dimension, value.var = "estandar",
#'                   fun.aggregate = toString)
#' }
#'
#' @export
#' @import data.table
#' @importFrom stringr str_detect str_extract
detectar_claves_sii <- function(files, encoding = "UTF-8") {
  library(data.table)

  # --- 1. Diccionario regex -> nombre estándar + dimensión -----------------
  regex_map <- list(
    # Geografía -------------------------------------------------------------
    CodRegion    = list(pattern = "(?i)^(cod.*region|codigo[ _]?region|region)$",
                        dim = "geo"),
    CodProvincia = list(pattern = "(?i)^(cod.*prov|codigo[ _]?prov.*|provincia)$",
                        dim = "geo"),
    CodComuna    = list(pattern = "(?i)^(cod.*comuna|codigo[ _]?comuna|comuna)$",
                        dim = "geo"),

    # Actividad -------------------------------------------------------------
    CodRubro     = list(pattern = "(?i)^(cod.*rubro|codigo[ _]?rubro)$",
                        dim = "act"),
    Rubro        = list(pattern = "(?i)^(rubro[ _]?economico)$",
                        dim = "act"),
    Actividad    = list(pattern = "(?i)^(actividad[ _]?economica)$",
                        dim = "act"),

    # Tramo / ventas --------------------------------------------------------
    TramoVentas  = list(pattern = "(?i)^(tramo.*ventas|tramo)$",
                        dim = "tramo"),

    # Tiempo ---------------------------------------------------------------
    Anio         = list(pattern = "(?i)^(anio|ano|year)$",
                        dim = "time"),
    AnioComercial= list(pattern = "(?i)^(aniocomercial|ano[ _]?comercial)$",
                        dim = "time"),

    # Métricas -------------------------------------------------------------
    NumeroEmpresas = list(pattern = "(?i)^(numero.*empresas)$",
                          dim = "metrics"),
    MontoVentas    = list(pattern = "(?i)^(monto.*ventas|ventas.*uf|monto_ventas)$",
                          dim = "metrics"),
    Trabajadores   = list(pattern = "(?i)^(numero.*trabajadores|trabajadores)$",
                          dim = "metrics"),
    Remuneraciones = list(pattern = "(?i)^(renta.*uf|remuneraciones)$",
                          dim = "metrics")
  )

  patterns <- sapply(regex_map, `[[`, "pattern")
  dims     <- sapply(regex_map, `[[`, "dim")
  std_names<- names(regex_map)

  out <- rbindlist(lapply(files, function(f) {
    # lectura solo de cabecera ---------------------------------------------
    hdr <- fread(f, nrows = 0L, encoding = encoding, sep = "\t",
                 header = TRUE, showProgress = FALSE)
    cols <- names(hdr)

    # correspondencia ------------------------------------------------------
    m <- lapply(seq_along(patterns), function(i) {
      which(stringr::str_detect(cols, patterns[i]))
    })
    idx <- unlist(m, use.names = FALSE)
    if (!length(idx)) return(NULL)

    data.table(
      file      = basename(f),
      original  = cols[idx],
      estandar  = std_names[rep(seq_along(patterns), lengths(m))],
      dimension = dims[rep(seq_along(patterns), lengths(m))]
    )
  }), fill = TRUE)

  out[]
}
