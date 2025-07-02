#' Verifica la presencia de un indicador de año en un `data.table` o en el nombre de archivo
#'
#' La función se utiliza como filtro previo en rutinas ETL para descartar
#' archivos que no permiten identificar el año de referencia.
#'
#' **Criterios de aceptación**
#' 1. El `data.table` contiene una columna llamada **`Anio`**.
#' 2. El `data.table` contiene una columna **`AnioComercial`** con al menos
#'    un valor no nulo.
#' 3. El nombre del archivo incluye un patrón `20xx`.
#'
#' @param dt `data.table` ya leído y con nombres originales de columnas.
#' @param file_name `character(1)`. Nombre (o ruta) del archivo de origen.
#'
#' @return `TRUE` si se encuentra algún indicador de año; `FALSE` en caso contrario.
#'
#' @examples
#' \dontrun{
#' dt <- data.table::data.table(AnioComercial = 2024, x = 1:10)
#' tiene_anio(dt, "PUB_ACT_2024.txt")   # devuelve TRUE
#'
#' dt2 <- data.table::data.table(x = 1:5)
#' tiene_anio(dt2, "PUB_TRAM_PROV.txt")  # devuelve FALSE
#' }
#'
#' @keywords internal
#' @export
#' @import data.table
#' @importFrom stringr str_extract
tiene_anio <- function(dt, file_name) {
  if ("Anio" %in% names(dt) && any(!is.na(dt$Anio)))                 return(TRUE)
  if ("AnioComercial" %in% names(dt) && any(!is.na(dt$AnioComercial))) return(TRUE)
  !is.na(stringr::str_extract(tolower(file_name), "(20\\d{2})"))
}
