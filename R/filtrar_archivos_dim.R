#' Selecciona los archivos que contienen todas las dimensiones requeridas
#'
#' Utiliza la salida de \code{\link{detectar_claves_sii}} para decidir
#' qué archivos disponen de la información mínima necesaria.
#'
#' @param mapping `data.table` devuelto por \code{detectar_claves_sii()}.
#' @param dims `character`. Dimensiones obligatorias, por defecto
#' `c("geo", "act", "time", "metrics")`.
#'
#' @return Un vector `character` con las rutas (o nombres) de los
#' archivos que cumplen los requisitos.
#'
#' @examples
#' \dontrun{
#' hdrs <- detectar_claves_sii(list.files("data-raw/sii", "\\.txt$", TRUE))
#' buenos <- filtrar_archivos_dim(hdrs)
#' print(buenos)
#' }
#'
#' @export
#' @import data.table
filtrar_archivos_dim <- function(mapping, dims = c("geo", "act", "time", "metrics")) {
  library(data.table);  setDT(mapping)

  resumen <- mapping[, .(dims_presentes = list(unique(dimension))),
                     by = file]

  resumen[, cumple := vapply(dims_presentes,
                             function(x) all(dims %in% x),
                             logical(1))]

  ok  <- resumen[cumple == TRUE, file]
  bad <- resumen[cumple == FALSE, file]

  message("\n--- Resumen selección de archivos ---")
  if (length(ok))  message("✓ Con dimensiones requeridas: ",
                           toString(ok))
  if (length(bad)) message("» Incompletos: ",
                           toString(bad))
  message("--------------------------------------\n")

  ok
}
