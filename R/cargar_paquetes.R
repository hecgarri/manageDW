#' Carga paquetes necesarios para el análisis
#'
#' @param packages carga paquetes que de no estar disponibles son instalados
#' @export
cargar_paquetes <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}
