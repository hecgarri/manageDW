#' Descarga EMPRESAS.zip desde el SII y lo descomprime (idempotente)
#' @param dir Carpeta destino; por defecto "data-raw/sii"
#' @return Vector de rutas a los TXT disponibles
#' @export
descargar_empresas_sii <- function(dir = here::here("data-raw/sii")) {
  library(fs);  library(curl)

  fs::dir_create(dir)
  existentes <- fs::dir_ls(dir, glob = "*.txt")

  if (length(existentes) > 0L) {
    message(sprintf(
      "Se omitió la descarga: %d archivos ya presentes en '%s'.",
      length(existentes), dir
    ))
    return(existentes)
  }

  zip_file <- fs::path(dir, "EMPRESAS.zip")
  curl::curl_download(
    url  = "https://www.sii.cl/sobre_el_sii/empresas/EMPRESAS.zip",
    dest = zip_file, mode = "wb"
  )
  utils::unzip(zip_file, exdir = dir)
  fs::dir_ls(dir, recurse = TRUE, glob = "*.txt")
}
