#' Inserta/actualiza dim_geo, dim_tramo y dim_tiempo
#'
#' La funcion detecta en tiempo de ejecucion si existen las
#' columnas minimas para cada dimension.
#' . Si faltan, muestra `message()` y omite la carga.
#' . En dim_tiempo desecha filas con `NA` en `Anio`.
#'
#' @param dt        data.table resultante de leer_txt_sii()
#' @param con       Conexion DBI (PostgreSQL)
#' @param file_name Nombre del archivo (se usa solo para mensajes)
#' @export
cargar_dimensiones <- function(dt, con, file_name = NULL) {
  library(data.table); setDT(dt)

  archivo <- ifelse(is.null(file_name), "<dt>", basename(file_name))

  ## ?? 1. dim_geo ------------
  req_geo <- c("CodRegion", "CodComuna")
  if (all(req_geo %in% names(dt))) {

    # columnas opcionales
    if (!"CodProvincia" %in% names(dt)) dt[, CodProvincia := NA_character_]
    if (!"NivelGeo"     %in% names(dt)) dt[, NivelGeo     := "COMUNA"]

    geo <- unique(dt[, list(
      codregion    = CodRegion,
      codprovincia = CodProvincia,
      codcomuna    = CodComuna,
      nombregeo    = if ("NombreComuna" %in% names(dt))
        NombreComuna[!is.na(NombreComuna)][1L]
      else NA_character_,
      nivelgeo     = NivelGeo
    )])

    dbWithTransaction(con, {
      dbWriteTable(con, "tmp_geo", geo, overwrite = TRUE, temporary = TRUE)
      n <- dbExecute(con, "
        INSERT INTO dim_geo (CodRegion,CodProvincia,CodComuna,NombreGeo,NivelGeo)
        SELECT codregion,codprovincia,codcomuna,nombregeo,nivelgeo
        FROM tmp_geo
        ON CONFLICT (CodRegion,CodProvincia,CodComuna)
        DO UPDATE SET NombreGeo = EXCLUDED.NombreGeo,
                      NivelGeo  = EXCLUDED.NivelGeo;",
                     immediate = TRUE)
      message(sprintf(". dim_geo   : %d filas insertadas/actualizadas (%s).", n, archivo))
    })

  } else {
    message(sprintf("> dim_geo   omitida: faltan %s en %s.",
                    paste(req_geo[!req_geo %in% names(dt)], collapse = ", "),
                    archivo))
  }

  ## ?? 3. dim_tiempo ???????????????????????????????????????????????????????
  if ("Anio" %in% names(dt) && any(!is.na(dt$Anio))) {
    tiempo <- unique(dt[!is.na(Anio), list(anio = Anio)])

    dbWithTransaction(con, {
      dbWriteTable(con, "tmp_tiempo", tiempo, overwrite = TRUE, temporary = TRUE)
      n <- dbExecute(con, "
        INSERT INTO dim_tiempo (Anio)
        SELECT anio FROM tmp_tiempo
        ON CONFLICT (Anio) DO NOTHING;",
                     immediate = TRUE)
      message(sprintf(". dim_tiempo: %d anos nuevos agregados (%s).", n, archivo))
    })
  } else {
    message(sprintf("> dim_tiempo omitida: sin valores validos de Anio en %s.", archivo))
  }
}
