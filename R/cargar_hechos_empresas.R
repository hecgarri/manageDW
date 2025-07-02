#' Inserta o actualiza la tabla de hechos f_empresas (vectorizado)
#'
#' @param dt  data.table normalizado con columnas
#'            CodRegion, CodProvincia, CodComuna, TramoVentas,
#'            CodRubro, Anio, NumeroEmpresas, MontoVentas,
#'            Trabajadores, Remuneraciones
#' @param con Conexión DBI (PostgreSQL)
#' @export
cargar_hechos_empresas <- function(dt, con) {
  library(data.table); setDT(dt)                         # garantiza data.table

  ## ───── 1. Enlace de claves surrogate ─────────────────────────────────────
  geo_map <- dbGetQuery(con,
                        "SELECT idgeo, codregion, codprovincia, codcomuna FROM dim_geo;"
  )
  setDT(geo_map)

  # join in-place para añadir IDGeo
  dt <- geo_map[dt,
                on = .(codregion = CodRegion,
                       codprovincia = CodProvincia,
                       codcomuna = CodComuna)]

  dt[, IDTramo := TramoVentas]

  ## ───── 2. Selección y renombre de columnas ───────────────────────────────
  hechos <- dt[, .(
    idgeo,
    idact          = CodRubro,
    idtramo        = IDTramo,
    anio           = Anio,
    numempresas    = NumeroEmpresas,
    montoventas    = MontoVentas,
    trabajadores   = Trabajadores,
    remuneraciones = Remuneraciones
  )]

  ## ───── 3. Upsert masivo en una sola transacción ──────────────────────────
  dbWithTransaction(con, {
    # 3.1 copiar a tabla temporal
    dbWriteTable(con,
                 name      = Id(schema = "pg_temp", table = "tmp_f_empresas"),
                 value     = hechos,
                 temporary = TRUE,
                 overwrite = TRUE)
    # 3.2 upsert
    n <- dbExecute(con, "
      INSERT INTO f_empresas AS f
      SELECT * FROM pg_temp.tmp_f_empresas
      ON CONFLICT (idgeo, idact, idtramo, anio)
      DO UPDATE SET
        numempresas    = EXCLUDED.numempresas,
        montoventas    = EXCLUDED.montoventas,
        trabajadores   = EXCLUDED.trabajadores,
        remuneraciones = EXCLUDED.remuneraciones;
    ", immediate = TRUE)
    message(sprintf("• f_empresas: %d filas insertadas/actualizadas.", n))
  })
}
