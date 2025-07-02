#' Función para obtener metadatos de cada base de datos
#'
#' @param dbname nombre de la base de datos
#' @param con conexión vía DBI a una base de datos
get_metadata <- function(dbname, con) {

  query <- glue("
    SELECT
      '{dbname}' AS database_name,
      s.schema_name,
      t.table_name,
      c.column_name,
      c.data_type,
      c.collation_name,
      'No disponible' AS fecha_creacion,
      COALESCE(ROUND(pc.reltuples * (1 - ps.null_frac)), 0) AS nonnull_count,
      r.rolname AS table_owner
    FROM information_schema.columns c
    INNER JOIN information_schema.tables t ON c.table_schema = t.table_schema AND c.table_name = t.table_name
    INNER JOIN information_schema.schemata s ON t.table_schema = s.schema_name
    LEFT JOIN pg_stats ps ON ps.schemaname = c.table_schema AND ps.tablename = c.table_name AND ps.attname = c.column_name
    INNER JOIN pg_class pc ON pc.relname = t.table_name
    INNER JOIN pg_namespace pn ON pn.oid = pc.relnamespace AND pn.nspname = s.schema_name
    INNER JOIN pg_roles r ON r.oid = pc.relowner
    WHERE s.schema_name NOT IN ('pg_catalog', 'information_schema', 'pg_toast')
      AND s.schema_name NOT LIKE 'pg_temp_%'
    ORDER BY s.schema_name, t.table_name, c.ordinal_position;
  ")

  res <- tryCatch(
    dbGetQuery(con, query),
    error = function(e) {
      message(glue("Error en base '{dbname}': {e$message}"))
      return(NULL)
    }
  )

  dbDisconnect(con)
  return(res)
}
