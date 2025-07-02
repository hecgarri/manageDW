#' Lista las bases de datos accesibles (excluye sistema)
#'
#' @return Vector de nombres de bases de datos visibles para el usuario
#' @export
listar_bases_usuario <- function() {
  con <- establecer_conexion("postgres")

  bases <- DBI::dbGetQuery(con, "
    SELECT datname
    FROM pg_database
    WHERE datistemplate = false
      AND datname NOT IN ('postgres', 'template0', 'template1')
      AND has_database_privilege(datname, 'CONNECT') = true;
  ")

  DBI::dbDisconnect(con)
  return(bases$datname)
}
