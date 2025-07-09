#' Establece una conexión a PostgreSQL usando credenciales en .env
#'
#' @param db_name Nombre de la base de datos a conectar (opcional)
#' @param env_path Ruta al archivo .env (por defecto usa here::here())
#' @return Objeto de conexión DBI
#' @export
establecer_conexion <- function(
    db_name = Sys.getenv("PGDATABASE"),
    env_path = NULL
) {
  library(DBI)
  library(RPostgres)
  library(dotenv)
  library(here)

  # Cargar archivo .env desde la ruta especificada
  if (!is.null(env_path)) {
    dotenv::load_dot_env(env_path)
  } else {
    dotenv::load_dot_env(here::here(".env"))
  }

    con <- dbConnect(
    RPostgres::Postgres(),
    dbname   = db_name,
    host     = Sys.getenv("DB_HOST"),
    port     = as.integer(Sys.getenv("DB_PORT")),
    user     = Sys.getenv("DB_USER_DEFAULT"),
    password = Sys.getenv("DB_PASSWORD")
  )

  return(con)
}
