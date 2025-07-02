
#' Crear base de datos si no existe (usando .env)
#'
#' @param nombre_bd Nombre de la base de datos a crear
crear_base_si_no_existe <- function(nombre_bd, env_path = NULL) {
  library(DBI)
  library(RPostgres)
  library(dotenv)
  library(here)
  library(glue)

  # Cargar variables de entorno
  dotenv::load_dot_env(here(env_path))

  # Conectarse a la base 'postgres' como administrador
  con_admin <- dbConnect(
    RPostgres::Postgres(),
    dbname   = "postgres",
    host     = Sys.getenv("DB_HOST"),
    port     = as.integer(Sys.getenv("DB_PORT")),
    user     = Sys.getenv("DB_USER_DEFAULT"),
    password = Sys.getenv("DB_PASSWORD")
  )


  # Revisar bases existentes
  bases_existentes <- dbGetQuery(con_admin, "
    SELECT datname
    FROM pg_database
    WHERE datistemplate = false
      AND datname NOT IN ('postgres')
      AND pg_catalog.pg_get_userbyid(datdba) = current_user;
  ")

  # Crear la base si no existe
  if (!(nombre_bd %in% bases_existentes$datname)) {
    dbExecute(con_admin, glue("CREATE DATABASE {nombre_bd};"))
    message(glue("Base de datos '{nombre_bd}' creada."))
  } else {
    message(glue("️ La base de datos '{nombre_bd}' ya existe."))
  }

  dbDisconnect(con_admin)
}
