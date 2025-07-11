#' Crea y documenta la estructura del Data Warehouse
#'
#' Imprime si cada tabla fue creada o ya existia.
#'
#' @param con    Conexion DBI (RPostgres) con permisos DDL
#' @param schema Esquema destino; por defecto "public"
#' @export
crear_esquema_empresas <- function(con, schema = "public") {
  library(DBI)

  asegurar_tabla <- function(nombre, ddl) {
    tbl_id  <- DBI::Id(schema = schema, table = nombre)
    existed <- dbExistsTable(con, tbl_id)

    # inmediato = TRUE ? se envia como comando sin preparar (permite multiples ordenes)
    dbExecute(con, ddl, immediate = TRUE)

    msg <- if (existed) "ya existia" else "creada"
    message(sprintf(". Tabla %-10s %s en esquema <%s>.", nombre, msg, schema))
  }

  asegurar_tabla("dim_geo", sprintf("
    CREATE TABLE IF NOT EXISTS %s.dim_geo (
      IDGeo        SERIAL PRIMARY KEY,
      CodRegion    TEXT,
      CodProvincia TEXT,
      CodComuna    TEXT,
      NombreGeo    TEXT,
      NivelGeo     TEXT,
      UNIQUE (CodRegion, CodProvincia, CodComuna)
    );
    COMMENT ON TABLE  %s.dim_geo IS
      'Dimension geografica jerarquica (Region ? Provincia ? Comuna).';
    COMMENT ON COLUMN %s.dim_geo.NivelGeo IS
      'Nivel de agregado: REGION | PROVINCIA | COMUNA';
  ", schema, schema, schema))

  asegurar_tabla("dim_act", sprintf("
    CREATE TABLE IF NOT EXISTS %s.dim_act (
      IDAct     TEXT PRIMARY KEY,
      Rubro     TEXT,
      SubRubro  TEXT,
      Actividad TEXT
    );
    COMMENT ON TABLE %s.dim_act IS
      'Dimension de actividad economica (Rubro, Sub-rubro, Actividad).';
  ", schema, schema))

  asegurar_tabla("dim_tramo", sprintf("
    CREATE TABLE IF NOT EXISTS %s.dim_tramo (
      IDTramo     TEXT PRIMARY KEY,
      Descripcion TEXT
    );
    COMMENT ON TABLE %s.dim_tramo IS
      'Dimension de tramo de ventas / tamano de empresa (SII).';
  ", schema, schema))

  asegurar_tabla("dim_tiempo", sprintf("
    CREATE TABLE IF NOT EXISTS %s.dim_tiempo (
      Anio INT PRIMARY KEY
    );
    COMMENT ON TABLE %s.dim_tiempo IS
      'Dimension temporal anual.';
  ", schema, schema))

  asegurar_tabla("f_empresas", sprintf("
    CREATE TABLE IF NOT EXISTS %s.f_empresas (
      IDGeo          INT,
      IDAct          TEXT,
      IDTramo        TEXT,
      Anio           INT,
      NumEmpresas    INT,
      MontoVentas    NUMERIC,
      Trabajadores   INT,
      Remuneraciones NUMERIC,
      PRIMARY KEY (IDGeo, IDAct, IDTramo, Anio)
    );
    COMMENT ON TABLE %s.f_empresas IS
      'Tabla de hechos: metricas anuales por geografia, actividad y tramo.';
    COMMENT ON COLUMN %s.f_empresas.NumEmpresas     IS 'Numero de empresas.';
    COMMENT ON COLUMN %s.f_empresas.MontoVentas     IS 'Ventas anuales (CLP).';
    COMMENT ON COLUMN %s.f_empresas.Trabajadores    IS 'Trabajadores declarados.';
    COMMENT ON COLUMN %s.f_empresas.Remuneraciones  IS 'Remuneraciones anuales (CLP).';
  ", schema, schema, schema, schema, schema, schema))
}
