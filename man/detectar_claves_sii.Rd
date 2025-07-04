% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detectar_claves_sii.R
\name{detectar_claves_sii}
\alias{detectar_claves_sii}
\title{Detecta y estandariza columnas clave en archivos TXT del SII}
\usage{
detectar_claves_sii(files, encoding = "UTF-8")
}
\arguments{
\item{files}{\code{character}. Rutas completas a los archivos \code{.txt}.}

\item{encoding}{\code{character(1)}. Codificación del archivo (por defecto \code{"UTF-8"}).}
}
\value{
Un \code{data.table} con las columnas:
\describe{
\item{file}{Nombre del archivo (sin ruta).}
\item{original}{Nombre tal cual aparece en la cabecera.}
\item{estandar}{Nombre estandarizado propuesto (p.\ ej.\ \code{CodRegion}).}
\item{dimension}{Dimensión a la que pertenece: \code{geo}, \code{act}, \code{time},
\code{tramo}, \code{metrics}.}
}
}
\description{
Analiza únicamente la primera línea de cada archivo (cabecera) y
reconoce, mediante expresiones regulares, columnas que corresponden
a claves de las dimensiones \strong{geográfica}, \strong{actividad},
\strong{tramo/ventas}, \strong{tiempo} y \strong{métricas}.
}
\examples{
\dontrun{
hdrs <- detectar_claves_sii(list.files("data-raw/sii", "\\\\.txt$", TRUE))
data.table::dcast(hdrs, file ~ dimension, value.var = "estandar",
                  fun.aggregate = toString)
}

}
