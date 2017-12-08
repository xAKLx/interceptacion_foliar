

PlantonesParser <- function(columna, valor) {
  switch(
    columna,
    "longitud" =  as.double(valor),
    "latitud" = as.double(valor),
    "fechaPlantacion" = as.Date(valor),
    "esBorde" = as.logical(valor),
    "radioEfectivo" = as.double(valor),
    valor
  )
}

PuntosCaptacionesParser <- function(columna, valor) {
  switch(
    columna,
    "longitud" =  as.double(valor),
    "latitud" = as.double(valor),
    "altura" = as.double(valor),
    "sobreCamellon" = as.logical(valor),
    valor
  )
}

SesionesParser <- function(columna, valor) {
  switch(
    columna,
    "velViento" =  as.double(valor),
    "temperatura" = as.double(valor),
    "humedad" = as.double(valor),
    "fechaInicio" = as.POSIXlt(valor),
    "fechaFin" = as.POSIXlt(valor),
    valor
  )
}

EventosParser <- function(columna, valor) {
  switch(
    columna,
    "fecha" = as.POSIXlt(as.numeric(Sys.time()), origin =valor),
    "lamina" = as.double(valor),
    valor
  )
}
