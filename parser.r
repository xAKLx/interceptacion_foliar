

plantonesParser <- function(columna, valor) {
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
