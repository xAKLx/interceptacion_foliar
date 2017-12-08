source('~/Development/interceptacion_foliar/parser.r')
setClass("data", slots=list(plantones="list"))


#' Crea o actualiza un objeto con la informacion suministrada.
#' 
#' Todos los parametros a excepcion de data son frames representativas de los 
#' datos en los cuales se encuentran la información de dichos parametros. Estos
#' se consiguen usando las funciones de filtraje.
#' 
#' @param plantones información de los plantones.
#' @param puntosCaptaciones información de los puntos de captaciones
#' @param sesion información de la sesion
#' @param eventos información de los eventos
#' @param data debe ser un objecto creado anteriormente por esta función
#' o por importarDesdeArchivos. de ser proveido, se tomara este objeto como base y 
#' actualizara sus datos con la informacion proveida en los demas parametros.
#' 
#' @return Un objeto con solo la data proveida en los demas parametros si
#' el parametro data no es proveido. de otro modo, un objeto con la
#' informacion de data y con la informacion proveida en los demas parametros.
#' @export
importar <- function(plantones, puntosCaptaciones, sesion, eventos, data
                     , sobreescribir) {
  newData <- new("data");
  
  if(!missing(plantones)) {
    newData@plantones <- plantones
  } 
  
  if(!missing(puntosCaptaciones)) {
    newData@puntosCaptaciones <- puntosCaptaciones
  } 
  
  if(!missing(sesion)) {
    newData@sesion <- sesion
  } 
  
  if(!missing(eventos)) {
    newData@eventos <- eventos
  } 
  
  
  if(missing(data)) {
    newData
  } else {
    combinarData(destino = data, origin = newData, sobreescribir);
  }
}

#' Crea o actualiza un objeto con la informacion suministrada.
#' 
#' Todos los parametros a excepcion de data son strings representativas de los 
#' archivos en los cuales se encuentran la información de dichos parametros.
#' 
#' @param plantones información de los plantones.
#' @param puntosCaptaciones información de los puntos de captaciones
#' @param sesion información de la sesion
#' @param eventos información de los eventos
#' @param data debe ser un objecto creado anteriormente por esta función.
#' de ser proveido, se tomara este objeto como base y actualizara sus datos
#' con la informacion proveida en los demas parametros.
#' 
#' @return Un objeto con solo la data proveida en los demas parametros si
#' el parametro data no es proveido. de otro modo, un objeto con la
#' informacion de data y con la informacion proveida en los demas parametros.
#' @export
importarDesdeArchivos <- function(plantones, puntosCaptaciones, sesion, eventos
                                  , data, sobreescribir) {
  newData <- new("data");
  
  if(!missing(plantones)) {
    newData@plantones <- read.csv(file = plantones, header = TRUE, sep = ",", stringsAsFactors = FALSE);
    
    
    for(column in colnames(newData@plantones)){
      newData@plantones[,column] <- PlantonesParser(column, newData@plantones[,column])
    }
  } 
  
  if(!missing(puntosCaptaciones)) {
    newData@puntosCaptaciones <- read.csv(file = puntosCaptaciones, header = TRUE
                                          , sep = ",", stringsAsFactors = FALSE);
    
    
    for(column in colnames(newData@puntosCaptaciones)){
      newData@puntosCaptaciones[,column] <- PuntosCaptacionesParser(
        column, newData@puntosCaptaciones[,column])
    }
  } 
  
  if(!missing(sesion)) {
    newData@sesion <- read.csv(file = sesion, header = TRUE
                               , sep = ",", stringsAsFactors = FALSE);
    
    
    for(column in colnames(newData@sesion)){
      newData@sesion[,column] <- SesionesParser(
        column, newData@sesion[,column])
    }
  } 
  
  if(!missing(eventos)) {
    newData@eventos <- read.csv(file = eventos, header = TRUE
                                , sep = ",", stringsAsFactors = FALSE);
    
    
    for(column in colnames(newData@eventos)){
      newData@eventos[,column] <- EventosParser(
        column, newData@eventos[,column])
    }
  } 
  
  
  if(missing(data)) {
    newData
  } else {
    combinarData(destino = data, origin = newData, sobreescribir);
  }
}

combinarData <- function(destino, origen, sobreescribir= TRUE) {
  for(i in seq(from = 1, to = nrow(origen@plantones))) {
    for(j in seq(from = 1, to = nrow(destino))) {
      #if(destino[1,'']) {}
    }
  }
}