#' Crea un respaldo de la data suministrada
#'
#' @param data data a respaldar
#' @param respaldador funcion que recibe los argumentos plantones
#' , puntosCaptaciones, sesion, eventos, y debe crear el respaldo.
crearRespaldo <- function(data, respaldador) {
  respaldador(plantones = data@plantones
              , puntosCaptaciones = data@puntosCaptaciones, sesion = data@sesion
              , eventos = data@eventos);
}

#' Crea una funcion que respalda data dado el nombre de un archivo
#' 
#' @param fileName nombre del archivo destino
funcRespaldadorArchivo <- function(fileName) {
  function(plantones, puntosCaptaciones, sesion, eventos) {
    data <- new("data");
    
    data@plantones <- plantones
    data@puntosCaptaciones <- puntosCaptaciones
    data@sesion <- sesion
    data@eventos <- eventos
    
    destFile <- file(fileName, open = "w");
    serialize(data, destFile);
    close(destFile);
    
  }
}


#' Carga data desde un archivo
#' 
#' @param filename nombre del archivo donde esta la data guardada
funcCargador <- function(fileName) {
  destFile <- file(fileName, open = "r");
  data = unserialize(destFile);
  close(destFile);
  
  importar(plantones = data@plantones
           , puntosCaptaciones = data@puntosCaptaciones, sesion = data@sesion
           , eventos = data@eventos);
}
