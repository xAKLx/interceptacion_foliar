setClass("data", slots=list(plantones="list"))


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
importar <- function(plantones, puntosCaptaciones, sesion, eventos, data) {
  data <- new("data");
  data@plantones <- read.csv(file = plantones, header = TRUE, sep = ",", stringsAsFactors = FALSE);
  
  
  for(column in colnames(data@plantones)){
    data@plantones[,column] <- plantonesParser(column, data@plantones[,column])
  }
  
  data
}

#' Filtra un conjunto de datos por una regla y guarda el resultado en un archivo.
#'
#' @param data objeto con al información
#' @param tipoDeData conjunto de datos que se quiere filtrar, los posibles 
#' valores son los nombres de los parametros de la funcion importar excluyendo 
#' data.
#' @param regla una funcion que sera aplicada a cada fila del conjunto de datos
#' y que debe retornar un logical que defina si la fila sera incluida en el
#' archivo destino. Esta funcion recibira una lista con la fila a evaluar.
#' @param destino nombre del archivo destino.
#' @export
filtrarData <- function(data, tipoDeData, regla, destino) {
  conjunto <- attr(data, tipoDeData);
  
  if(nrow(conjunto) > 0) {
    name  <- conjunto[0,];
    for(index in seq(1:nrow(conjunto))){
      if(regla(conjunto[index, ]))
        name[nrow(name)+1,] <- conjunto[index,]; 
    }
    
    name
  }
  
}
  
#' Evalúa si la columna de fila suministrada es igual al valor
#' 
#' @param fila lista la cual contiene la fila a evaluar con las columnas
#' nombradas
#' @param columna nombre de la columna a filtrar
#' @param valor 
#' @return TRUE si el valor de la columna en la fila es igual al valor 
#' suministrado, FALSE si esto no es cierto.
#' @export
reglaIgualdad <- function(columna, valor) {
  function(fila) {
    fila[1,columna] == valor
  }
}

#' Evalúa si la columna de fila suministrada tiene un valor entre el rango
#' suministrado.
#' 
#' @param fila lista la cual contiene la fila a evaluar con las columnas
#' nombradas
#' @param columna nombre de la columna a filtrar
#' @param valorMinimo 
#' @param valorMaximo
#' @return TRUE si el valor de la columna en la fila se encuentra entre el
#' rango de valorMinimo-valorMaximo, FALSE si esto no es cierto.
#' @export
reglaRango <- function(columna, valorMinimo, valorMaximo) {
  function(fila) {
    fila[1,columna] >= valorMinimo && fila[1,columna] <= valorMaximo;
  }
}

crearRespaldo <- function(data, respaldador) {
  respaldador(plantones = data@plantones);
}

cargarRespaldo <- function(plantones) {
  data <- new("data");
  data@plantones <- plantones;
  data;
}

funcRespaldador <- function(plantones) {
  data <- new("data");
  data@plantones <- plantones;
  
  destFile <- file("test2.txt", open = "w");
  serialize(data, destFile);
  close(destFile);
  
}

funcCargador <- function(fileName) {
  destFile <- file(fileName, open = "r");
  data = unserialize(destFile);
  close(destFile);
  
  cargarRespaldo(plantones = data@plantones);
}





