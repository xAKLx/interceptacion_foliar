#' Filtra un conjunto de datos por una regla y guarda el resultado en un archivo.
#'
#' @param data objeto con la información
#' @param tipoDeData conjunto de datos que se quiere filtrar, los posibles 
#' valores son los nombres de los parametros de la funcion importar excluyendo 
#' data.
#' @param regla una funcion que sera aplicada a cada fila del conjunto de datos
#' y que debe retornar un logical que defina si la fila sera incluida en el
#' archivo destino. Esta funcion recibira una lista con la fila a evaluar.
#' @param filtrador una funcion que sera aplicada a cada fila cullo resultado
#' de la aplicacion de la funcion regla sea verdadera. De ser proveeida, esta 
#' funcion recibira una fila y retornara la fila modificada.
#' @param destino nombre del archivo destino. De ser proveeido, el resultado
#' de esta funcion se guardara en el archivo especificado.
#' @return frame del conjunto de datos especificado despues de realizar el
#' filtraje.
#' @export
filtrarData <- function(data, tipoDeData, regla, filtrador, destino) {
  conjunto <- attr(data, tipoDeData);
  
  if(nrow(conjunto) > 0) {
    name  <- conjunto[0,];
    for(index in seq(1:nrow(conjunto))){
      if(regla(conjunto[index, ])) {
        if(!missing(filtrador)){
          name[nrow(name)+1,] <- filtrador(conjunto[index,]); 
        } else {
          name[nrow(name)+1,] <- conjunto[index,]; 
        }
      }
    }
    
    name
  }
  
}

filtradorSustituir <- function(columna, nuevoValor) {
  function(fila) {
    fila[1,columna] = nuevoValor
    fila
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



