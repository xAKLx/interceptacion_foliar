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

#' Crea el modelo
#' 
#' @param hd  Lámina máxima posible de acumular
#' @param hm  Lámina mínima (lámina muerta)
#' @param Aint   Área efectiva de interceptación
#' @param Aef Área efectiva de descarga
#' 
#' @return funcion representativa del modelo
builder <- function(hd, hm, Aint, Aef) {
  #'  Lámina acumulada
  h <<- 0;
  
  descargando <<- FALSE;
  g <- 9.8
  
  #' Modelo de interceptación foliar
  #' 
  #' @param Qp Intensidad de la precipitación
  #' @return c(Intensidad de la descarga,  Lámina acumulada)
  function(Qp) {
    h <<- h + Qp;
    
    if(!descargando) {
      descargando <- h >= hd;
      Qd <- 0;
    } 
    
    if(descargando){
      Qd <- Aef * sqrt(2 * g * h)
      
      if((h - Qd) < hm)
        Qd <- h - hm
      
      h <<- h - Qd;
      descargando <- h > hm;
    }
    
    c(Qd, h)
  }
}

graphModel <- function(hd, hm, Aint, Aef, xData) {
  modelo <- builder(hd, hm, Aint, Aef);
  
  expresion <- function(Qp) {
    x <- sapply(Qp, modelo)[1,];
    print(Qp)
    x
  }
  #plot.function(expresion, xData, ylab = "salida", main = "klk");
  x <- sapply(xData, modelo)[2,];
  print(x);
  plot(x)
}

