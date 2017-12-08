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