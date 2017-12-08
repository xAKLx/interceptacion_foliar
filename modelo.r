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
      descargando <<- h > hm;
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

getSolucion <- function(eventos, modelo){
  first <<- TRUE
  i <<- 1
  f <- function(t, y, parms){
    with(as.list(c(y, parms)),{

      if(first) {
        first <<- FALSE
        list(c(1,1))
      } else {
        result <- modelo(eventos[i, 'lamina'])
        i <<- i + 1
        list(c(result[2] - h, result[1] - Qd))
      }
    })
    
    
  }
  
  yini = c(h= 0, Qd= 0)
  
  sol <- ode(y = yini, times = c(0, eventos[, 'fecha']), func = f, parms = NULL , method = "euler")
  colnames(sol) <- list('time', 'h(t)', 'Qd(t)')
  
  return(sol)
}
