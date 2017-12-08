
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
