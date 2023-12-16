#' @title LongLat
#' @description
#' This function converts location coordinate degrees to decimal minutes
#' @param Longitude input (in degrees) vector to be converted to decimal minutes
#' @param Latitude input (in degrees) vector to be converted to decimal minutes
#' @export LongLat



LongLat <- function(longitude,latitude){
  
  degree <- as.numeric(sapply(strsplit(longitude, "°"), "[[", 1))
  
  minute <- sapply(strsplit(longitude, "°"), "[[", 2)
  
  minute <- as.numeric(gsub("'W", "", minute))/60
  
  Longitude <- -degree - minute
  
  
  
  degree <- as.numeric(sapply(strsplit(latitude, "°"), "[[", 1))
  
  minute <- sapply(strsplit(latitude, "°"), "[[", 2)
  
  minute <- as.numeric(gsub("'N", "", minute))/60
  
  Latitude <- degree + minute
  
  LongLat_out <- data.frame(Longitude,Latitude)
  colnames(LongLat_out) <- c("Longitude", "Latitude")
  return(LongLat_out)
}

