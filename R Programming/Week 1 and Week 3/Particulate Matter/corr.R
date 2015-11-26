corr <- function(directory, threshold = 0) {
 
  directory <- "./data/specdata/"
  
  files <- list.files( path = directory )
  
  crr <- c()
  
  for(f in 1:length(files)){
    data <- read.csv( paste(directory, "/", files[f], sep="") )
    data <- data[complete.cases(data),]
    if ( nrow(data) > threshold ) {
      crr <- c(crr, cor(data$sulfate, data$nitrate) ) # append corralations
    }
  }
  
  return( crr )
  
}