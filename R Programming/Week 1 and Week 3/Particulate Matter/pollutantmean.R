pollutantmean <- function(directory, pollutant, id = 1:332) {
  directory = "./data/specdata"
  fileList = list.files(directory)
  fileNames = as.numeric(sub("\\.csv$","",fileList))
  selected.files = fileList[match(id,fileNames)]
  Data = lapply(file.path(directory,selected.files),read.csv)
  Data = do.call(rbind.data.frame,Data)
    mean(Data[,pollutant],na.rm=TRUE)

}