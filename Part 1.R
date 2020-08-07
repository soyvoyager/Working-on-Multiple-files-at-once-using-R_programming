pollutantmean <- function(directory, pollutant, id = 1:332) {
  file.names <- list.files(directory)
  file.numbers <- as.numeric(sub('\\.csv$','', file.names))
  selected.files <- na.omit(file.names[match(id, file.numbers)])
  selected.dfs <- lapply(file.path(directory,selected.files), read.csv)
  mean(c(sapply(selected.dfs, function(x) x[ ,pollutant])), na.rm=TRUE)
}

pollutantmean1 <- function(directory, pollutant, id = 1:332) {

  # Break this function up into a series of smaller functions
  # that do exactly what you expect them to. Your friends
  # will love you for it.

  csvFiles = getFilesById(id, directory)

  dataFrames = readMultipleCsvFiles(csvFiles)

  dataFrame = bindMultipleDataFrames(dataFrames)

  getColumnMean(dataFrame, column = pollutant)
}


getFilesById <- function(id, directory = getwd()) {
  allFiles = list.files(directory)
  file.path(directory, allFiles[id])
}

readMultipleCsvFiles <- function(csvFiles) {
  lapply(csvFiles, read.csv)
}

bindMultipleDataFrames <- function(dataFrames) {
  Reduce(function(x, y) rbind(x, y), dataFrames)
}

getColumnMean <- function(dataFrame, column, ignoreNA = TRUE) {
  mean(dataFrame[ , column], na.rm = ignoreNA)
}

pollutmean2<- function(dir, pollutant, id=1:332) {
  dir<- list.files(dir, full.names = T)     #list files
  dat<- data.frame()                        #make empty df
  for (i in id) {
    dat <- rbind(dat, read.csv(dir[i]))     #rbind all files
  }
  mean(dat[,pollutant], na.rm = TRUE)       #calculate mean of given column
}