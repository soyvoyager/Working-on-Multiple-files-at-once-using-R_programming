#Method 1
pollutantmean <- function(directory, pollutant, id = 1:332) {
  file.names <- list.files(directory)
  file.numbers <- as.numeric(sub('\\.csv$','', file.names))
  selected.files <- na.omit(file.names[match(id, file.numbers)])
  selected.dfs <- lapply(file.path(directory,selected.files), read.csv)
  mean(c(sapply(selected.dfs, function(x) x[ ,pollutant])), na.rm=TRUE)
}

#Method 2 (Best and easy method which I have Worked after hours of understanding....)
pollutmean2<- function(dir, pollutant, id=1:332) {
  dir<- list.files(dir, full.names = T)     #list files
  dat<- data.frame()                        #make empty df
  for (i in id) {
    dat <- rbind(dat, read.csv(dir[i]))     #rbind all files
  }
  mean(dat[,pollutant], na.rm = TRUE)       #calculate mean of given column
}

# Method 3 (suggest More methods if you can)
                
