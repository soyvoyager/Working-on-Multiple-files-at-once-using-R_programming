
complete <- function(directory, id= 1:332) {
    file.list <- list.files(directory, full.names = T)

    ## Replace for loop with:
    counts <- sapply(id, function(i) {
        dat <- read.csv(file.list[i])
        sum(complete.cases(dat))
    })
    data.frame(id=id, rowCounts = counts)
}