# helper functions to facilitate uploading the data to Google Sheets


saveData_gs <- function(dat, student_number, last_name) {
    humanTime <- format(Sys.time(), "%Y-%m-%d %H-%M")
    filename <- paste0(last_name, "_", student_number, "_", humanTime, ".csv")

    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp))

    write.csv(dat, file = tmp, row.names = F)
    gs_upload(tmp, sheet_title = filename, verbose = T)
}

# a function to count how many decimal places a number has.
#decimalplaces <- function(x) {
#    ifelse (abs(x - round(x)) > .Machine$double.eps^0.5, nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]]), 0)
#    }
decimalplaces <- function(x) {
    temp <- strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)
    return(sapply(temp, function(y){ifelse(length(y) == 2, nchar(y[2]), 0)}))
}
#decimalplaces(x)
