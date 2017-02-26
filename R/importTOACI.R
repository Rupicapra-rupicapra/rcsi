require(reshape2)

#' Read header
#' @keywords internal
.readTOACIHeader <- function(filename)
{
  return(scan(file = filenames, nlines = 2, what = character(), sep = "\r"))
}

#' Read TOACI data
#' @keywords internal
.readTOACIData <- function(filename, time)
{
  header <- scan(file = filename,
                 nlines = 1,
                 skip = 1,
                 what = character(),
                 sep = ",")
  station.data <- read.table(file = filename,
                             skip = 4,
                             header = FALSE,
                             na.string = c("NAN"),
                             sep = ",",
                             stringsAsFactors = FALSE
                             )
  names(station.data) <- header
  
  timeformat <- "%Y-%m-%d %H:%M:%S"
  
  if(time == "POSIXct")
  {
    station.data$TIMESTAMP <- as.POSIXct(strptime(station.data$TIMESTAMP, format = timeformat))
  }
  else
  {
    station.data$TIMESTAMP <- as.POSIXlt(strptime(station.data$TIMESTAMP, format = timeformat))
  }
}

#' imports a datafile into an R structure (type of structure depends on \code{RetOpt})
#'
#' \code{importTOACIdata} reads TOACI text file and transposes contents into an R structure
#'
#' @param filename the TOACI file name with extension (generally .dat)
#' @param RetOpt Return option. As TOACI has a header before data, you can choose to get the header (\code{RetOpt = "info"}, structure type = vector), the data (\code{RetOpt = "data"}(default), structure type = data.frame) or both (\code{RetOpt = "all"} , structure type = list).
#' @param time Time structure for storing TIMESTAMP. May be \code{POSIXct} or \code{POSIXlt}. POSIXlt is a better solution for milisecond precision, but causes weird behavior with \code{reshape2::melt} function.
#' @return An R Structure depending on \code{RetOpt} param.
#' @export
importTOACIdata <- function(filename, RetOpt = "data", time = "POSIXlt")
{
  if(RetOpt == "info")
  {
    return(.readTOACIHeader(filename))
  }
  else if(RetOpt == "all")
  {
    fullHeader <- .readTOACIHeader(filename)
    stationData <- .readTOACIData(filename, time)
    return(list(infos = fullHeader, data = stationData))
  }
  else
  {
    return(.readTOACIData(filename, time))
  }
}