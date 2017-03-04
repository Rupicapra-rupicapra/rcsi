require(reshape2)

#' Read header
#' @keywords internal
.readTOAC1Header <- function(filename)
{
  return(scan(file = filenames, nlines = 2, what = character(), sep = "\r"))
}

#' Read TOACI data
#' @keywords internal
.readTOAC1Data <- function(filename, time)
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
    station.data$TMSTAMP <- as.POSIXct(strptime(station.data$TMSTAMP, format = timeformat))
  }
  else
  {
    station.data$TMSTAMP <- as.POSIXlt(strptime(station.data$TMSTAMP, format = timeformat))
  }
}

#' imports a datafile into an R structure (type of structure depends on \code{RetOpt})
#'
#' \code{importTOACIdata} reads TOACI text file and transposes contents into an R structure
#'
#' @param filename the TOACI file name with extension (generally .dat)
#' @param RetOpt Return option. As TOACI has a header before data, you can choose to get the header (\code{RetOpt = "info"}, structure type = vector), the data (\code{RetOpt = "data"}(default), structure type = data.frame) or both (\code{RetOpt = "all"} , structure type = list).
#' @param time Time structure for storing TMSTAMP. May be \code{POSIXct} or \code{POSIXlt}. POSIXlt is a better solution for milisecond precision, but causes weird behavior with \code{reshape2::melt} function.
#' @return An R Structure depending on \code{RetOpt} param.
#' @export
importTOAC1data <- function(filename, RetOpt = "data", time = "POSIXlt")
{
  if(RetOpt == "info")
  {
    return(.readTOAC1Header(filename))
  }
  else if(RetOpt == "all")
  {
    fullHeader <- .readTOAC1Header(filename)
    stationData <- .readTOAC1Data(filename, time)
    return(list(infos = fullHeader, data = stationData))
  }
  else
  {
    return(.readTOAC1Data(filename, time))
  }
}
