require(reshape2)
# require(ggplot2) Not yet… In a further version maybe

#' Reads header only
#' @keywords internal
.readTOA5Header <- function(filename)
{
  return(scan(file=filename, nlines=4, what=character(), sep="\r"))
}

#' Read CS data, not header
#' @keywords internal
.readTOA5Data <- function(filename)
{
  # second line of header contains variable names
  header <- scan(file=filename,skip=1,nlines=1,what=character(),sep=",")
  # bring in data
  station.data <- read.table(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep=",", stringsAsFactors = FALSE)
  names(station.data) <- header
  # add column of R-formatted date/timestamps
  station.data$TIMESTAMP <- as.POSIXct(strptime(station.data$TIMESTAMP,"%Y-%m-%d %H:%M:%S"))
  return(station.data)
}

#' imports a datafile into an R structure (type of structure depends on \code{RetOpt})
#'
#' \code{importCSdata} reads TOA5 text file and transposes contents into an R structure
#'
#' @param filename the TOA5 file name with extension (generally .dat)
#' @param RetOpt Return option. As TOA5 has a header before data, you can choose to get the header (\code{RetOpt = "info"}, structure type = vector), the data (\code{RetOpt = "data"}, structure type = data.frame) or both (\code{RetOpt = "all"} (default), structure type = list).
#' @return An R Structure depending on \code{RetOpt} param.
#' @export
importTOA5data <- function(filename,RetOpt="all")
{
  if(RetOpt=="info")
  {
    # bring in entire header of CSI TOA5 data file for metadata
    return(.readTOA5Header(filename))
  }
  else if(RetOpt=="data")
  {
    return(.readTOA5Data(filename))
  }
  else # Default is "all"
  {
    fullHeader <- .readTOA5Header(filename)
    stationData <- .readTOA5Data(filename)
    return(list(infos=fullHeader, data=stationData))
  }
}

#' Pre-built function to melt imported data
#' 
#' @param data The data which needs to be melted. Warning: if your \code{RetOpt} in \code{importCSdata} was "all", specify which part of the list should be melted (eg \code{myvar$data})
#' @export
meltTOA5data <- function(data)
{
  return(melt(data, id.vars=c("RECORD", "TIMESTAMP"), variable.name="Param"))
}
