#' Make an xts time series
#'
#'Function to make an xts time series from vectors of dates and values removing all dates with NA observations.
#'
#' @param  dates A vector of dates, these must be formatted as dates
#' @param  values A vector of values associated with the dates, this vector must be the same length as the date vector
#' @param  na.val na value for missing data in the timeseries. Defaults is NA.
#' @param  na.omit Option for whether NA values shoudl be omitted from the final time series or not. Default is True.
#'
#' @return An xts time series of monthly values column1 is the date, column 2 is the monthly aggregated value

#'
#' @examples
#' make.xts(dates=datevec, values=atributevec,  na.val=-999, na.omit=T)
#'
#' @export
#'
make.xts=function(dates, values, fun, na.val=NA, na.omit=T, na.fill=F){

  if(length(dates)!=length(values)){stop("Dates and Values must have the same length")}

  #if a na value other than NA is used then replace all of these with NA's
  if(is.na(na.val)==F){
    print(paste("Replacing", na.val, "with NAs"))
    ilist=which(values==na.val)
    values[ilist]=NA
  }

  TimeSeries=xts::xts(values, as.Date(dates))

  #if na.omit=T cut out any NA values from the time series
  if(na.omit==T){
    TimeSeries=na.omit(TimeSeries)
  }

  # if na.fill=T fill any na values with linear interpolation
  if(na.fill==T){
    TimeSeries = na.approx(TimeSeries)
  }

return(TimeSeries)

}#end function
